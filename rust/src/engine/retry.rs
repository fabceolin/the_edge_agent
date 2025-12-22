//! Retry execution with exponential backoff
//!
//! Provides configurable retry logic with:
//! - Exponential backoff with jitter
//! - Configurable retry limits
//! - Error type filtering

use rand::Rng;
use serde_json::Value as JsonValue;
use std::time::Duration;

use crate::engine::graph::RetryConfig;
use crate::error::{TeaError, TeaResult};

/// Retry executor
pub struct RetryExecutor {
    /// Random number generator seed for jitter (for testing)
    deterministic: bool,
}

impl RetryExecutor {
    /// Create a new retry executor
    pub fn new() -> Self {
        Self {
            deterministic: false,
        }
    }

    /// Create a deterministic executor (for testing)
    pub fn deterministic() -> Self {
        Self {
            deterministic: true,
        }
    }

    /// Execute a function with retry logic
    pub fn execute_with_retry<F>(&self, f: F, config: &RetryConfig) -> TeaResult<JsonValue>
    where
        F: Fn() -> TeaResult<JsonValue>,
    {
        let mut last_error = None;
        let mut delay = Duration::from_millis(config.base_delay_ms);

        for attempt in 0..=config.max_retries {
            match f() {
                Ok(result) => return Ok(result),
                Err(e) => {
                    last_error = Some(e);

                    if attempt < config.max_retries {
                        // Apply jitter if enabled
                        let actual_delay = if config.jitter && !self.deterministic {
                            self.add_jitter(delay)
                        } else {
                            delay
                        };

                        std::thread::sleep(actual_delay);

                        // Calculate next delay with exponential backoff
                        let next_ms = (delay.as_millis() as f64 * config.backoff_multiplier) as u64;
                        delay = Duration::from_millis(next_ms.min(config.max_delay_ms));
                    }
                }
            }
        }

        // All retries exhausted
        Err(last_error.unwrap_or_else(|| TeaError::Execution {
            node: "unknown".to_string(),
            message: "Retry failed with unknown error".to_string(),
        }))
    }

    /// Execute with retry, tracking attempt count
    pub fn execute_with_retry_tracked<F>(
        &self,
        f: F,
        config: &RetryConfig,
    ) -> (TeaResult<JsonValue>, u32)
    where
        F: Fn() -> TeaResult<JsonValue>,
    {
        let mut last_error = None;
        let mut delay = Duration::from_millis(config.base_delay_ms);
        let mut attempts = 0;

        for attempt in 0..=config.max_retries {
            attempts = attempt;
            match f() {
                Ok(result) => return (Ok(result), attempt),
                Err(e) => {
                    last_error = Some(e);

                    if attempt < config.max_retries {
                        let actual_delay = if config.jitter && !self.deterministic {
                            self.add_jitter(delay)
                        } else {
                            delay
                        };

                        std::thread::sleep(actual_delay);

                        let next_ms = (delay.as_millis() as f64 * config.backoff_multiplier) as u64;
                        delay = Duration::from_millis(next_ms.min(config.max_delay_ms));
                    }
                }
            }
        }

        let err = last_error.unwrap_or_else(|| TeaError::Execution {
            node: "unknown".to_string(),
            message: "Retry failed with unknown error".to_string(),
        });

        (Err(err), attempts)
    }

    /// Add jitter to delay (±20%)
    fn add_jitter(&self, delay: Duration) -> Duration {
        let mut rng = rand::thread_rng();
        let jitter_factor: f64 = rng.gen_range(0.8..1.2);
        Duration::from_secs_f64(delay.as_secs_f64() * jitter_factor)
    }

    /// Calculate delay for a specific attempt
    pub fn calculate_delay(&self, attempt: u32, config: &RetryConfig) -> Duration {
        let base = config.base_delay_ms as f64;
        let multiplier = config.backoff_multiplier;
        let max = config.max_delay_ms as f64;

        let delay_ms = (base * multiplier.powi(attempt as i32)).min(max);
        Duration::from_millis(delay_ms as u64)
    }
}

impl Default for RetryExecutor {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for retry configuration
pub struct RetryConfigBuilder {
    config: RetryConfig,
}

impl RetryConfigBuilder {
    /// Start building a retry config
    pub fn new() -> Self {
        Self {
            config: RetryConfig::default(),
        }
    }

    /// Set max retries
    pub fn max_retries(mut self, n: u32) -> Self {
        self.config.max_retries = n;
        self
    }

    /// Set base delay
    pub fn base_delay(mut self, duration: Duration) -> Self {
        self.config.base_delay_ms = duration.as_millis() as u64;
        self
    }

    /// Set max delay
    pub fn max_delay(mut self, duration: Duration) -> Self {
        self.config.max_delay_ms = duration.as_millis() as u64;
        self
    }

    /// Set backoff multiplier
    pub fn backoff_multiplier(mut self, multiplier: f64) -> Self {
        self.config.backoff_multiplier = multiplier;
        self
    }

    /// Enable or disable jitter
    pub fn jitter(mut self, enabled: bool) -> Self {
        self.config.jitter = enabled;
        self
    }

    /// Build the config
    pub fn build(self) -> RetryConfig {
        self.config
    }
}

impl Default for RetryConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if an error should be retried
pub fn is_retryable(error: &TeaError) -> bool {
    matches!(
        error,
        TeaError::Http(_) | TeaError::Timeout(_) | TeaError::Io(_) | TeaError::Execution { .. }
    )
}

/// Check if an error is transient (temporary failure)
pub fn is_transient(error: &TeaError) -> bool {
    matches!(error, TeaError::Http(_) | TeaError::Timeout(_))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};
    use std::sync::Arc;

    #[test]
    fn test_immediate_success() {
        let executor = RetryExecutor::deterministic();
        let config = RetryConfig::default();

        let result =
            executor.execute_with_retry(|| Ok(serde_json::json!({"success": true})), &config);

        assert!(result.is_ok());
        assert_eq!(result.unwrap()["success"], true);
    }

    #[test]
    fn test_retry_then_success() {
        let executor = RetryExecutor::deterministic();
        let config = RetryConfigBuilder::new()
            .max_retries(3)
            .base_delay(Duration::from_millis(10))
            .jitter(false)
            .build();

        let attempts = Arc::new(AtomicU32::new(0));
        let attempts_clone = attempts.clone();

        let result = executor.execute_with_retry(
            move || {
                let n = attempts_clone.fetch_add(1, Ordering::SeqCst);
                if n < 2 {
                    Err(TeaError::Http("Transient error".to_string()))
                } else {
                    Ok(serde_json::json!({"attempt": n}))
                }
            },
            &config,
        );

        assert!(result.is_ok());
        assert_eq!(attempts.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn test_all_retries_fail() {
        let executor = RetryExecutor::deterministic();
        let config = RetryConfigBuilder::new()
            .max_retries(2)
            .base_delay(Duration::from_millis(1))
            .jitter(false)
            .build();

        let attempts = Arc::new(AtomicU32::new(0));
        let attempts_clone = attempts.clone();

        let result = executor.execute_with_retry(
            move || {
                attempts_clone.fetch_add(1, Ordering::SeqCst);
                Err(TeaError::Http("Always fails".to_string()))
            },
            &config,
        );

        assert!(result.is_err());
        // Initial + 2 retries = 3 attempts
        assert_eq!(attempts.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn test_tracked_retry() {
        let executor = RetryExecutor::deterministic();
        let config = RetryConfigBuilder::new()
            .max_retries(5)
            .base_delay(Duration::from_millis(1))
            .jitter(false)
            .build();

        let attempts = Arc::new(AtomicU32::new(0));
        let attempts_clone = attempts.clone();

        let (result, attempt_count) = executor.execute_with_retry_tracked(
            move || {
                let n = attempts_clone.fetch_add(1, Ordering::SeqCst);
                if n < 3 {
                    Err(TeaError::Timeout("retry".to_string()))
                } else {
                    Ok(serde_json::json!({}))
                }
            },
            &config,
        );

        assert!(result.is_ok());
        assert_eq!(attempt_count, 3); // 0-indexed, so 3 means 4th attempt succeeded
    }

    #[test]
    fn test_delay_calculation() {
        let executor = RetryExecutor::new();
        let config = RetryConfigBuilder::new()
            .base_delay(Duration::from_millis(100))
            .backoff_multiplier(2.0)
            .max_delay(Duration::from_secs(10))
            .build();

        let delay0 = executor.calculate_delay(0, &config);
        let delay1 = executor.calculate_delay(1, &config);
        let delay2 = executor.calculate_delay(2, &config);

        assert_eq!(delay0.as_millis(), 100);
        assert_eq!(delay1.as_millis(), 200);
        assert_eq!(delay2.as_millis(), 400);
    }

    #[test]
    fn test_delay_caps_at_max() {
        let executor = RetryExecutor::new();
        let config = RetryConfigBuilder::new()
            .base_delay(Duration::from_secs(1))
            .backoff_multiplier(10.0)
            .max_delay(Duration::from_secs(5))
            .build();

        let delay = executor.calculate_delay(10, &config);
        assert_eq!(delay.as_secs(), 5);
    }

    #[test]
    fn test_is_retryable() {
        assert!(is_retryable(&TeaError::Http("err".to_string())));
        assert!(is_retryable(&TeaError::Timeout("err".to_string())));
        assert!(is_retryable(&TeaError::Execution {
            node: "n".to_string(),
            message: "m".to_string(),
        }));
        assert!(!is_retryable(&TeaError::NodeNotFound("n".to_string())));
        assert!(!is_retryable(&TeaError::CycleDetected));
    }

    #[test]
    fn test_is_transient() {
        assert!(is_transient(&TeaError::Http("err".to_string())));
        assert!(is_transient(&TeaError::Timeout("err".to_string())));
        assert!(!is_transient(&TeaError::Lua("err".to_string())));
    }

    #[test]
    fn test_retry_config_builder() {
        let config = RetryConfigBuilder::new()
            .max_retries(10)
            .base_delay(Duration::from_secs(2))
            .max_delay(Duration::from_secs(60))
            .backoff_multiplier(3.0)
            .jitter(false)
            .build();

        assert_eq!(config.max_retries, 10);
        assert_eq!(config.base_delay_ms, 2000);
        assert_eq!(config.max_delay_ms, 60000);
        assert_eq!(config.backoff_multiplier, 3.0);
        assert!(!config.jitter);
    }
}
