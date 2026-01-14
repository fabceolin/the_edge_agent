//! Metric trait and built-in metrics for experiment evaluation.
//!
//! Metrics are used to score agent outputs against expected outputs.
//! Custom metrics can be implemented by implementing the `Metric` trait.

use serde_json::Value;
use std::fmt;

/// Result of evaluating a metric on an output.
///
/// Contains the numeric score, pass/fail status, and optional details.
#[derive(Debug, Clone)]
pub struct MetricResult {
    /// Numeric score, typically 0.0 to 1.0
    pub score: f64,

    /// Whether the result passes (score >= threshold)
    pub passed: bool,

    /// Optional details about the scoring
    pub details: Option<String>,
}

impl MetricResult {
    /// Create a new metric result.
    pub fn new(score: f64, passed: bool) -> Self {
        Self {
            score,
            passed,
            details: None,
        }
    }

    /// Create a metric result with details.
    pub fn with_details(score: f64, passed: bool, details: impl Into<String>) -> Self {
        Self {
            score,
            passed,
            details: Some(details.into()),
        }
    }

    /// Create a passing result with score 1.0.
    pub fn pass() -> Self {
        Self::new(1.0, true)
    }

    /// Create a failing result with score 0.0.
    pub fn fail() -> Self {
        Self::new(0.0, false)
    }

    /// Create a failing result with details.
    pub fn fail_with(details: impl Into<String>) -> Self {
        Self::with_details(0.0, false, details)
    }
}

impl fmt::Display for MetricResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let status = if self.passed { "PASS" } else { "FAIL" };
        write!(f, "{} (score: {:.2})", status, self.score)?;
        if let Some(ref details) = self.details {
            write!(f, " - {}", details)?;
        }
        Ok(())
    }
}

/// Trait for implementing custom metrics.
///
/// Metrics evaluate agent outputs against expected outputs and produce
/// a score indicating how well the output matches expectations.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::{Metric, MetricResult};
/// use serde_json::Value;
///
/// struct AlwaysPass;
///
/// impl Metric for AlwaysPass {
///     fn name(&self) -> &str {
///         "always_pass"
///     }
///
///     fn score(&self, _output: &Value, _expected: &Value) -> MetricResult {
///         MetricResult::pass()
///     }
/// }
/// ```
pub trait Metric: Send + Sync {
    /// Return the name of this metric.
    fn name(&self) -> &str;

    /// Score the output against the expected value.
    ///
    /// # Arguments
    ///
    /// * `output` - The actual output from the agent
    /// * `expected` - The expected output from the dataset
    ///
    /// # Returns
    ///
    /// A `MetricResult` containing the score, pass/fail status, and optional details.
    fn score(&self, output: &Value, expected: &Value) -> MetricResult;
}

/// Exact match metric.
///
/// Returns 1.0 if output equals expected exactly, else 0.0.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::{Metric, ExactMatch};
/// use serde_json::json;
///
/// let metric = ExactMatch;
/// let result = metric.score(&json!("hello"), &json!("hello"));
/// assert!(result.passed);
/// assert_eq!(result.score, 1.0);
/// ```
#[derive(Debug, Clone, Default)]
pub struct ExactMatch;

impl Metric for ExactMatch {
    fn name(&self) -> &str {
        "exact_match"
    }

    fn score(&self, output: &Value, expected: &Value) -> MetricResult {
        if output == expected {
            MetricResult::pass()
        } else {
            MetricResult::fail_with(format!(
                "Expected: {}, Got: {}",
                serde_json::to_string(expected).unwrap_or_default(),
                serde_json::to_string(output).unwrap_or_default()
            ))
        }
    }
}

/// Contains match metric.
///
/// Returns 1.0 if output contains expected as a substring.
/// Works on string values; other types are converted to strings.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::{Metric, ContainsMatch};
/// use serde_json::json;
///
/// let metric = ContainsMatch::new();
/// let result = metric.score(&json!("hello world"), &json!("world"));
/// assert!(result.passed);
/// ```
#[derive(Debug, Clone)]
pub struct ContainsMatch {
    /// Whether to perform case-sensitive matching
    pub case_sensitive: bool,
}

impl ContainsMatch {
    /// Create a new case-insensitive contains match metric.
    pub fn new() -> Self {
        Self {
            case_sensitive: false,
        }
    }

    /// Create a case-sensitive contains match metric.
    pub fn case_sensitive() -> Self {
        Self {
            case_sensitive: true,
        }
    }
}

impl Default for ContainsMatch {
    fn default() -> Self {
        Self::new()
    }
}

impl Metric for ContainsMatch {
    fn name(&self) -> &str {
        "contains_match"
    }

    fn score(&self, output: &Value, expected: &Value) -> MetricResult {
        let output_str = value_to_string(output);
        let expected_str = value_to_string(expected);

        let contains = if self.case_sensitive {
            output_str.contains(&expected_str)
        } else {
            output_str.to_lowercase().contains(&expected_str.to_lowercase())
        };

        if contains {
            MetricResult::pass()
        } else {
            MetricResult::fail_with(format!(
                "Output '{}' does not contain '{}'",
                truncate(&output_str, 50),
                truncate(&expected_str, 50)
            ))
        }
    }
}

/// Numeric tolerance metric.
///
/// Returns 1.0 if the numeric difference is within tolerance.
/// Works on numeric values; non-numeric values fail.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::{Metric, NumericTolerance};
/// use serde_json::json;
///
/// let metric = NumericTolerance::new(0.1);
/// let result = metric.score(&json!(3.05), &json!(3.0));
/// assert!(result.passed);
///
/// let result = metric.score(&json!(3.5), &json!(3.0));
/// assert!(!result.passed);
/// ```
#[derive(Debug, Clone)]
pub struct NumericTolerance {
    /// Maximum allowed difference between values
    pub tolerance: f64,
}

impl NumericTolerance {
    /// Create a new numeric tolerance metric.
    ///
    /// # Arguments
    ///
    /// * `tolerance` - Maximum allowed absolute difference
    pub fn new(tolerance: f64) -> Self {
        Self { tolerance }
    }
}

impl Default for NumericTolerance {
    fn default() -> Self {
        Self::new(0.001)
    }
}

impl Metric for NumericTolerance {
    fn name(&self) -> &str {
        "numeric_tolerance"
    }

    fn score(&self, output: &Value, expected: &Value) -> MetricResult {
        let output_num = match output {
            Value::Number(n) => n.as_f64(),
            _ => None,
        };

        let expected_num = match expected {
            Value::Number(n) => n.as_f64(),
            _ => None,
        };

        match (output_num, expected_num) {
            (Some(out), Some(exp)) => {
                let diff = (out - exp).abs();
                if diff <= self.tolerance {
                    MetricResult::pass()
                } else {
                    MetricResult::fail_with(format!(
                        "Difference {} exceeds tolerance {}",
                        diff, self.tolerance
                    ))
                }
            }
            (None, _) => MetricResult::fail_with(format!(
                "Output is not a number: {}",
                serde_json::to_string(output).unwrap_or_default()
            )),
            (_, None) => MetricResult::fail_with(format!(
                "Expected is not a number: {}",
                serde_json::to_string(expected).unwrap_or_default()
            )),
        }
    }
}

/// JSON path match metric.
///
/// Compares values at a specific JSON path in output and expected.
/// Uses JMESPath syntax for path expressions.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::experiments::{Metric, JsonPathMatch};
/// use serde_json::json;
///
/// let metric = JsonPathMatch::new("answer");
/// let result = metric.score(
///     &json!({"answer": "Paris", "extra": "data"}),
///     &json!({"answer": "Paris"})
/// );
/// assert!(result.passed);
/// ```
#[derive(Debug, Clone)]
pub struct JsonPathMatch {
    /// JMESPath expression to extract values
    pub path: String,
}

impl JsonPathMatch {
    /// Create a new JSON path match metric.
    ///
    /// # Arguments
    ///
    /// * `path` - JMESPath expression (e.g., "answer", "data.items[0].name")
    pub fn new(path: impl Into<String>) -> Self {
        Self { path: path.into() }
    }

    fn extract_value<'a>(&self, value: &'a Value) -> Option<&'a Value> {
        // Simple path extraction supporting dot notation and array indexing
        let mut current = value;
        for part in self.path.split('.') {
            // Check for array index (e.g., "items[0]")
            if let Some(bracket_pos) = part.find('[') {
                let field = &part[..bracket_pos];
                let index_str = &part[bracket_pos + 1..part.len() - 1];

                if !field.is_empty() {
                    current = current.get(field)?;
                }

                if let Ok(index) = index_str.parse::<usize>() {
                    current = current.get(index)?;
                } else {
                    return None;
                }
            } else {
                current = current.get(part)?;
            }
        }
        Some(current)
    }
}

impl Metric for JsonPathMatch {
    fn name(&self) -> &str {
        "json_path_match"
    }

    fn score(&self, output: &Value, expected: &Value) -> MetricResult {
        let output_value = self.extract_value(output);
        let expected_value = self.extract_value(expected);

        match (output_value, expected_value) {
            (Some(out), Some(exp)) => {
                if out == exp {
                    MetricResult::pass()
                } else {
                    MetricResult::fail_with(format!(
                        "Path '{}': expected {}, got {}",
                        self.path,
                        serde_json::to_string(exp).unwrap_or_default(),
                        serde_json::to_string(out).unwrap_or_default()
                    ))
                }
            }
            (None, Some(_)) => MetricResult::fail_with(format!(
                "Path '{}' not found in output",
                self.path
            )),
            (Some(_), None) => MetricResult::fail_with(format!(
                "Path '{}' not found in expected",
                self.path
            )),
            (None, None) => {
                // Both missing - this is a failure (path should exist for meaningful comparison)
                MetricResult::fail_with(format!(
                    "Path '{}' not found in either output or expected",
                    self.path
                ))
            }
        }
    }
}

// Helper functions

fn value_to_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        _ => serde_json::to_string(value).unwrap_or_default(),
    }
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_metric_result_display() {
        let pass = MetricResult::pass();
        assert!(pass.to_string().contains("PASS"));

        let fail = MetricResult::fail_with("test reason");
        assert!(fail.to_string().contains("FAIL"));
        assert!(fail.to_string().contains("test reason"));
    }

    #[test]
    fn test_exact_match_equal() {
        let metric = ExactMatch;
        let result = metric.score(&json!("hello"), &json!("hello"));
        assert!(result.passed);
        assert_eq!(result.score, 1.0);
    }

    #[test]
    fn test_exact_match_not_equal() {
        let metric = ExactMatch;
        let result = metric.score(&json!("hello"), &json!("world"));
        assert!(!result.passed);
        assert_eq!(result.score, 0.0);
    }

    #[test]
    fn test_exact_match_complex_json() {
        let metric = ExactMatch;
        let result = metric.score(
            &json!({"a": 1, "b": [1, 2, 3]}),
            &json!({"a": 1, "b": [1, 2, 3]}),
        );
        assert!(result.passed);
    }

    #[test]
    fn test_contains_match_substring() {
        let metric = ContainsMatch::new();
        let result = metric.score(&json!("hello world"), &json!("world"));
        assert!(result.passed);
    }

    #[test]
    fn test_contains_match_case_insensitive() {
        let metric = ContainsMatch::new();
        let result = metric.score(&json!("Hello World"), &json!("WORLD"));
        assert!(result.passed);
    }

    #[test]
    fn test_contains_match_case_sensitive() {
        let metric = ContainsMatch::case_sensitive();
        let result = metric.score(&json!("Hello World"), &json!("WORLD"));
        assert!(!result.passed);
    }

    #[test]
    fn test_contains_match_not_found() {
        let metric = ContainsMatch::new();
        let result = metric.score(&json!("hello"), &json!("world"));
        assert!(!result.passed);
    }

    #[test]
    fn test_numeric_tolerance_within() {
        let metric = NumericTolerance::new(0.1);
        let result = metric.score(&json!(3.05), &json!(3.0));
        assert!(result.passed);
    }

    #[test]
    fn test_numeric_tolerance_outside() {
        let metric = NumericTolerance::new(0.1);
        let result = metric.score(&json!(3.5), &json!(3.0));
        assert!(!result.passed);
    }

    #[test]
    fn test_numeric_tolerance_non_numeric() {
        let metric = NumericTolerance::new(0.1);
        let result = metric.score(&json!("hello"), &json!(3.0));
        assert!(!result.passed);
        assert!(result.details.unwrap().contains("not a number"));
    }

    #[test]
    fn test_json_path_match_simple() {
        let metric = JsonPathMatch::new("answer");
        let result = metric.score(
            &json!({"answer": "Paris", "extra": "data"}),
            &json!({"answer": "Paris"}),
        );
        assert!(result.passed);
    }

    #[test]
    fn test_json_path_match_nested() {
        let metric = JsonPathMatch::new("data.value");
        let result = metric.score(
            &json!({"data": {"value": 42}}),
            &json!({"data": {"value": 42}}),
        );
        assert!(result.passed);
    }

    #[test]
    fn test_json_path_match_array() {
        let metric = JsonPathMatch::new("items[0].name");
        let result = metric.score(
            &json!({"items": [{"name": "first"}]}),
            &json!({"items": [{"name": "first"}]}),
        );
        assert!(result.passed);
    }

    #[test]
    fn test_json_path_match_mismatch() {
        let metric = JsonPathMatch::new("answer");
        let result = metric.score(
            &json!({"answer": "London"}),
            &json!({"answer": "Paris"}),
        );
        assert!(!result.passed);
    }

    #[test]
    fn test_json_path_match_missing_path() {
        let metric = JsonPathMatch::new("missing");
        let result = metric.score(
            &json!({"answer": "Paris"}),
            &json!({"answer": "Paris"}),
        );
        assert!(!result.passed);
    }

    #[test]
    fn test_metric_names() {
        assert_eq!(ExactMatch.name(), "exact_match");
        assert_eq!(ContainsMatch::new().name(), "contains_match");
        assert_eq!(NumericTolerance::new(0.1).name(), "numeric_tolerance");
        assert_eq!(JsonPathMatch::new("test").name(), "json_path_match");
    }
}
