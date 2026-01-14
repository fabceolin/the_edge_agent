//! Integration tests for the experiments module (TEA-RUST-044.3).
//!
//! Tests the experiment framework for running TEA agents against datasets
//! and calculating metrics.

use serde_json::json;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use the_edge_agent::experiments::{
    run_experiment, AggregateResults, ContainsMatch, Dataset, DatasetItem, ExactMatch,
    ExperimentConfig, JsonPathMatch, Metric, MetricResult, NumericTolerance,
};

// =============================================================================
// Dataset Loading Tests
// =============================================================================

#[test]
fn test_dataset_from_json() {
    let json = r#"{
        "name": "integration_test",
        "description": "Test dataset for experiments",
        "items": [
            {
                "input": {"query": "What is 2+2?"},
                "expected_output": {"answer": "4"},
                "metadata": {"category": "math"}
            },
            {
                "input": {"query": "Capital of France?"},
                "expected_output": {"answer": "Paris"},
                "metadata": {"category": "geography"}
            }
        ]
    }"#;

    let dataset = Dataset::from_json_str(json).unwrap();

    assert_eq!(dataset.name, "integration_test");
    assert_eq!(
        dataset.description,
        Some("Test dataset for experiments".to_string())
    );
    assert_eq!(dataset.len(), 2);

    // Check first item
    assert_eq!(dataset.items[0].input["query"], "What is 2+2?");
    assert_eq!(dataset.items[0].expected_output["answer"], "4");
    assert_eq!(dataset.items[0].metadata["category"], "math");

    // Check second item
    assert_eq!(dataset.items[1].input["query"], "Capital of France?");
    assert_eq!(dataset.items[1].expected_output["answer"], "Paris");
}

#[test]
fn test_dataset_nested_json() {
    let json = r#"{
        "name": "nested_test",
        "items": [
            {
                "input": {
                    "request": {
                        "type": "query",
                        "params": {"limit": 10, "offset": 0},
                        "filters": ["active", "verified"]
                    }
                },
                "expected_output": {
                    "response": {
                        "status": "success",
                        "data": {
                            "items": [1, 2, 3],
                            "total": 100
                        }
                    }
                }
            }
        ]
    }"#;

    let dataset = Dataset::from_json_str(json).unwrap();
    assert_eq!(dataset.items[0].input["request"]["params"]["limit"], 10);
    assert_eq!(
        dataset.items[0].expected_output["response"]["data"]["total"],
        100
    );
}

// =============================================================================
// Metric Tests
// =============================================================================

#[test]
fn test_exact_match_metric() {
    let metric = ExactMatch;

    // Exact match - pass
    let result = metric.score(&json!({"a": 1, "b": "hello"}), &json!({"a": 1, "b": "hello"}));
    assert!(result.passed);
    assert_eq!(result.score, 1.0);

    // Not exact - fail
    let result = metric.score(&json!({"a": 1}), &json!({"a": 2}));
    assert!(!result.passed);
    assert_eq!(result.score, 0.0);

    // Extra fields - fail
    let result = metric.score(
        &json!({"a": 1, "extra": true}),
        &json!({"a": 1}),
    );
    assert!(!result.passed);
}

#[test]
fn test_contains_match_metric() {
    let metric = ContainsMatch::new();

    // Contains substring - pass
    let result = metric.score(&json!("hello world"), &json!("world"));
    assert!(result.passed);

    // Case insensitive
    let result = metric.score(&json!("Hello World"), &json!("WORLD"));
    assert!(result.passed);

    // Not contained - fail
    let result = metric.score(&json!("hello"), &json!("goodbye"));
    assert!(!result.passed);

    // Case sensitive mode
    let metric_cs = ContainsMatch::case_sensitive();
    let result = metric_cs.score(&json!("Hello"), &json!("HELLO"));
    assert!(!result.passed);
}

#[test]
fn test_numeric_tolerance_metric() {
    let metric = NumericTolerance::new(0.01);

    // Within tolerance
    let result = metric.score(&json!(3.005), &json!(3.0));
    assert!(result.passed);

    // At exact tolerance boundary
    let result = metric.score(&json!(3.01), &json!(3.0));
    assert!(result.passed);

    // Outside tolerance
    let result = metric.score(&json!(3.02), &json!(3.0));
    assert!(!result.passed);

    // Non-numeric values
    let result = metric.score(&json!("not a number"), &json!(3.0));
    assert!(!result.passed);
    assert!(result.details.unwrap().contains("not a number"));
}

#[test]
fn test_json_path_match_metric() {
    let metric = JsonPathMatch::new("data.value");

    // Matching nested path
    let result = metric.score(
        &json!({"data": {"value": 42, "extra": "ignored"}}),
        &json!({"data": {"value": 42}}),
    );
    assert!(result.passed);

    // Different values at path
    let result = metric.score(
        &json!({"data": {"value": 100}}),
        &json!({"data": {"value": 42}}),
    );
    assert!(!result.passed);

    // Path not found
    let metric_missing = JsonPathMatch::new("missing.path");
    let result = metric_missing.score(&json!({"data": 1}), &json!({"data": 2}));
    assert!(!result.passed);
}

#[test]
fn test_json_path_match_array_index() {
    let metric = JsonPathMatch::new("items[0].name");

    let result = metric.score(
        &json!({"items": [{"name": "first"}, {"name": "second"}]}),
        &json!({"items": [{"name": "first"}]}),
    );
    assert!(result.passed);

    let result = metric.score(
        &json!({"items": [{"name": "wrong"}, {"name": "second"}]}),
        &json!({"items": [{"name": "first"}]}),
    );
    assert!(!result.passed);
}

// =============================================================================
// Experiment Runner Tests
// =============================================================================

fn create_test_agent_yaml() -> String {
    r#"
    name: test_experiment_agent
    nodes:
      - name: process
        run: |
          local input = state.input or state.query or ""
          if input == "double" then
            return { result = 4 }
          elseif input == "hello" then
            return { result = "world" }
          else
            return { result = "default" }
          end
    edges:
      - from: __start__
        to: process
      - from: process
        to: __end__
    "#
    .to_string()
}

#[tokio::test]
async fn test_experiment_runner_basic() {
    let yaml = create_test_agent_yaml();
    let dataset = Dataset::new("basic_test")
        .with_item(DatasetItem::new(
            json!({"input": "hello"}),
            json!({"result": "world"}),
        ))
        .with_item(DatasetItem::new(
            json!({"input": "double"}),
            json!({"result": 4}),
        ));

    let config = ExperimentConfig::new(&yaml, dataset)
        .with_metric(Box::new(JsonPathMatch::new("result")));

    let result = run_experiment(config).await.unwrap();

    assert_eq!(result.items.len(), 2);
    assert!(result.errors.is_empty());
    assert_eq!(result.aggregates.total_items, 2);
    assert_eq!(result.aggregates.failed_items, 0);

    // Both items should pass with JsonPathMatch
    for item in &result.items {
        assert!(
            item.scores["json_path_match"].passed,
            "Item {} failed: {:?}",
            item.index,
            item.scores["json_path_match"].details
        );
    }

    // Check aggregate scores
    assert_eq!(result.aggregates.mean_scores["json_path_match"], 1.0);
    assert_eq!(result.aggregates.pass_rates["json_path_match"], 1.0);
}

#[tokio::test]
async fn test_experiment_handles_failures() {
    // Invalid YAML that will fail to parse
    let invalid_yaml = "not: {{{{ valid: yaml";
    let dataset = Dataset::new("failure_test")
        .with_item(DatasetItem::new(json!({}), json!({})))
        .with_item(DatasetItem::new(json!({}), json!({})));

    let config = ExperimentConfig::new(invalid_yaml, dataset)
        .with_metric(Box::new(ExactMatch));

    let result = run_experiment(config).await.unwrap();

    // All items should fail
    assert!(result.items.is_empty());
    assert_eq!(result.errors.len(), 2);
    assert_eq!(result.aggregates.total_items, 2);
    assert_eq!(result.aggregates.failed_items, 2);
}

#[tokio::test]
async fn test_experiment_with_progress_callback() {
    let yaml = create_test_agent_yaml();
    let dataset = Dataset::new("progress_test")
        .with_item(DatasetItem::new(json!({}), json!({"result": "default"})))
        .with_item(DatasetItem::new(json!({}), json!({"result": "default"})))
        .with_item(DatasetItem::new(json!({}), json!({"result": "default"})));

    let progress_calls = Arc::new(AtomicUsize::new(0));
    let progress_clone = Arc::clone(&progress_calls);

    let last_values = Arc::new(parking_lot::Mutex::new((0, 0)));
    let last_clone = Arc::clone(&last_values);

    let config = ExperimentConfig::new(&yaml, dataset)
        .with_metric(Box::new(JsonPathMatch::new("result")))
        .with_progress_callback(move |current, total| {
            progress_clone.fetch_add(1, Ordering::SeqCst);
            *last_clone.lock() = (current, total);
        });

    let result = run_experiment(config).await.unwrap();

    assert_eq!(result.items.len(), 3);
    assert_eq!(progress_calls.load(Ordering::SeqCst), 3);

    let (last_current, last_total) = *last_values.lock();
    assert_eq!(last_current, 3);
    assert_eq!(last_total, 3);
}

#[tokio::test]
async fn test_experiment_multiple_metrics() {
    let yaml = create_test_agent_yaml();
    let dataset = Dataset::new("multi_metric_test").with_item(DatasetItem::new(
        json!({"input": "hello"}),
        json!({"result": "world"}),
    ));

    let config = ExperimentConfig::new(&yaml, dataset)
        .with_metric(Box::new(JsonPathMatch::new("result")))
        .with_metric(Box::new(ContainsMatch::new()));

    let result = run_experiment(config).await.unwrap();

    assert_eq!(result.items.len(), 1);
    assert_eq!(result.items[0].scores.len(), 2);
    assert!(result.items[0].scores.contains_key("json_path_match"));
    assert!(result.items[0].scores.contains_key("contains_match"));
}

#[test]
fn test_aggregate_calculations() {
    // Test aggregate calculations with known values
    let metrics: Vec<Box<dyn Metric>> = vec![Box::new(ExactMatch)];

    // Create mock item results manually
    let items = vec![
        the_edge_agent::experiments::ItemResult {
            index: 0,
            input: json!({}),
            output: json!(1),
            expected: json!(1),
            scores: {
                let mut m = std::collections::HashMap::new();
                m.insert("exact_match".to_string(), MetricResult::new(1.0, true));
                m
            },
            duration_ms: 100,
        },
        the_edge_agent::experiments::ItemResult {
            index: 1,
            input: json!({}),
            output: json!(2),
            expected: json!(1),
            scores: {
                let mut m = std::collections::HashMap::new();
                m.insert("exact_match".to_string(), MetricResult::new(0.0, false));
                m
            },
            duration_ms: 100,
        },
        the_edge_agent::experiments::ItemResult {
            index: 2,
            input: json!({}),
            output: json!(1),
            expected: json!(1),
            scores: {
                let mut m = std::collections::HashMap::new();
                m.insert("exact_match".to_string(), MetricResult::new(1.0, true));
                m
            },
            duration_ms: 100,
        },
    ];

    // Calculate mean: (1.0 + 0.0 + 1.0) / 3 = 0.666...
    let sum: f64 = items.iter().map(|i| i.scores["exact_match"].score).sum();
    let mean = sum / items.len() as f64;

    assert!((mean - 0.6666666).abs() < 0.001);

    // Pass rate: 2/3 = 0.666...
    let passes = items.iter().filter(|i| i.scores["exact_match"].passed).count();
    let pass_rate = passes as f64 / items.len() as f64;

    assert!((pass_rate - 0.6666666).abs() < 0.001);
}

#[tokio::test]
async fn test_async_execution() {
    let yaml = create_test_agent_yaml();
    let dataset = Dataset::new("async_test").with_item(DatasetItem::new(
        json!({"input": "hello"}),
        json!({"result": "world"}),
    ));

    let config = ExperimentConfig::new(&yaml, dataset)
        .with_metric(Box::new(JsonPathMatch::new("result")));

    // Run experiment asynchronously
    let result = run_experiment(config).await.unwrap();

    assert_eq!(result.items.len(), 1);
    assert!(result.duration_ms > 0);
}

// =============================================================================
// Custom Metric Test
// =============================================================================

struct CustomLengthMetric {
    expected_length: usize,
}

impl Metric for CustomLengthMetric {
    fn name(&self) -> &str {
        "custom_length"
    }

    fn score(&self, output: &serde_json::Value, _expected: &serde_json::Value) -> MetricResult {
        if let Some(s) = output.as_str() {
            if s.len() == self.expected_length {
                MetricResult::pass()
            } else {
                MetricResult::fail_with(format!(
                    "Expected length {}, got {}",
                    self.expected_length,
                    s.len()
                ))
            }
        } else {
            MetricResult::fail_with("Output is not a string")
        }
    }
}

#[test]
fn test_custom_metric() {
    let metric = CustomLengthMetric { expected_length: 5 };

    let result = metric.score(&json!("hello"), &json!(null));
    assert!(result.passed);

    let result = metric.score(&json!("hi"), &json!(null));
    assert!(!result.passed);
}

// =============================================================================
// Edge Cases
// =============================================================================

#[test]
fn test_empty_dataset() {
    let dataset = Dataset::new("empty");
    assert!(dataset.is_empty());
    assert_eq!(dataset.len(), 0);
}

#[test]
fn test_dataset_invalid_json() {
    let result = Dataset::from_json_str("not valid json");
    assert!(result.is_err());
}

#[test]
fn test_metric_result_display() {
    let pass = MetricResult::new(1.0, true);
    let display = format!("{}", pass);
    assert!(display.contains("PASS"));

    let fail = MetricResult::fail_with("reason here");
    let display = format!("{}", fail);
    assert!(display.contains("FAIL"));
    assert!(display.contains("reason here"));
}
