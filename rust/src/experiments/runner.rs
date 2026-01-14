//! Experiment runner for executing agents against datasets.
//!
//! The experiment runner executes a TEA agent against each item in a dataset,
//! calculates metrics, and aggregates results.
//!
//! # Opik Integration
//!
//! When `opik_enabled` is set to `true` in the `ExperimentConfig`, the experiment
//! runner will create traces for each dataset item execution. This allows you to
//! visualize and analyze agent behavior in the Opik dashboard.
//!
//! Each trace includes:
//! - Input data from the dataset item
//! - Agent output
//! - Metric scores as metadata
//! - Execution duration

use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use crate::engine::observability::{set_trace_context, ObsConfig, TraceContext};
use crate::experiments::{Dataset, DatasetItem, Metric, MetricResult};
use crate::{ActionRegistry, Executor, StateGraph, TeaError};

/// Configuration for running an experiment.
///
/// # Example
///
/// ```rust,ignore
/// use the_edge_agent::experiments::{ExperimentConfig, Dataset, ExactMatch};
///
/// let config = ExperimentConfig::new(agent_yaml, dataset)
///     .with_metric(Box::new(ExactMatch))
///     .with_progress_callback(|current, total| {
///         println!("Progress: {}/{}", current, total);
///     });
/// ```
pub struct ExperimentConfig {
    /// YAML definition of the agent to run
    pub agent_yaml: String,

    /// Dataset to evaluate against
    pub dataset: Dataset,

    /// Metrics to calculate for each item
    pub metrics: Vec<Box<dyn Metric>>,

    /// Optional callback for progress reporting
    pub progress_callback: Option<Box<dyn Fn(usize, usize) + Send + Sync>>,

    /// Whether to enable Opik tracing
    pub opik_enabled: bool,

    /// Optional action registry for custom actions
    pub action_registry: Option<Arc<ActionRegistry>>,

    /// Optional experiment name for Opik tracing
    pub experiment_name: Option<String>,
}

impl ExperimentConfig {
    /// Create a new experiment configuration.
    pub fn new(agent_yaml: impl Into<String>, dataset: Dataset) -> Self {
        Self {
            agent_yaml: agent_yaml.into(),
            dataset,
            metrics: Vec::new(),
            progress_callback: None,
            opik_enabled: false,
            action_registry: None,
            experiment_name: None,
        }
    }

    /// Set the experiment name for Opik tracing.
    pub fn with_experiment_name(mut self, name: impl Into<String>) -> Self {
        self.experiment_name = Some(name.into());
        self
    }

    /// Add a metric to the configuration.
    pub fn with_metric(mut self, metric: Box<dyn Metric>) -> Self {
        self.metrics.push(metric);
        self
    }

    /// Add multiple metrics to the configuration.
    pub fn with_metrics(mut self, metrics: impl IntoIterator<Item = Box<dyn Metric>>) -> Self {
        self.metrics.extend(metrics);
        self
    }

    /// Set a progress callback.
    pub fn with_progress_callback<F>(mut self, callback: F) -> Self
    where
        F: Fn(usize, usize) + Send + Sync + 'static,
    {
        self.progress_callback = Some(Box::new(callback));
        self
    }

    /// Enable Opik tracing.
    pub fn with_opik_enabled(mut self, enabled: bool) -> Self {
        self.opik_enabled = enabled;
        self
    }

    /// Set a custom action registry.
    pub fn with_action_registry(mut self, registry: Arc<ActionRegistry>) -> Self {
        self.action_registry = Some(registry);
        self
    }
}

/// Result for a single dataset item.
#[derive(Debug, Clone)]
pub struct ItemResult {
    /// Index of the item in the dataset
    pub index: usize,

    /// Input that was provided to the agent
    pub input: Value,

    /// Output produced by the agent
    pub output: Value,

    /// Expected output from the dataset
    pub expected: Value,

    /// Metric scores for this item
    pub scores: HashMap<String, MetricResult>,

    /// Execution duration in milliseconds
    pub duration_ms: u64,
}

/// Aggregate results across all items.
#[derive(Debug, Clone)]
pub struct AggregateResults {
    /// Mean score for each metric
    pub mean_scores: HashMap<String, f64>,

    /// Minimum score for each metric
    pub min_scores: HashMap<String, f64>,

    /// Maximum score for each metric
    pub max_scores: HashMap<String, f64>,

    /// Pass rate for each metric (percentage of items passing)
    pub pass_rates: HashMap<String, f64>,

    /// Total number of items in the dataset
    pub total_items: usize,

    /// Number of items that failed to execute
    pub failed_items: usize,
}

impl AggregateResults {
    fn new() -> Self {
        Self {
            mean_scores: HashMap::new(),
            min_scores: HashMap::new(),
            max_scores: HashMap::new(),
            pass_rates: HashMap::new(),
            total_items: 0,
            failed_items: 0,
        }
    }
}

/// Result of running an experiment.
#[derive(Debug)]
pub struct ExperimentResult {
    /// Results for each item
    pub items: Vec<ItemResult>,

    /// Aggregate statistics
    pub aggregates: AggregateResults,

    /// Total duration in milliseconds
    pub duration_ms: u64,

    /// Errors that occurred (item index, error message)
    pub errors: Vec<(usize, String)>,
}

/// Run an experiment with the given configuration.
///
/// Executes the agent against each item in the dataset, calculates metrics,
/// and returns aggregated results. Individual item failures are captured
/// but do not stop the entire experiment.
///
/// # Arguments
///
/// * `config` - Experiment configuration including agent, dataset, and metrics
///
/// # Returns
///
/// An `ExperimentResult` containing per-item scores and aggregate statistics.
///
/// # Example
///
/// ```rust,ignore
/// use the_edge_agent::experiments::{run_experiment, ExperimentConfig, Dataset, ExactMatch};
///
/// let dataset = Dataset::from_json_str(r#"{"name": "test", "items": [...]}"#)?;
/// let config = ExperimentConfig::new(agent_yaml, dataset)
///     .with_metric(Box::new(ExactMatch));
///
/// let result = run_experiment(config).await?;
/// println!("Mean exact match: {}", result.aggregates.mean_scores["exact_match"]);
/// ```
pub async fn run_experiment(config: ExperimentConfig) -> Result<ExperimentResult, TeaError> {
    let start_time = Instant::now();
    let mut items = Vec::new();
    let mut errors = Vec::new();
    let total = config.dataset.items.len();

    // Create experiment-level trace context when Opik is enabled
    let experiment_trace = if config.opik_enabled {
        let ctx = TraceContext::new_root();
        log::debug!(
            "[EXPERIMENT] Created experiment trace: {} (name: {:?})",
            ctx.trace_id,
            config.experiment_name
        );
        Some(ctx)
    } else {
        None
    };

    for (index, dataset_item) in config.dataset.items.iter().enumerate() {
        // Report progress
        if let Some(ref callback) = config.progress_callback {
            callback(index + 1, total);
        }

        // Create child trace context for this item
        let item_trace = experiment_trace.as_ref().map(|parent| {
            let child = parent.child_span_named(format!("item_{}", index));
            log::debug!(
                "[EXPERIMENT] Item {} trace: span_id={}, parent_span_id={:?}",
                index,
                child.span_id,
                child.parent_span_id
            );
            child
        });

        // Set trace context for this item execution
        if let Some(ctx) = item_trace {
            set_trace_context(Some(ctx));
        }

        // Execute item with error handling
        match execute_item(&config, dataset_item, index).await {
            Ok((output, item_duration_ms)) => {
                let scores =
                    calculate_scores(&config.metrics, &output, &dataset_item.expected_output);
                items.push(ItemResult {
                    index,
                    input: dataset_item.input.clone(),
                    output,
                    expected: dataset_item.expected_output.clone(),
                    scores,
                    duration_ms: item_duration_ms,
                });
            }
            Err(e) => {
                errors.push((index, e.to_string()));
            }
        }

        // Clear trace context after item execution
        if config.opik_enabled {
            set_trace_context(None);
        }
    }

    let aggregates = calculate_aggregates(&items, &config.metrics, total);
    let duration_ms = start_time.elapsed().as_millis() as u64;

    Ok(ExperimentResult {
        items,
        aggregates,
        duration_ms,
        errors,
    })
}

/// Execute a single dataset item against the agent.
async fn execute_item(
    config: &ExperimentConfig,
    item: &DatasetItem,
    item_index: usize,
) -> Result<(Value, u64), TeaError> {
    let start_time = Instant::now();

    // Parse and compile the agent graph
    let graph = StateGraph::from_yaml(&config.agent_yaml)?;
    let compiled = graph.compile()?;

    // Create executor with optional custom registry and observability
    let executor = if config.opik_enabled {
        // Create observability config for Opik tracing
        let obs_config = ObsConfig {
            enabled: true,
            ..Default::default()
        };

        if let Some(ref registry) = config.action_registry {
            Executor::with_actions_and_observability(compiled, Arc::clone(registry), obs_config)?
        } else {
            Executor::with_observability(compiled, obs_config)?
        }
    } else if let Some(ref registry) = config.action_registry {
        Executor::with_actions(compiled, Arc::clone(registry))?
    } else {
        Executor::new(compiled)?
    };

    log::debug!(
        "[EXPERIMENT] Executing item {} with input: {:?}",
        item_index,
        item.input
    );

    // Run the agent with the input
    let result = executor.invoke(item.input.clone())?;
    let duration_ms = start_time.elapsed().as_millis() as u64;

    log::debug!(
        "[EXPERIMENT] Item {} completed in {}ms",
        item_index,
        duration_ms
    );

    Ok((result, duration_ms))
}

/// Calculate metric scores for an output.
fn calculate_scores(
    metrics: &[Box<dyn Metric>],
    output: &Value,
    expected: &Value,
) -> HashMap<String, MetricResult> {
    let mut scores = HashMap::new();
    for metric in metrics {
        let result = metric.score(output, expected);
        scores.insert(metric.name().to_string(), result);
    }
    scores
}

/// Calculate aggregate statistics from item results.
fn calculate_aggregates(
    items: &[ItemResult],
    metrics: &[Box<dyn Metric>],
    total_items: usize,
) -> AggregateResults {
    let mut aggregates = AggregateResults::new();
    aggregates.total_items = total_items;
    aggregates.failed_items = total_items - items.len();

    if items.is_empty() {
        return aggregates;
    }

    // Initialize metric accumulators
    for metric in metrics {
        let metric_name = metric.name().to_string();
        let mut scores: Vec<f64> = Vec::new();
        let mut passes: usize = 0;

        for item in items {
            if let Some(result) = item.scores.get(&metric_name) {
                scores.push(result.score);
                if result.passed {
                    passes += 1;
                }
            }
        }

        if !scores.is_empty() {
            let sum: f64 = scores.iter().sum();
            let mean = sum / scores.len() as f64;
            let min = scores.iter().cloned().fold(f64::INFINITY, f64::min);
            let max = scores.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
            let pass_rate = passes as f64 / scores.len() as f64;

            aggregates.mean_scores.insert(metric_name.clone(), mean);
            aggregates.min_scores.insert(metric_name.clone(), min);
            aggregates.max_scores.insert(metric_name.clone(), max);
            aggregates.pass_rates.insert(metric_name, pass_rate);
        }
    }

    aggregates
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::experiments::{ContainsMatch, ExactMatch, NumericTolerance};
    use serde_json::json;

    fn create_test_dataset() -> Dataset {
        Dataset::new("test")
            .with_item(DatasetItem::new(json!({"x": 1}), json!({"result": "one"})))
            .with_item(DatasetItem::new(json!({"x": 2}), json!({"result": "two"})))
    }

    fn create_mock_agent_yaml() -> String {
        r#"
        name: test_agent
        nodes:
          - name: process
            run: |
              return { result = "one" }
        edges:
          - from: __start__
            to: process
          - from: process
            to: __end__
        "#
        .to_string()
    }

    #[test]
    fn test_experiment_config_builder() {
        let dataset = create_test_dataset();
        let config = ExperimentConfig::new("yaml", dataset)
            .with_metric(Box::new(ExactMatch))
            .with_metric(Box::new(ContainsMatch::new()))
            .with_opik_enabled(true);

        assert_eq!(config.metrics.len(), 2);
        assert!(config.opik_enabled);
    }

    #[test]
    fn test_calculate_scores() {
        let metrics: Vec<Box<dyn Metric>> =
            vec![Box::new(ExactMatch), Box::new(ContainsMatch::new())];

        let output = json!({"answer": "hello world"});
        let expected = json!({"answer": "hello world"});

        let scores = calculate_scores(&metrics, &output, &expected);

        assert!(scores.get("exact_match").unwrap().passed);
        assert!(scores.get("contains_match").unwrap().passed);
    }

    #[test]
    fn test_calculate_aggregates() {
        let metrics: Vec<Box<dyn Metric>> = vec![Box::new(ExactMatch)];

        let items = vec![
            ItemResult {
                index: 0,
                input: json!({}),
                output: json!({"a": 1}),
                expected: json!({"a": 1}),
                scores: {
                    let mut m = HashMap::new();
                    m.insert("exact_match".to_string(), MetricResult::new(1.0, true));
                    m
                },
                duration_ms: 100,
            },
            ItemResult {
                index: 1,
                input: json!({}),
                output: json!({"a": 2}),
                expected: json!({"a": 1}),
                scores: {
                    let mut m = HashMap::new();
                    m.insert("exact_match".to_string(), MetricResult::new(0.0, false));
                    m
                },
                duration_ms: 100,
            },
        ];

        let aggregates = calculate_aggregates(&items, &metrics, 2);

        assert_eq!(aggregates.total_items, 2);
        assert_eq!(aggregates.failed_items, 0);
        assert_eq!(aggregates.mean_scores["exact_match"], 0.5);
        assert_eq!(aggregates.min_scores["exact_match"], 0.0);
        assert_eq!(aggregates.max_scores["exact_match"], 1.0);
        assert_eq!(aggregates.pass_rates["exact_match"], 0.5);
    }

    #[test]
    fn test_calculate_aggregates_empty() {
        let metrics: Vec<Box<dyn Metric>> = vec![Box::new(ExactMatch)];
        let items: Vec<ItemResult> = vec![];

        let aggregates = calculate_aggregates(&items, &metrics, 5);

        assert_eq!(aggregates.total_items, 5);
        assert_eq!(aggregates.failed_items, 5);
        assert!(aggregates.mean_scores.is_empty());
    }

    #[tokio::test]
    async fn test_run_experiment_basic() {
        let yaml = create_mock_agent_yaml();
        let dataset = Dataset::new("test").with_item(DatasetItem::new(
            json!({"input": "test"}),
            json!({"result": "one"}),
        ));

        // Use JsonPathMatch to compare just the "result" field
        let config = ExperimentConfig::new(&yaml, dataset)
            .with_metric(Box::new(crate::experiments::JsonPathMatch::new("result")));

        let result = run_experiment(config).await.unwrap();

        assert_eq!(result.items.len(), 1);
        assert!(result.errors.is_empty());
        // The agent output should have result="one" which matches expected
        assert!(
            result.items[0]
                .scores
                .get("json_path_match")
                .unwrap()
                .passed,
            "Expected json_path_match to pass. Output: {:?}, Expected: {:?}, Details: {:?}",
            result.items[0].output,
            result.items[0].expected,
            result.items[0]
                .scores
                .get("json_path_match")
                .unwrap()
                .details
        );
    }

    #[tokio::test]
    async fn test_run_experiment_with_failure() {
        // Use invalid YAML to simulate a failure
        let invalid_yaml = "not valid yaml: {{{{";
        let dataset = Dataset::new("test").with_item(DatasetItem::new(json!({}), json!({})));

        let config = ExperimentConfig::new(invalid_yaml, dataset).with_metric(Box::new(ExactMatch));

        let result = run_experiment(config).await.unwrap();

        // The experiment should complete but with errors
        assert!(result.items.is_empty());
        assert_eq!(result.errors.len(), 1);
        assert_eq!(result.aggregates.failed_items, 1);
    }

    #[tokio::test]
    async fn test_run_experiment_with_progress() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let yaml = create_mock_agent_yaml();
        let dataset = Dataset::new("test")
            .with_item(DatasetItem::new(json!({}), json!({"result": "one"})))
            .with_item(DatasetItem::new(json!({}), json!({"result": "one"})));

        let progress_count = Arc::new(AtomicUsize::new(0));
        let progress_clone = Arc::clone(&progress_count);

        let config = ExperimentConfig::new(&yaml, dataset)
            .with_metric(Box::new(ExactMatch))
            .with_progress_callback(move |_, _| {
                progress_clone.fetch_add(1, Ordering::SeqCst);
            });

        let _ = run_experiment(config).await.unwrap();

        assert_eq!(progress_count.load(Ordering::SeqCst), 2);
    }

    #[tokio::test]
    async fn test_run_experiment_multiple_metrics() {
        let yaml = create_mock_agent_yaml();
        let dataset =
            Dataset::new("test").with_item(DatasetItem::new(json!({}), json!({"result": "one"})));

        let config = ExperimentConfig::new(&yaml, dataset)
            .with_metric(Box::new(ExactMatch))
            .with_metric(Box::new(ContainsMatch::new()));

        let result = run_experiment(config).await.unwrap();

        assert_eq!(result.items[0].scores.len(), 2);
        assert!(result.items[0].scores.contains_key("exact_match"));
        assert!(result.items[0].scores.contains_key("contains_match"));
    }

    #[test]
    fn test_item_result_structure() {
        let item = ItemResult {
            index: 0,
            input: json!({"query": "test"}),
            output: json!({"answer": "result"}),
            expected: json!({"answer": "result"}),
            scores: HashMap::new(),
            duration_ms: 150,
        };

        assert_eq!(item.index, 0);
        assert_eq!(item.input["query"], "test");
        assert_eq!(item.duration_ms, 150);
    }

    #[test]
    fn test_aggregate_results_structure() {
        let mut aggregates = AggregateResults::new();
        aggregates.mean_scores.insert("test".to_string(), 0.75);
        aggregates.pass_rates.insert("test".to_string(), 0.8);
        aggregates.total_items = 10;
        aggregates.failed_items = 2;

        assert_eq!(aggregates.mean_scores["test"], 0.75);
        assert_eq!(aggregates.pass_rates["test"], 0.8);
        assert_eq!(aggregates.total_items, 10);
        assert_eq!(aggregates.failed_items, 2);
    }
}
