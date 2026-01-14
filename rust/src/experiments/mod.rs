//! Experiment Framework for TEA Agents
//!
//! This module provides a basic experiment framework to run TEA agents against datasets
//! and calculate metrics for systematic quality measurement and comparison.
//!
//! # Features
//!
//! - **Experiment Runner**: Execute agents against dataset items with metric scoring
//! - **Dataset Loading**: Load test datasets from JSON fixture files
//! - **Custom Metrics**: Pluggable scoring implementations via the `Metric` trait
//! - **Built-in Metrics**: `ExactMatch`, `ContainsMatch`, `NumericTolerance`, `JsonPathMatch`
//! - **Result Aggregation**: Per-item scores and aggregate statistics (mean, min, max)
//! - **Error Handling**: Single item failures don't crash entire experiment
//! - **Progress Callback**: Optional callback for progress reporting
//! - **Async Support**: Experiment runner works with async agent execution
//!
//! # Quick Start
//!
//! ```rust,ignore
//! use the_edge_agent::experiments::{run_experiment, ExperimentConfig, Dataset, ExactMatch};
//!
//! // Load dataset
//! let dataset = Dataset::from_json_str(r#"{
//!     "name": "test",
//!     "items": [
//!         {"input": {"x": 1}, "expected_output": {"y": 2}}
//!     ]
//! }"#)?;
//!
//! // Configure experiment
//! let config = ExperimentConfig::new(agent_yaml, dataset)
//!     .with_metric(Box::new(ExactMatch));
//!
//! // Run experiment
//! let result = run_experiment(config).await?;
//!
//! println!("Mean score: {}", result.aggregates.mean_scores["exact_match"]);
//! ```

mod dataset;
mod metrics;
mod runner;

pub use dataset::{Dataset, DatasetItem};
pub use metrics::{
    ContainsMatch, ExactMatch, JsonPathMatch, Metric, MetricResult, NumericTolerance,
};
pub use runner::{
    run_experiment, AggregateResults, ExperimentConfig, ExperimentResult, ItemResult,
};
