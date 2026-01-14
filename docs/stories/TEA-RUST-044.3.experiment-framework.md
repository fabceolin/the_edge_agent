# Story TEA-RUST-044.3: Basic Experiment Framework for Rust Agents

## Status

Done

## Test Design

Completed: 2026-01-13
Document: `docs/qa/assessments/44.3-experiment-framework-test-design-20260113.md`
Total Scenarios: 42 (P0: 14, P1: 18, P2: 10)

## Story

**As a** developer evaluating Rust TEA agents,
**I want** a basic experiment framework to run agents against datasets and calculate metrics,
**So that** I can systematically measure agent quality and compare different implementations.

## Acceptance Criteria

1. **Experiment Runner**: `run_experiment()` function executes agent against dataset items
2. **Dataset Loading**: Load test datasets from JSON fixture files
3. **Custom Metrics**: `Metric` trait allows pluggable scoring implementations
4. **Built-in Metrics**: `ExactMatch`, `ContainsMatch`, `NumericTolerance` metrics included
5. **Result Aggregation**: Results include per-item scores and aggregate statistics (mean, min, max)
6. **Error Handling**: Single item failures don't crash entire experiment
7. **Opik Integration**: Experiments traced to Opik when enabled (optional)
8. **Progress Callback**: Optional callback for progress reporting
9. **Async Support**: Experiment runner works with async agent execution

## Tasks / Subtasks

- [x] **Task 1: Create Experiments Module Structure** (AC: 1)
  - [x] Create `rust/src/experiments/mod.rs`
  - [x] Create `rust/src/experiments/runner.rs`
  - [x] Create `rust/src/experiments/metrics.rs`
  - [x] Create `rust/src/experiments/dataset.rs`
  - [x] Export module from `rust/src/lib.rs`

- [x] **Task 2: Implement Dataset Loading** (AC: 2)
  - [x] Define `DatasetItem` struct with `input`, `expected_output`, `metadata`
  - [x] Define `Dataset` struct with `name`, `items`, `description`
  - [x] Implement `Dataset::from_json_file(path)` for file loading
  - [x] Implement `Dataset::from_json_str(json)` for inline data
  - [x] Support nested JSON structures for complex inputs

- [x] **Task 3: Implement Metric Trait** (AC: 3)
  - [x] Define `Metric` trait with `name()` and `score()` methods
  - [x] `score()` takes `output: &Value`, `expected: &Value` → `MetricResult`
  - [x] `MetricResult` contains `score: f64`, `passed: bool`, `details: Option<String>`
  - [x] Add `Display` implementation for debugging

- [x] **Task 4: Implement Built-in Metrics** (AC: 4)
  - [x] `ExactMatch` - Returns 1.0 if output == expected, else 0.0
  - [x] `ContainsMatch` - Returns 1.0 if output contains expected substring
  - [x] `NumericTolerance` - Returns 1.0 if within tolerance, else 0.0
  - [x] `JsonPathMatch` - Compares specific JSON paths
  - [x] Add constructors with configurable options

- [x] **Task 5: Implement Experiment Runner** (AC: 1, 6, 8, 9)
  - [x] Define `ExperimentConfig` struct (agent_yaml, dataset, metrics, progress_callback)
  - [x] Define `ExperimentResult` struct (items, aggregates, duration, errors)
  - [x] Implement `run_experiment(config) -> ExperimentResult`
  - [x] Execute agent for each dataset item
  - [x] Calculate metrics for each item
  - [x] Aggregate results across all items
  - [x] Handle individual item failures gracefully (continue to next)
  - [x] Support optional progress callback

- [x] **Task 6: Implement Result Aggregation** (AC: 5)
  - [x] Calculate mean score per metric
  - [x] Calculate min/max scores
  - [x] Calculate pass rate (% of items with score >= threshold)
  - [x] Calculate total duration and per-item timing
  - [x] Generate summary statistics struct

- [x] **Task 7: Add Opik Integration** (AC: 7)
  - [x] Create experiment-level trace when Opik enabled
  - [x] Each dataset item execution as child span
  - [x] Include metric scores in span metadata
  - [x] Use trace context from TEA-RUST-044.2
  - [x] Optional: send `_opik_graph_definition` for experiment

- [x] **Task 8: Write Unit Tests**
  - [x] Test dataset loading from JSON
  - [x] Test each built-in metric
  - [x] Test experiment runner with mock agent
  - [x] Test error handling (failing items)
  - [x] Test result aggregation calculations
  - [x] Test async execution

- [x] **Task 9: Documentation**
  - [x] Add module-level documentation
  - [x] Add examples in docstrings
  - [x] Update Rust development guide with experiments section

## File List

### New Files
| File | Description |
|------|-------------|
| `rust/src/experiments/mod.rs` | Module definition and exports |
| `rust/src/experiments/dataset.rs` | Dataset and DatasetItem types with JSON loading |
| `rust/src/experiments/metrics.rs` | Metric trait and built-in metrics |
| `rust/src/experiments/runner.rs` | run_experiment() function and result types |
| `rust/tests/test_experiments.rs` | Integration tests (17 tests) |

### Modified Files
| File | Description |
|------|-------------|
| `rust/src/lib.rs` | Added `pub mod experiments;` |
| `rust/Cargo.toml` | Added `log` dependency |
| `rust/README.md` | Added Experiment Framework section |
| `docs/rust/development-guide.md` | Added Experiment Framework section |

## Dev Notes

### Python Reference Implementation

**File:** `python/src/the_edge_agent/experiments/runner.py`

```python
def run_tea_experiment(
    agent_yaml: str,
    dataset_name: str,
    metrics: List[BaseTeaMetric],
    experiment_name: Optional[str] = None,
    include_graph: bool = True,
) -> ExperimentResult:
    """Run TEA agent experiment using Opik evaluation."""
    # Load dataset
    dataset = get_dataset(dataset_name)

    # Create evaluation function
    def evaluate_item(item: DatasetItem) -> Dict:
        engine = YAMLEngine.from_yaml(agent_yaml)
        result = list(engine.invoke(item.input))[-1]
        return result

    # Run experiment
    results = opik.evaluate(
        experiment_name=experiment_name,
        dataset=dataset,
        task=evaluate_item,
        scoring_metrics=metrics,
    )

    return results
```

### Rust Module Structure

```
rust/src/experiments/
├── mod.rs          # Module exports
├── runner.rs       # run_experiment() function
├── metrics.rs      # Metric trait + built-in metrics
└── dataset.rs      # Dataset, DatasetItem structs
```

### Core Types

```rust
// dataset.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatasetItem {
    pub input: serde_json::Value,
    pub expected_output: serde_json::Value,
    #[serde(default)]
    pub metadata: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone)]
pub struct Dataset {
    pub name: String,
    pub description: Option<String>,
    pub items: Vec<DatasetItem>,
}

impl Dataset {
    pub fn from_json_file(path: &Path) -> Result<Self, Error> { /* ... */ }
    pub fn from_json_str(json: &str) -> Result<Self, Error> { /* ... */ }
}

// metrics.rs
pub trait Metric: Send + Sync {
    fn name(&self) -> &str;
    fn score(&self, output: &Value, expected: &Value) -> MetricResult;
}

#[derive(Debug, Clone)]
pub struct MetricResult {
    pub score: f64,        // 0.0 to 1.0
    pub passed: bool,      // score >= threshold
    pub details: Option<String>,
}

// Built-in metrics
pub struct ExactMatch;
pub struct ContainsMatch { pub case_sensitive: bool }
pub struct NumericTolerance { pub tolerance: f64 }
pub struct JsonPathMatch { pub path: String }

// runner.rs
pub struct ExperimentConfig {
    pub agent_yaml: String,
    pub dataset: Dataset,
    pub metrics: Vec<Box<dyn Metric>>,
    pub progress_callback: Option<Box<dyn Fn(usize, usize) + Send>>,
    pub opik_enabled: bool,
}

pub struct ExperimentResult {
    pub items: Vec<ItemResult>,
    pub aggregates: AggregateResults,
    pub duration_ms: u64,
    pub errors: Vec<(usize, String)>,
}

pub struct ItemResult {
    pub index: usize,
    pub input: Value,
    pub output: Value,
    pub expected: Value,
    pub scores: HashMap<String, MetricResult>,
    pub duration_ms: u64,
}

pub struct AggregateResults {
    pub mean_scores: HashMap<String, f64>,
    pub min_scores: HashMap<String, f64>,
    pub max_scores: HashMap<String, f64>,
    pub pass_rates: HashMap<String, f64>,
    pub total_items: usize,
    pub failed_items: usize,
}

pub async fn run_experiment(config: ExperimentConfig) -> Result<ExperimentResult, Error> {
    // Implementation
}
```

### JSON Dataset Format

```json
{
  "name": "test_dataset",
  "description": "Test cases for agent evaluation",
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
}
```

### Error Handling Pattern

```rust
pub async fn run_experiment(config: ExperimentConfig) -> Result<ExperimentResult, Error> {
    let mut items = Vec::new();
    let mut errors = Vec::new();

    for (index, dataset_item) in config.dataset.items.iter().enumerate() {
        // Report progress
        if let Some(ref callback) = config.progress_callback {
            callback(index + 1, config.dataset.items.len());
        }

        // Execute with error handling
        match execute_item(&config.agent_yaml, dataset_item).await {
            Ok(output) => {
                let scores = calculate_scores(&config.metrics, &output, &dataset_item.expected_output);
                items.push(ItemResult { index, output, scores, /* ... */ });
            }
            Err(e) => {
                errors.push((index, e.to_string()));
                // Continue to next item
            }
        }
    }

    Ok(ExperimentResult {
        items,
        aggregates: calculate_aggregates(&items),
        errors,
        // ...
    })
}
```

## Testing

**Test File Location:** `rust/tests/test_experiments.rs` (new file)

**Test Patterns:**
- Use inline JSON datasets for testing
- Mock agent execution where possible
- Test metric calculations with known inputs
- Test aggregation with controlled scores

**Key Test Cases:**
```rust
#[test]
fn test_exact_match_metric() { /* ... */ }

#[test]
fn test_contains_match_metric() { /* ... */ }

#[test]
fn test_dataset_from_json() { /* ... */ }

#[tokio::test]
async fn test_experiment_runner_basic() { /* ... */ }

#[tokio::test]
async fn test_experiment_handles_failures() { /* ... */ }

#[test]
fn test_aggregate_calculations() { /* ... */ }
```

## Test Results

### Unit Tests (36 tests)
- `experiments::dataset::tests::*` - 9 tests passed
- `experiments::metrics::tests::*` - 17 tests passed
- `experiments::runner::tests::*` - 10 tests passed

### Integration Tests (17 tests)
- `test_experiments.rs` - 17 tests passed

**Total: 53 tests passing**

## Definition of Done

- [x] All acceptance criteria verified
- [x] All tasks completed
- [x] Experiment runner executes successfully
- [x] Built-in metrics work correctly
- [x] Error handling prevents cascading failures
- [x] Unit tests pass
- [x] Documentation complete

## QA Results

**Review Date:** 2026-01-13
**Reviewer:** Quinn (Test Architect)
**Gate Status:** PASS

### Risk Assessment

| Factor | Rating | Notes |
|--------|--------|-------|
| Complexity | Medium | New module with 4 submodules (mod.rs, runner.rs, metrics.rs, dataset.rs) |
| Blast Radius | Low | Self-contained experiments module, no changes to core engine |
| External Dependencies | Low | Only serde_json dependency (already in use) |
| Security Surface | Minimal | No external network calls, file I/O is read-only for datasets |

**Review Depth:** Standard (Medium risk warrants thorough code and test review)

### Requirements Traceability

| AC | Description | Implementation | Tests | Status |
|----|-------------|----------------|-------|--------|
| AC1 | Experiment Runner | `runner.rs:216-291` - `run_experiment()` | `test_experiment_runner_basic`, `test_run_experiment_basic` | PASS |
| AC2 | Dataset Loading | `dataset.rs:151-191` - `from_json_file()`, `from_json_str()` | `test_dataset_from_json`, `test_dataset_nested_json` | PASS |
| AC3 | Custom Metrics | `metrics.rs:93-108` - `Metric` trait | `test_custom_metric` | PASS |
| AC4 | Built-in Metrics | `metrics.rs:110-396` - ExactMatch, ContainsMatch, NumericTolerance, JsonPathMatch | 17 unit tests in metrics.rs | PASS |
| AC5 | Result Aggregation | `runner.rs:358-401` - `calculate_aggregates()` | `test_calculate_aggregates`, `test_aggregate_calculations` | PASS |
| AC6 | Error Handling | `runner.rs:258-280` - Error capture with continuation | `test_experiment_handles_failures`, `test_run_experiment_with_failure` | PASS |
| AC7 | Opik Integration | `runner.rs:222-279` - Trace context creation and child spans | `test_run_experiment_with_opik` (in unit tests) | PASS |
| AC8 | Progress Callback | `runner.rs:236-239` - Callback invocation | `test_experiment_with_progress_callback`, `test_run_experiment_with_progress` | PASS |
| AC9 | Async Support | `runner.rs:216` - `async fn run_experiment()` | `test_async_execution` | PASS |

**AC Coverage:** 9/9 (100%)

### Code Quality Review

| Aspect | Rating | Notes |
|--------|--------|-------|
| Documentation | Excellent | Module-level docs with examples, trait docs with usage patterns |
| Error Handling | Good | Uses `TeaError` consistently, graceful item failure handling |
| Type Safety | Excellent | Proper use of `Box<dyn Metric>`, generic builders |
| API Design | Excellent | Fluent builder pattern (`with_metric()`, `with_progress_callback()`) |
| Test Coverage | Good | 53 tests total (36 unit + 17 integration) |
| Code Organization | Excellent | Clean separation: dataset, metrics, runner |

**Issues Found:**
- None critical
- Minor: `truncate()` helper in metrics.rs could use `.chars()` for Unicode safety (P2)

### Test Architecture Assessment

| Category | Count | Coverage |
|----------|-------|----------|
| Unit Tests | 36 | Metrics: 17, Dataset: 9, Runner: 10 |
| Integration Tests | 17 | Full experiment flows |
| P0 Tests | 14 | Core functionality covered |
| P1 Tests | 18 | Extended scenarios covered |
| P2 Tests | 10 | Edge cases covered |

**Test Quality:**
- Proper async test setup (`#[tokio::test]`)
- Good use of mock agents for testing
- Progress callback testing with `AtomicUsize`
- Edge cases covered (empty dataset, invalid YAML, failing items)

### NFR Validation

| NFR | Status | Evidence |
|-----|--------|----------|
| Security | PASS | Read-only dataset loading, no external network exposure |
| Performance | PASS | Sequential item execution with per-item timing capture |
| Reliability | PASS | Graceful error handling, experiment continues after item failures |
| Maintainability | PASS | Well-documented trait with clear extension points |

### Standards Compliance

| Standard | Status | Notes |
|----------|--------|-------|
| Rust Naming Conventions | PASS | snake_case functions, PascalCase types |
| Module Exports | PASS | Clean re-exports in mod.rs |
| Error Types | PASS | Uses existing TeaError |
| Async Patterns | PASS | Proper async/await usage |
| Documentation | PASS | Docstrings with examples on public API |

### Test Design Document Alignment

Test design document: `docs/qa/assessments/44.3-experiment-framework-test-design-20260113.md`
- 42 scenarios defined, implementation has 53 tests
- All P0 scenarios implemented
- Risk coverage matrix addressed

### Final Assessment

**Gate Decision:** PASS

**Rationale:**
1. All 9 acceptance criteria fully implemented with test coverage
2. Clean, idiomatic Rust code following project conventions
3. Comprehensive test suite exceeding test design requirements
4. Documentation complete with examples
5. No security concerns - module is self-contained
6. API follows established project patterns (builder pattern, trait-based extensibility)

**Recommendations:**
- Future: Add E2E test with real Opik integration (currently mocked)
- Future: Consider parallel item execution for large datasets
- Minor: Use `.chars().take(n)` for Unicode-safe truncation in error messages

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-13 | 0.1 | Initial story draft | Sarah (PO Agent) |
| 2026-01-13 | 1.0 | Implementation complete | Claude (Dev Agent) |
| 2026-01-13 | 1.1 | QA Review complete - PASS | Quinn (Test Architect) |

