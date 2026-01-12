//! Performance benchmarks for Scryer Prolog backend
//!
//! TEA-RELEASE-005.1: Scryer Prolog Spike
//!
//! # Running benchmarks
//!
//! ```bash
//! # Run Scryer benchmarks
//! cargo bench --features scryer scryer
//! ```

#![cfg(feature = "scryer")]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_json::json;

use the_edge_agent::prolog::ScryerRuntime;

// ============================================================================
// Scryer Prolog Benchmarks
// ============================================================================

fn bench_scryer_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("Scryer Runtime");

    // Measure runtime creation overhead
    group.bench_function("create_runtime", |b| {
        b.iter(|| {
            let runtime = ScryerRuntime::new().unwrap();
            black_box(runtime)
        })
    });

    group.finish();
}

fn bench_scryer_simple_query(c: &mut Criterion) {
    let mut group = c.benchmark_group("Scryer Simple Queries");

    let runtime = ScryerRuntime::new().unwrap();

    // Simple arithmetic
    group.bench_function("arithmetic", |b| {
        b.iter(|| {
            let result = runtime.query("X is 21 * 2.");
            black_box(result)
        })
    });

    // Unification
    group.bench_function("unification", |b| {
        b.iter(|| {
            let result = runtime.query("X = hello.");
            black_box(result)
        })
    });

    // List length
    group.bench_function("list_length", |b| {
        b.iter(|| {
            let result = runtime.query("length([1,2,3,4,5], N).");
            black_box(result)
        })
    });

    group.finish();
}

fn bench_scryer_execute_node_code(c: &mut Criterion) {
    let mut group = c.benchmark_group("Scryer Node Execution");

    let runtime = ScryerRuntime::new().unwrap();
    let state = json!({"value": 21});
    let code = r#"
        state(value, V),
        D is V * 2,
        return(doubled, D).
    "#;

    // Node code execution (the main use case)
    group.bench_function("double_value", |b| {
        b.iter(|| {
            let result = runtime.execute_node_code(code, &state);
            black_box(result)
        })
    });

    // Multiple state variables
    let multi_state = json!({"a": 10, "b": 20, "c": 30});
    let multi_code = r#"
        state(a, A),
        state(b, B),
        state(c, C),
        Sum is A + B + C,
        return(sum, Sum).
    "#;

    group.bench_function("multi_state", |b| {
        b.iter(|| {
            let result = runtime.execute_node_code(multi_code, &multi_state);
            black_box(result)
        })
    });

    group.finish();
}

fn bench_scryer_json_conversion(c: &mut Criterion) {
    let mut group = c.benchmark_group("Scryer JSON Conversion");

    let runtime = ScryerRuntime::new().unwrap();

    // JSON to Prolog conversion
    group.bench_function("json_to_prolog_simple", |b| {
        let value = json!(42);
        b.iter(|| {
            let result = runtime.json_to_prolog(&value);
            black_box(result)
        })
    });

    group.bench_function("json_to_prolog_list", |b| {
        let value = json!([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
        b.iter(|| {
            let result = runtime.json_to_prolog(&value);
            black_box(result)
        })
    });

    group.bench_function("json_to_prolog_object", |b| {
        let value = json!({"name": "Alice", "age": 30, "active": true});
        b.iter(|| {
            let result = runtime.json_to_prolog(&value);
            black_box(result)
        })
    });

    // Prolog to JSON conversion
    group.bench_function("prolog_to_json_number", |b| {
        b.iter(|| {
            let result = runtime.prolog_to_json("42");
            black_box(result)
        })
    });

    group.bench_function("prolog_to_json_list", |b| {
        b.iter(|| {
            let result = runtime.prolog_to_json("[1, 2, 3, 4, 5]");
            black_box(result)
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_scryer_creation,
    bench_scryer_simple_query,
    bench_scryer_execute_node_code,
    bench_scryer_json_conversion
);

criterion_main!(benches);
