//! Performance benchmarks for The Edge Agent
//!
//! These benchmarks establish performance baselines for the Rust implementation.
//! Compare against Python baselines to verify ≥10x improvement target (AC-15).
//!
//! # Running benchmarks
//!
//! ```bash
//! cargo bench
//! ```
//!
//! # Python baseline (for comparison)
//!
//! See docs/stories/TEA-RUST-015-testing-documentation.md for Python baseline script.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use serde_json::json;
use std::collections::HashMap;

use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, StateGraph};
use the_edge_agent::engine::yaml::YamlEngine;

// ============================================================================
// Graph Construction Benchmarks
// ============================================================================

fn bench_graph_construction(c: &mut Criterion) {
    let mut group = c.benchmark_group("Graph Construction");

    // Simple graph: single node
    group.bench_function("single_node", |b| {
        b.iter(|| {
            let mut graph = StateGraph::new();
            graph.add_node(Node::new("process").with_run(|s| {
                let mut state = s.clone();
                state["result"] = json!("processed");
                Ok(state)
            }));
            graph.set_entry_point("process").unwrap();
            graph.set_finish_point("process").unwrap();
            black_box(graph.compile().unwrap())
        })
    });

    // Medium graph: 5 sequential nodes
    group.bench_function("five_sequential_nodes", |b| {
        b.iter(|| {
            let mut graph = StateGraph::new();
            for i in 0..5 {
                let step = i;
                graph.add_node(Node::new(&format!("node{}", i)).with_run(move |s| {
                    let mut state = s.clone();
                    state["step"] = json!(step);
                    Ok(state)
                }));
            }
            graph.set_entry_point("node0").unwrap();
            for i in 0..4 {
                graph
                    .add_simple_edge(&format!("node{}", i), &format!("node{}", i + 1))
                    .unwrap();
            }
            graph.set_finish_point("node4").unwrap();
            black_box(graph.compile().unwrap())
        })
    });

    // Large graph: 20 nodes with conditional routing
    group.bench_function("twenty_nodes_with_conditionals", |b| {
        b.iter(|| {
            let mut graph = StateGraph::new();

            // Add 20 nodes
            for i in 0..20 {
                graph.add_node(Node::new(&format!("node{}", i)).with_run(|s| Ok(s.clone())));
            }

            graph.set_entry_point("node0").unwrap();

            // Chain first 10 sequentially
            for i in 0..9 {
                graph
                    .add_simple_edge(&format!("node{}", i), &format!("node{}", i + 1))
                    .unwrap();
            }

            // Add conditional branch at node9
            let targets = HashMap::from([
                ("a".to_string(), "node10".to_string()),
                ("b".to_string(), "node15".to_string()),
            ]);
            graph
                .add_conditional_edge("node9", "state.branch or 'a'", targets)
                .unwrap();

            // Chain remaining nodes to finish
            for i in 10..14 {
                graph
                    .add_simple_edge(&format!("node{}", i), &format!("node{}", i + 1))
                    .unwrap();
            }
            for i in 15..19 {
                graph
                    .add_simple_edge(&format!("node{}", i), &format!("node{}", i + 1))
                    .unwrap();
            }

            graph.set_finish_point("node14").unwrap();
            graph.set_finish_point("node19").unwrap();

            black_box(graph.compile().unwrap())
        })
    });

    group.finish();
}

// ============================================================================
// Sequential Execution Benchmarks
// ============================================================================

fn bench_sequential_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("Sequential Execution");

    // Simple: 1 node, 1000 iterations
    group.bench_function("single_node_1000x", |b| {
        let mut graph = StateGraph::new();
        graph.add_node(Node::new("process").with_run(|s| {
            let mut state = s.clone();
            let val = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
            state["value"] = json!(val * 2);
            Ok(state)
        }));
        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            for _ in 0..1000 {
                let result = black_box(executor.invoke(json!({"value": 42})).unwrap());
                assert_eq!(result["value"], 84);
            }
        })
    });

    // Pipeline: 5 nodes, state transformation
    group.bench_function("five_node_pipeline", |b| {
        let mut graph = StateGraph::new();

        // Each node adds 10 to value
        for i in 0..5 {
            graph.add_node(Node::new(&format!("step{}", i)).with_run(|s| {
                let mut state = s.clone();
                let val = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
                state["value"] = json!(val + 10);
                Ok(state)
            }));
        }

        graph.set_entry_point("step0").unwrap();
        for i in 0..4 {
            graph
                .add_simple_edge(&format!("step{}", i), &format!("step{}", i + 1))
                .unwrap();
        }
        graph.set_finish_point("step4").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            let result = black_box(executor.invoke(json!({"value": 0})).unwrap());
            assert_eq!(result["value"], 50); // 5 nodes * 10 each
        })
    });

    // Conditional routing
    group.bench_function("conditional_routing", |b| {
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("router").with_run(|s| Ok(s.clone())));
        graph.add_node(Node::new("positive").with_run(|s| {
            let mut state = s.clone();
            state["result"] = json!("positive");
            Ok(state)
        }));
        graph.add_node(Node::new("negative").with_run(|s| {
            let mut state = s.clone();
            state["result"] = json!("negative");
            Ok(state)
        }));

        graph.set_entry_point("router").unwrap();

        let targets = HashMap::from([
            ("positive".to_string(), "positive".to_string()),
            ("negative".to_string(), "negative".to_string()),
        ]);
        graph
            .add_conditional_edge(
                "router",
                "state.value > 0 and 'positive' or 'negative'",
                targets,
            )
            .unwrap();

        graph.set_finish_point("positive").unwrap();
        graph.set_finish_point("negative").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            let result = black_box(executor.invoke(json!({"value": 42})).unwrap());
            assert_eq!(result["result"], "positive");
        })
    });

    group.finish();
}

// ============================================================================
// Cyclic Graph Benchmarks
// ============================================================================

fn bench_cyclic_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("Cyclic Execution");

    // Loop until counter reaches 10
    group.bench_function("loop_10_iterations", |b| {
        let mut graph = StateGraph::new().allow_cycles().with_max_iterations(100);

        graph.add_node(Node::new("increment").with_run(|s| {
            let mut state = s.clone();
            let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
            state["count"] = json!(count + 1);
            Ok(state)
        }));
        graph.add_node(Node::new("done").with_run(|s| Ok(s.clone())));

        graph.set_entry_point("increment").unwrap();

        let targets = HashMap::from([
            ("done".to_string(), "done".to_string()),
            ("loop".to_string(), "increment".to_string()),
        ]);
        graph
            .add_conditional_edge(
                "increment",
                "state.count >= 10 and 'done' or 'loop'",
                targets,
            )
            .unwrap();

        graph.set_finish_point("done").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            let result = black_box(executor.invoke(json!({"count": 0})).unwrap());
            assert_eq!(result["count"], 10);
        })
    });

    // Loop until counter reaches 100
    group.bench_function("loop_100_iterations", |b| {
        let mut graph = StateGraph::new().allow_cycles().with_max_iterations(200);

        graph.add_node(Node::new("increment").with_run(|s| {
            let mut state = s.clone();
            let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
            state["count"] = json!(count + 1);
            Ok(state)
        }));
        graph.add_node(Node::new("done").with_run(|s| Ok(s.clone())));

        graph.set_entry_point("increment").unwrap();

        let targets = HashMap::from([
            ("done".to_string(), "done".to_string()),
            ("loop".to_string(), "increment".to_string()),
        ]);
        graph
            .add_conditional_edge(
                "increment",
                "state.count >= 100 and 'done' or 'loop'",
                targets,
            )
            .unwrap();

        graph.set_finish_point("done").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            let result = black_box(executor.invoke(json!({"count": 0})).unwrap());
            assert_eq!(result["count"], 100);
        })
    });

    group.finish();
}

// ============================================================================
// Large State Benchmarks
// ============================================================================

fn bench_large_state(c: &mut Criterion) {
    let mut group = c.benchmark_group("Large State");

    // State with 100 keys
    group.bench_function("state_100_keys", |b| {
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("process").with_run(|s| {
            let mut state = s.clone();
            state["processed"] = json!(true);
            Ok(state)
        }));

        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        // Create large initial state
        let mut state_map = serde_json::Map::new();
        for i in 0..100 {
            state_map.insert(format!("key_{}", i), json!(format!("value_{}", i)));
        }
        let initial_state = serde_json::Value::Object(state_map);

        b.iter(|| {
            let result = black_box(executor.invoke(initial_state.clone()).unwrap());
            assert_eq!(result["processed"], true);
        })
    });

    // State with nested objects
    group.bench_function("state_nested_objects", |b| {
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("process").with_run(|s| {
            let mut state = s.clone();
            state["processed"] = json!(true);
            Ok(state)
        }));

        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        // Create nested state
        let initial_state = json!({
            "level1": {
                "level2": {
                    "level3": {
                        "data": [1, 2, 3, 4, 5],
                        "nested": {
                            "deep": "value"
                        }
                    }
                }
            },
            "array": (0..50).collect::<Vec<i32>>(),
            "strings": ["alpha", "beta", "gamma", "delta"],
        });

        b.iter(|| {
            let result = black_box(executor.invoke(initial_state.clone()).unwrap());
            assert_eq!(result["processed"], true);
        })
    });

    group.finish();
}

// ============================================================================
// Streaming Benchmarks
// ============================================================================

fn bench_streaming(c: &mut Criterion) {
    let mut group = c.benchmark_group("Streaming");

    // Stream with 5 nodes
    group.bench_function("stream_5_nodes", |b| {
        let mut graph = StateGraph::new();

        for i in 0..5 {
            let step_name = format!("step{}", i);
            let step_key = step_name.clone();
            graph.add_node(Node::new(&step_name).with_run(move |s| {
                let mut state = s.clone();
                state[&step_key] = json!(true);
                Ok(state)
            }));
        }

        graph.set_entry_point("step0").unwrap();
        for i in 0..4 {
            graph
                .add_simple_edge(&format!("step{}", i), &format!("step{}", i + 1))
                .unwrap();
        }
        graph.set_finish_point("step4").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        b.iter(|| {
            let events: Vec<_> = black_box(executor.stream(json!({})).unwrap().collect());
            // At least 5 complete events + 1 finish
            assert!(events.len() >= 6);
        })
    });

    group.finish();
}

// ============================================================================
// Template Caching Benchmarks (TEA-RUST-018)
// ============================================================================

fn bench_template_caching(c: &mut Criterion) {
    let mut group = c.benchmark_group("Template Caching");

    // Benchmark: First render (cold cache)
    group.bench_function("template_cold_cache", |b| {
        b.iter_with_setup(YamlEngine::new, |engine| {
            let template = "Hello, {{ state.name }}!";
            let state = json!({"name": "World"});
            black_box(
                engine
                    .render_template(template, &state, &HashMap::new())
                    .unwrap(),
            )
        })
    });

    // Benchmark: Repeated render (warm cache) vs cold
    group.bench_function("template_warm_cache_1000x", |b| {
        let engine = YamlEngine::new();
        let template = "Hello, {{ state.name }}!";
        let state = json!({"name": "World"});

        // Warm up the cache
        engine.render_template(template, &state, &HashMap::new()).unwrap();

        b.iter(|| {
            for _ in 0..1000 {
                black_box(engine.render_template(template, &state, &HashMap::new()).unwrap());
            }
        })
    });

    // Benchmark: Complex template with loops
    group.bench_function("template_complex_cached_100x", |b| {
        let engine = YamlEngine::new();
        let template = r#"
            {% for item in state.items %}
            - {{ item.name }}: {{ item.value }}
            {% endfor %}
        "#;
        let state = json!({
            "items": (0..10).map(|i| json!({"name": format!("item{}", i), "value": i})).collect::<Vec<_>>()
        });

        // Warm up the cache
        engine.render_template(template, &state, &HashMap::new()).unwrap();

        b.iter(|| {
            for _ in 0..100 {
                black_box(engine.render_template(template, &state, &HashMap::new()).unwrap());
            }
        })
    });

    // Benchmark: Multiple different templates (cache growth)
    group.bench_function("template_multiple_unique", |b| {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});

        b.iter(|| {
            for i in 0..50 {
                let template = format!("Template {} - Hello, {{{{ state.name }}}}!", i);
                black_box(engine.render_template(&template, &state, &HashMap::new()).unwrap());
            }
            // All 50 templates should be cached
            assert_eq!(engine.cache_size(), 50);
        })
    });

    // Benchmark: With variables (full context)
    group.bench_function("template_full_context_cached", |b| {
        let engine = YamlEngine::new();

        let template = "User: {{ state.user }}, Model: {{ variables.model }}";
        let state = json!({"user": "alice"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        // Warm up cache
        engine.render_template(template, &state, &variables).unwrap();

        b.iter(|| {
            for _ in 0..100 {
                black_box(engine.render_template(template, &state, &variables).unwrap());
            }
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_graph_construction,
    bench_sequential_execution,
    bench_cyclic_execution,
    bench_large_state,
    bench_streaming,
    bench_template_caching,
);

criterion_main!(benches);
