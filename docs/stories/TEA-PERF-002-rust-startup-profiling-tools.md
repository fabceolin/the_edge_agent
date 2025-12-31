# Story TEA-PERF-002: Rust Startup Profiling Tools

## Status

Approved

## Story

**As a** developer/maintainer,
**I want** tools to profile and analyze the startup time of the Rust tea CLI binary,
**so that** I can identify initialization bottlenecks, slow feature loading, and optimization opportunities for cold-start performance.

## Story Context

**Existing System Integration:**

- Integrates with: `rust/src/bin/tea.rs` (clap-based CLI entry point)
- Technology: Rust, Cargo, clap, mlua, swipl (optional), tokio
- Follows pattern: Existing benchmark infrastructure (`rust/benches/`)
- Touch points: CLI entry point, runtime initialization (Lua, Prolog), action registry, HTTP client

**Current Initialization Chain (to be profiled):**

```
main() [tea.rs]
 └─> Cli::parse() [clap]
 └─> tracing_subscriber init
 └─> YamlEngine::new()
      └─> Tera template engine init
 └─> ActionRegistry::new()
      └─> actions::register_defaults()
 └─> Executor::with_actions()
      └─> LuaRuntime (lazy init on first use)
      └─> PrologRuntime (lazy init, feature-gated)
 └─> reqwest client (TLS init)
```

**Build Profile Impact:**
- Release build: LTO enabled, optimized for size (`opt-level = "z"`)
- Feature flags: `default = ["memory", "trace", "data", "llm"]`
- Optional features: `prolog`, `ltm-duckdb`, `graph`

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1**: Create a benchmark `rust/benches/startup_bench.rs` that measures CLI cold-start time
2. **AC-2**: Measure initialization time for individual subsystems (Lua runtime, action registry, template engine)
3. **AC-3**: Script to measure binary startup via wall-clock timing (`time tea --version`)
4. **AC-4**: Support for comparing startup across feature flag combinations
5. **AC-5**: Support for `--baseline` and `--compare` modes for regression detection
6. **AC-6**: Output formats: text (human-readable), JSON (for CI), CSV (for analysis)

**Integration Requirements:**

7. Benchmarks integrate with existing `rust/benches/graph_benchmarks.rs` structure
8. Profiling tools are development-only (in `dev-dependencies`)
9. Works on Linux, macOS (both x86_64 and ARM64)

**Quality Requirements:**

10. Benchmarks are reproducible (low variance between runs)
11. Documentation for interpreting profiling results
12. CI integration for tracking startup time trends

## Technical Notes

### Profiling Approaches

**1. Criterion Benchmarks (Micro-benchmarks):**
```rust
// rust/benches/startup_bench.rs
use criterion::{criterion_group, criterion_main, Criterion};
use the_edge_agent::engine::yaml::YamlEngine;

fn bench_yaml_engine_init(c: &mut Criterion) {
    c.bench_function("yaml_engine_new", |b| {
        b.iter(|| YamlEngine::new())
    });
}

fn bench_action_registry_init(c: &mut Criterion) {
    c.bench_function("action_registry_new", |b| {
        b.iter(|| {
            let registry = ActionRegistry::new();
            actions::register_defaults(&registry);
        })
    });
}

fn bench_lua_runtime_init(c: &mut Criterion) {
    c.bench_function("lua_runtime_new", |b| {
        b.iter(|| LuaRuntime::new())
    });
}

criterion_group!(
    startup_benches,
    bench_yaml_engine_init,
    bench_action_registry_init,
    bench_lua_runtime_init,
);
criterion_main!(startup_benches);
```

**2. Binary Startup Script:**
```bash
#!/bin/bash
# scripts/profile_rust_startup.sh

BINARY="${1:-./target/release/tea}"
RUNS="${2:-10}"

echo "=== Rust Tea CLI Startup Profile ==="
echo "Binary: $BINARY"
echo "Runs: $RUNS"
echo ""

# Warm up filesystem cache
$BINARY --version > /dev/null 2>&1

# Measure startup time
times=()
for i in $(seq 1 $RUNS); do
    start=$(date +%s%N)
    $BINARY --version > /dev/null 2>&1
    end=$(date +%s%N)
    elapsed=$(( (end - start) / 1000000 ))  # ms
    times+=($elapsed)
done

# Calculate stats
sum=0
for t in "${times[@]}"; do sum=$((sum + t)); done
avg=$((sum / RUNS))
echo "Average startup time: ${avg}ms"
```

**3. Flamegraph Profiling (Linux):**
```bash
# Install perf and cargo-flamegraph
cargo install flamegraph

# Generate flamegraph
cargo flamegraph --bin tea -- --version

# Or with perf directly
perf record --call-graph dwarf ./target/release/tea --version
perf report
```

**4. Feature Flag Comparison Matrix:**
```bash
# Compare startup with different feature combinations
for features in "default" "memory,trace" "memory,trace,llm" "all"; do
    cargo build --release --features "$features"
    echo "Features: $features"
    time ./target/release/tea --version
done
```

### Expected Bottleneck Areas

| Component | Expected Impact | Notes |
|-----------|-----------------|-------|
| `mlua` init | Medium-High | Lua VM initialization |
| `swipl` init | High (if enabled) | Prolog engine load |
| `reqwest` TLS | Low-Medium | rustls certificate loading |
| `tera` init | Low | Template engine parse |
| `tracing_subscriber` | Low | Logging setup |
| `clap` parse | Very Low | Argument parsing |

### Key Constraints

- Benchmarks must use `criterion` for statistical reliability
- Avoid measuring I/O-bound operations (file reads) in micro-benchmarks
- Profile both debug and release builds
- Consider static vs dynamic linking impact

### Implementation Guidance

**Benchmark Structure:**
```rust
// rust/benches/startup_bench.rs
use criterion::{criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Duration;

fn bench_full_startup(c: &mut Criterion) {
    let mut group = c.benchmark_group("startup");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(50);

    group.bench_function("yaml_engine_new", |b| {
        b.iter(|| {
            let engine = YamlEngine::new();
            std::hint::black_box(engine)
        })
    });

    group.finish();
}
```

**Profiling Script:**
```python
#!/usr/bin/env python3
# scripts/profile_rust_startup.py

import argparse
import subprocess
import json
import statistics
from pathlib import Path

def measure_startup(binary: str, runs: int = 10) -> dict:
    """Measure binary startup time."""
    times = []
    for _ in range(runs):
        result = subprocess.run(
            ["time", "-v", binary, "--version"],
            capture_output=True,
            text=True
        )
        # Parse wall-clock time from time output
        # ...

    return {
        "mean_ms": statistics.mean(times),
        "stdev_ms": statistics.stdev(times),
        "min_ms": min(times),
        "max_ms": max(times),
        "runs": runs
    }

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary", default="./target/release/tea")
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--format", choices=["text", "json", "csv"], default="text")
    parser.add_argument("--baseline", help="Save results as baseline")
    parser.add_argument("--compare", help="Compare against baseline")
    # ...
```

### Output Example

```
=== Tea CLI (Rust) Startup Profile ===
Binary: ./target/release/tea
Features: default (memory, trace, data, llm)
Runs: 50 (criterion-style)

Subsystem Initialization:
  yaml_engine_new     :   1.23 ms ±  0.05 ms
  action_registry_new :   0.45 ms ±  0.02 ms
  lua_runtime_new     :   8.76 ms ±  0.12 ms  [SLOWEST]
  tera_init           :   0.31 ms ±  0.01 ms

Binary Startup (wall-clock):
  tea --version       :  12.4 ms ±  0.8 ms

Feature Flag Impact:
  default             :  12.4 ms
  +prolog             :  45.2 ms (+264%)  [Prolog VM load]
  +ltm-duckdb         :  18.7 ms (+51%)
  all                 :  52.1 ms (+320%)

Recommendations:
  - Lua runtime init dominates startup (71% of subsystem time)
  - Consider lazy-init for LuaRuntime (only when first Lua node runs)
  - Prolog feature adds significant overhead - keep optional
```

## Tasks / Subtasks

- [ ] **Task 1: Create Criterion benchmark file** (AC: 1, 2)
  - [ ] Create `rust/benches/startup_bench.rs`
  - [ ] Add benchmark for `YamlEngine::new()`
  - [ ] Add benchmark for `ActionRegistry::new()` + `register_defaults()`
  - [ ] Add benchmark for `LuaRuntime::new()`
  - [ ] Add to `Cargo.toml` bench configuration

- [ ] **Task 2: Create wall-clock profiling script** (AC: 3)
  - [ ] Create `scripts/profile_rust_startup.sh` (bash)
  - [ ] Create `scripts/profile_rust_startup.py` (Python, more features)
  - [ ] Support multiple runs with statistical output
  - [ ] Cross-platform support (Linux, macOS)

- [ ] **Task 3: Implement feature flag comparison** (AC: 4)
  - [ ] Script to build with different feature combinations
  - [ ] Measure startup for each combination
  - [ ] Generate comparison matrix

- [ ] **Task 4: Implement baseline/compare modes** (AC: 5)
  - [ ] Save baseline to `.tea-rust-baseline.json`
  - [ ] Compare current run against baseline
  - [ ] Show diff with percentage change
  - [ ] CI integration for regression detection

- [ ] **Task 5: Implement output formats** (AC: 6)
  - [ ] Text format (human-readable)
  - [ ] JSON format (for CI/tooling)
  - [ ] CSV format (for spreadsheet analysis)

- [ ] **Task 6: Add flamegraph support** (AC: 2)
  - [ ] Document flamegraph generation process
  - [ ] Add `release-with-debug` profile instructions
  - [ ] Script to generate and open flamegraph

- [ ] **Task 7: Documentation** (AC: 11)
  - [ ] Add usage section to `docs/rust/development-guide.md`
  - [ ] Document how to interpret criterion output
  - [ ] Document flamegraph analysis

## Dev Notes

### Source Tree Reference

```
rust/
├── Cargo.toml                # Package manifest with features
├── src/
│   ├── lib.rs                # Library exports
│   ├── bin/
│   │   └── tea.rs            # CLI entry point
│   └── engine/
│       ├── mod.rs
│       ├── yaml.rs           # YamlEngine (Tera init)
│       ├── executor.rs       # Executor, ActionRegistry
│       ├── lua_runtime.rs    # LuaRuntime (mlua init)
│       └── prolog_runtime.rs # PrologRuntime (optional, swipl)
├── benches/
│   ├── graph_benchmarks.rs   # Existing benchmarks
│   └── startup_bench.rs      # NEW: Startup benchmarks
└── scripts/                   # NEW: Profiling scripts
    ├── profile_rust_startup.sh
    └── profile_rust_startup.py
```

### Key Dependencies & Impact

| Dependency | Version | Impact on Startup | Notes |
|------------|---------|-------------------|-------|
| `mlua` | 0.9 | High | Vendored Lua 5.4, VM init |
| `swipl` | 0.3 | Very High | SWI-Prolog engine, optional |
| `reqwest` + `rustls` | 0.12 | Medium | TLS certificate loading |
| `tera` | 1.19 | Low | Template parser init |
| `clap` | 4.5 | Very Low | Arg parsing |
| `tokio` | 1 | Low | Async runtime (lazy) |

### Build Profiles

```toml
# Already in Cargo.toml
[profile.release]
lto = true           # Link-time optimization
codegen-units = 1    # Single codegen unit (slower build, faster binary)
strip = true         # Strip debug symbols
opt-level = "z"      # Optimize for size

[profile.release-with-debug]
inherits = "release"
debug = true         # Keep debug info for flamegraph
strip = false
```

### Testing

- **Benchmark location**: `rust/benches/startup_bench.rs`
- **Test framework**: Criterion 0.5
- **Run command**: `cargo bench --bench startup_bench`

## Definition of Done

- [ ] Criterion benchmarks for subsystem initialization
- [ ] Wall-clock profiling script (bash + Python)
- [ ] Feature flag comparison matrix
- [ ] Baseline/compare functionality
- [ ] Flamegraph documentation
- [ ] Integration with existing bench infrastructure
- [ ] Documentation updated

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk**: Benchmark results may vary across machines
- **Mitigation**: Use criterion's statistical methods, require multiple runs
- **Rollback**: Benchmarks are dev-only, can be removed without impact

**Compatibility Verification:**

- [ ] No changes to production code
- [ ] Only `dev-dependencies` added
- [ ] No breaking changes to build process

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-28 | 0.1 | Initial draft | PO Agent (Sarah) |
