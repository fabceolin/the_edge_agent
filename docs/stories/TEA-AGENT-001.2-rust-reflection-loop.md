# Story TEA-AGENT-001.2-rust: Reflection Loop (Rust/Embedded)

## Status

**Ready for Development**

*Updated: 2026-01-05 - QA Gate passed with all acceptance criteria covered by test design. See `docs/qa/assessments/TEA-AGENT-001.2-rust-test-design-20260105.md` for full assessment.*

## Story

**As a** developer deploying self-correcting agents to edge environments,
**I want** a built-in reflection loop primitive in the Rust runtime,
**so that** agents can iteratively improve their outputs without external Python dependencies.

## Background

This is the Rust adaptation of TEA-AGENT-001.2, optimized for embedded/offline execution. The reflection pattern is **fully portable** to Rust as it's a pure logic pattern.

| Aspect | Python Version | Rust Version |
|--------|---------------|--------------|
| **Evaluators** | Python exec, JSON Schema | Lua, JSON Schema (jsonschema-rs) |
| **LLM Evaluator** | Any provider | Ollama, OpenAI-compatible only |
| **Custom Logic** | Python inline | Lua 5.4 inline |
| **Circuit Breaker** | Python class | Rust struct with atomics |

## Scope

### In Scope
- `reflection.loop` - Full generate→evaluate→correct cycle
- Schema evaluator via `jsonschema-rs` crate
- LLM evaluator via Ollama/OpenAI-compatible
- Custom evaluator via Lua scripts
- Circuit breaker with atomic state
- All on-failure strategies

### Out of Scope
- Python-based custom evaluators

## Acceptance Criteria

### AC1: `reflection.loop` Action (Rust Native)
1. Executes generate→evaluate→correct cycle
2. Generator: Any Rust-native action or Lua code
3. Evaluator: `schema`, `llm`, or `lua` type
4. Corrector: Any Rust-native action or Lua code
5. Returns best/last result based on strategy

### AC2: Schema Evaluator (jsonschema-rs)
1. JSON Schema validation via `jsonschema` crate
2. Returns detailed validation errors with JSON paths
3. Supports JSON Schema Draft 7 (embedded-friendly, no network)
4. Schema can be inline or loaded from file

### AC3: LLM Evaluator (Ollama/OpenAI-compatible)
1. Uses LLM to evaluate output quality
2. Configurable evaluation prompt (Tera templated)
3. Structured response parsing: `{pass: bool, score: f64, feedback: str}`
4. Model from `settings.llm` or override

### AC4: Lua Evaluator
1. Custom evaluation logic in Lua 5.4
2. Lua script receives: `state`, `output`, `iteration`
3. Must return: `{valid = bool, score = number, errors = table}`
4. Sandboxed execution (no file/network access)

### AC5: Iteration Tracking
1. State includes `reflection_iteration: u32`
2. State includes `reflection_history: Vec<Attempt>`
3. State includes `reflection_errors: Vec<String>`
4. All tracking uses stack allocation where possible

### AC6: Circuit Breaker (Atomic)
1. `max_iterations` enforced via `AtomicU32`
2. Circuit opens after max_iterations reached
3. No infinite loops possible (compile-time guarantee)

### AC7: On-Failure Strategies
1. `return_best`: Return highest-scoring attempt
2. `return_last`: Return final attempt
3. `raise`: Return `ReflectionError` with full history
4. Strategy is enum (exhaustive match)

### AC8: Standalone Actions
1. `reflection.evaluate` - Evaluate without loop
2. `reflection.correct` - Correct without loop
3. Composable for custom reflection patterns

### AC9: Feature Flag
1. Actions behind `--features reflection` cargo flag
2. `jsonschema` crate only included when enabled

## Tasks / Subtasks

- [ ] **Task 1: Core Reflection Loop** (AC: 1, 5, 6)
  - [ ] Define `ReflectionConfig` struct
  - [ ] Implement `reflection_loop` function
  - [ ] Iteration tracking with state updates
  - [ ] Circuit breaker with AtomicU32
  - [ ] Unit tests

- [ ] **Task 2: Schema Evaluator** (AC: 2)
  - [ ] Integrate `jsonschema` crate
  - [ ] Implement `SchemaEvaluator` struct
  - [ ] Error path extraction
  - [ ] Schema loading (inline/file)
  - [ ] Unit tests

- [ ] **Task 3: LLM Evaluator** (AC: 3)
  - [ ] Implement `LlmEvaluator` struct
  - [ ] Prompt templating via Tera
  - [ ] Response parsing
  - [ ] Integration with Ollama provider
  - [ ] Unit tests with mock

- [ ] **Task 4: Lua Evaluator** (AC: 4)
  - [ ] Implement `LuaEvaluator` struct
  - [ ] Lua sandbox configuration
  - [ ] State/output injection
  - [ ] Result extraction
  - [ ] Unit tests

- [ ] **Task 5: On-Failure Strategies** (AC: 7)
  - [ ] Define `OnFailure` enum
  - [ ] Implement `return_best` with scoring
  - [ ] Implement `return_last`
  - [ ] Implement `raise` with history
  - [ ] Unit tests

- [ ] **Task 6: Standalone Actions** (AC: 8)
  - [ ] Implement `reflection.evaluate` action
  - [ ] Implement `reflection.correct` action
  - [ ] Register in actions registry
  - [ ] Unit tests

- [ ] **Task 7: Feature Flag & Integration** (AC: 9)
  - [ ] Add `reflection` feature to Cargo.toml
  - [ ] Conditional compilation
  - [ ] Integration tests

## Dev Notes

### Source Tree

```
rust/src/
├── engine/
│   ├── actions/
│   │   └── reflection.rs     # NEW: Reflection actions
│   └── evaluators/
│       ├── mod.rs
│       ├── schema.rs         # JSON Schema evaluator
│       ├── llm.rs            # LLM-as-judge evaluator
│       └── lua.rs            # Lua custom evaluator
```

### YAML Syntax

```yaml
nodes:
  - name: generate_json
    action: reflection.loop
    with:
      generator:
        action: llm.call
        prompt: "Generate valid JSON for: {{ state.request }}"

      evaluator:
        type: schema
        schema:
          type: object
          required: [name, email]
          properties:
            name: { type: string, minLength: 1 }
            email: { type: string, format: email }

      corrector:
        action: llm.call
        prompt: |
          Fix this JSON. Errors: {{ state.reflection_errors | json }}
          Original: {{ state.reflection_output }}

      max_iterations: 3
      on_failure: return_best
```

### Lua Evaluator Example

```yaml
evaluator:
  type: lua
  code: |
    -- Custom validation logic
    local output = state.reflection_output
    local valid = output.name ~= nil and #output.name > 0
    local score = valid and 1.0 or 0.0
    local errors = {}

    if not valid then
      table.insert(errors, "name is required")
    end

    return {
      valid = valid,
      score = score,
      errors = errors
    }
```

### Rust Types

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReflectionConfig {
    pub generator: ActionConfig,
    pub evaluator: EvaluatorConfig,
    pub corrector: ActionConfig,
    pub max_iterations: u32,
    pub on_failure: OnFailure,
}

#[derive(Debug, Clone)]
pub enum EvaluatorConfig {
    Schema { schema: serde_json::Value },
    Llm { prompt: String, model: Option<String> },
    Lua { code: String },
}

#[derive(Debug, Clone, Copy)]
pub enum OnFailure {
    ReturnBest,
    ReturnLast,
    Raise,
}

#[derive(Debug, Clone, Serialize)]
pub struct Attempt {
    pub iteration: u32,
    pub output: serde_json::Value,
    pub score: f64,
    pub errors: Vec<String>,
}
```

### Dependencies

```toml
[dependencies]
jsonschema = "0.17"  # JSON Schema validation
mlua = { version = "0.9", features = ["lua54", "vendored"] }

[features]
reflection = ["jsonschema"]
```

### Related Stories
- TEA-RUST-001: Rust Migration Epic
- TEA-AGENT-001.2: Python version (reference)
- TEA-RUST-009: Lua Integration

## Testing

### Test File Location
- `rust/tests/test_reflection_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Core Loop | 8 | P0 |
| Schema Evaluator | 6 | P0 |
| LLM Evaluator | 4 | P1 |
| Lua Evaluator | 6 | P0 |
| On-Failure | 4 | P0 |
| Circuit Breaker | 4 | P0 |

### Key Test Scenarios

1. **First-pass success** - Generator passes immediately
2. **Correction success** - Fixed within max_iterations
3. **Circuit breaker** - Stops at max_iterations exactly
4. **Schema errors** - JSON path in error message
5. **Lua sandbox** - File access blocked
6. **return_best** - Highest score returned

## QA Notes

**Assessment Date:** 2026-01-05
**Assessor:** Quinn (Test Architect)
**Test Design Reference:** `docs/qa/assessments/TEA-AGENT-001.2-rust-test-design-20260105.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 52 |
| Unit Tests | 36 (69%) |
| Integration Tests | 12 (23%) |
| E2E Tests | 4 (8%) |
| P0 (Critical) | 32 |
| P1 (Important) | 14 |
| P2 (Nice-to-have) | 6 |

**Coverage Status:** All 9 Acceptance Criteria have dedicated test scenarios with no coverage gaps identified.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Infinite Loop** | Critical | Circuit breaker with AtomicU32 (tests 035-038, INT-008) |
| **Lua Sandbox Escape** | Critical | 3 security-specific tests blocking io, os, require (tests 025-027) |
| **Thread Safety** | High | Concurrent access test for AtomicU32 counter (test 036) |
| **Data Loss on Failure** | High | History/error accumulation tests (tests 031, 032, 043) |
| **LLM Response Parsing** | Medium | Graceful handling of malformed responses (tests 017, 019) |
| **Checkpoint Corruption** | Medium | State persistence across save/resume (INT-007) |

### Recommended Test Scenarios

**P0 - Must Have Before Merge:**
1. Circuit breaker stops at exactly max_iterations (prevents infinite loops)
2. Lua sandbox blocks file access (`io.open`), os module, and arbitrary `require`
3. Schema evaluator returns accurate JSON paths in error messages
4. `return_best` selects highest-scoring attempt with deterministic tie-breaking
5. Iteration tracking state persists across checkpoint save/resume

**P1 - Should Have:**
1. LLM evaluator with mocked Ollama endpoint
2. Lua syntax/runtime errors return helpful messages
3. Feature flag correctly excludes jsonschema dependency

**P2 - Optional:**
1. Full E2E with real LLM evaluation
2. Custom reflection patterns using standalone actions

### Concerns / Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| **Dependency** | `mlua` crate with `vendored` feature increases binary size | Accept for portability; document size impact |
| **Testing Gap** | No performance benchmarks for reflection loop overhead | Add benchmark in future iteration (not blocking) |
| **Security Review** | Lua sandbox relies on mlua's sandboxing - needs security audit | Request security review of mlua sandbox configuration before production |

### QA Gate Readiness

- **Status:** PASS with advisory notes
- **Rationale:** All ACs have test coverage, security-critical paths have P0 tests, thread safety is explicitly tested
- **Advisory:** Recommend security audit of Lua sandbox configuration before first production deployment

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial Rust adaptation from TEA-AGENT-001.2 | Sarah (PO) |
