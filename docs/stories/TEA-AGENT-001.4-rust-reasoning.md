# Story TEA-AGENT-001.4-rust: Reasoning Techniques (Rust/Embedded)

## Status

**Ready for Development**

## Story

**As a** developer deploying reasoning agents to edge environments,
**I want** built-in reasoning technique primitives in the Rust runtime,
**so that** agents can use Chain-of-Thought and ReAct patterns without Python dependencies.

## Background

This is the Rust adaptation of TEA-AGENT-001.4, optimized for embedded/offline execution. Core reasoning patterns are **fully portable**, but DSPy integration is excluded.

| Aspect | Python Version | Rust Version |
|--------|---------------|--------------|
| **CoT** | Full support | Full support |
| **ReAct** | Full + all tool bridges | Rust-native tools only |
| **Self-Correct** | Full support | Full support |
| **Decompose** | Full support | Full support |
| **DSPy** | `reason.dspy.*` actions | NOT SUPPORTED |

## Scope

### In Scope
- `reason.cot` - Chain-of-Thought prompting
- `reason.react` - ReAct loop with Rust-native tools
- `reason.self_correct` - Generate-critique-improve cycle
- `reason.decompose` - Problem decomposition
- Structured output parsing
- Reasoning trace for observability

### Out of Scope (Python-only)
- `reason.dspy.cot` - DSPy ChainOfThought
- `reason.dspy.react` - DSPy ReAct
- `reason.dspy.compile` - DSPy compilation
- MCP/CrewAI/LangChain tools in ReAct

## Acceptance Criteria

### AC1: `reason.cot` Action (Chain-of-Thought)
1. Wraps LLM call with CoT prompting
2. Structured output: `{thinking: String, answer: Value}`
3. Few-shot examples support (inline in YAML)
4. Thinking formats: `step_by_step`, `pros_cons`, `tree`
5. Output parsed via JSON or custom delimiter

### AC2: `reason.react` Action
1. Implements Thought→Action→Observation loop
2. Tools: Rust-native actions only (no MCP/CrewAI)
3. `max_steps` enforced (prevents infinite loops)
4. Early termination on goal achieved
5. Returns full trace and final answer

### AC3: `reason.self_correct` Action
1. Generate→Critique→Improve cycle
2. Configurable improvement rounds
3. Same or different models for generate/critique
4. Returns final output with improvement trace

### AC4: `reason.decompose` Action
1. Breaks problem into sub-problems
2. Solves sub-problems independently (parallel via rayon)
3. Synthesizes final answer
4. `max_depth` for recursive decomposition

### AC5: Structured Output Parsing
1. JSON parsing with fallback to delimiter-based
2. Regex extraction for `<thinking>...</thinking>` blocks
3. Graceful handling of malformed LLM output
4. Error includes raw output for debugging

### AC6: Reasoning Trace
1. All actions return `ReasoningTrace` struct
2. Trace format compatible with tracing crate
3. Optional export to file (newline-delimited JSON)
4. Memory-efficient (streaming to file)

### AC7: Rust-Native Tool Registry
1. ReAct can use any registered Rust action
2. Tool discovery: `tools.list` returns available actions
3. Tool schema: JSON Schema for action parameters
4. Tool execution: Direct function call (no HTTP)

### AC8: Feature Flag
1. Actions behind `--features reasoning` cargo flag
2. Minimal dependencies when disabled

## Tasks / Subtasks

- [ ] **Task 1: `reason.cot` Action** (AC: 1, 5)
  - [ ] Implement CoT prompt wrapper
  - [ ] Thinking format templates
  - [ ] Few-shot example injection
  - [ ] Output parsing (JSON, delimiters)
  - [ ] Unit tests with mock LLM

- [ ] **Task 2: `reason.react` Action** (AC: 2, 7)
  - [ ] Implement ReAct loop
  - [ ] Tool registry integration
  - [ ] max_steps enforcement
  - [ ] Early termination detection
  - [ ] Trace accumulation
  - [ ] Unit and integration tests

- [ ] **Task 3: `reason.self_correct` Action** (AC: 3)
  - [ ] Implement generate-critique-improve cycle
  - [ ] Multi-model support
  - [ ] Improvement trace
  - [ ] Unit tests

- [ ] **Task 4: `reason.decompose` Action** (AC: 4)
  - [ ] Implement decomposition
  - [ ] Parallel sub-problem solving
  - [ ] Answer synthesis
  - [ ] max_depth limit
  - [ ] Unit tests

- [ ] **Task 5: Reasoning Trace** (AC: 6)
  - [ ] Define `ReasoningTrace` struct
  - [ ] Integration with tracing crate
  - [ ] File export option
  - [ ] Unit tests

- [ ] **Task 6: Tool Registry** (AC: 7)
  - [ ] Tool discovery mechanism
  - [ ] JSON Schema generation for actions
  - [ ] Unit tests

- [ ] **Task 7: Feature Flag & Integration** (AC: 8)
  - [ ] Add `reasoning` feature to Cargo.toml
  - [ ] Conditional compilation
  - [ ] Integration tests

## Dev Notes

### Source Tree

```
rust/src/
├── engine/
│   ├── actions/
│   │   └── reasoning.rs      # NEW: Reasoning actions
│   └── reasoning/
│       ├── mod.rs
│       ├── cot.rs            # Chain-of-Thought
│       ├── react.rs          # ReAct loop
│       ├── self_correct.rs   # Self-correction
│       ├── decompose.rs      # Problem decomposition
│       └── trace.rs          # Reasoning trace
```

### YAML Syntax

#### Chain-of-Thought
```yaml
nodes:
  - name: solve_math
    action: reason.cot
    with:
      problem: "{{ state.math_problem }}"
      model: ollama:llama3.2
      thinking_format: step_by_step
      few_shot:
        - problem: "What is 15% of 80?"
          thinking: |
            Step 1: Convert 15% to decimal: 0.15
            Step 2: Multiply: 0.15 × 80 = 12
          answer: "12"
```

#### ReAct (Rust-native tools)
```yaml
nodes:
  - name: research
    action: reason.react
    with:
      goal: "{{ state.research_goal }}"
      model: ollama:llama3.2
      tools:
        - web.scrape    # Rust-native via Firecrawl
        - memory.store  # Rust-native
        - json.transform
      max_steps: 10
```

#### Self-Correction
```yaml
nodes:
  - name: improve_code
    action: reason.self_correct
    with:
      task: "{{ state.coding_task }}"
      generator_model: ollama:codellama
      critic_model: ollama:llama3.2
      improvement_rounds: 2
      critic_prompt: |
        Review this code for bugs and improvements:
        {{ output }}
```

### Rust Types

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CotOutput {
    pub thinking: String,
    pub answer: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReactTrace {
    pub steps: Vec<ReactStep>,
    pub final_answer: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReactStep {
    pub step: u32,
    pub thought: String,
    pub action: String,
    pub action_input: serde_json::Value,
    pub observation: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelfCorrectOutput {
    pub final_output: serde_json::Value,
    pub improvement_history: Vec<ImprovementRound>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementRound {
    pub round: u32,
    pub output: serde_json::Value,
    pub critique: String,
    pub improved: serde_json::Value,
}

#[derive(Debug, Clone, Copy)]
pub enum ThinkingFormat {
    StepByStep,
    ProsCons,
    Tree,
}
```

### ReAct Prompt Template

```rust
const REACT_SYSTEM_PROMPT: &str = r#"
You are a reasoning agent. For each step:
1. Think about what to do next
2. Choose an action from available tools
3. Observe the result
4. Repeat until goal achieved

Available tools:
{{ tools | json }}

Respond in JSON:
{"thought": "...", "action": "tool_name", "action_input": {...}}

When goal is achieved:
{"thought": "Goal achieved", "action": "finish", "action_input": {"answer": "..."}}
"#;
```

### Dependencies

```toml
[dependencies]
regex = "1.10"  # For output parsing
tracing = "0.1"

[features]
reasoning = []
```

### Related Stories
- TEA-RUST-001: Rust Migration Epic
- TEA-AGENT-001.4: Python version (reference)
- TEA-RUST-014: Library API (action registry)

## Testing

### Test File Location
- `rust/tests/test_reasoning_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| reason.cot | 6 | P0 |
| reason.react | 8 | P0 |
| reason.self_correct | 4 | P1 |
| reason.decompose | 4 | P1 |
| Output Parsing | 6 | P0 |
| Tool Integration | 4 | P0 |

### Key Test Scenarios

1. **CoT parsing** - Structured output extracted correctly
2. **ReAct tool call** - Tool executed with correct args
3. **ReAct max_steps** - Terminates at limit
4. **ReAct finish** - Early termination detected
5. **Malformed output** - Graceful fallback
6. **Self-correct improvement** - Quality increases

## QA Notes

**Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)
**Assessment:** docs/qa/assessments/TEA-AGENT-001.4-rust-test-design-20260105.md

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 48 |
| **Unit tests** | 28 (58%) |
| **Integration tests** | 14 (29%) |
| **E2E tests** | 6 (13%) |
| **Priority distribution** | P0: 22, P1: 16, P2: 8, P3: 2 |

**Coverage Assessment:** All 8 Acceptance Criteria have test coverage. No gaps identified.

### Risk Areas Identified

| Risk | Probability | Impact | Severity |
|------|-------------|--------|----------|
| Infinite ReAct loop | Medium | High | **CRITICAL** |
| Malformed LLM output crash | High | Medium | **HIGH** |
| Tool execution failure in ReAct | Medium | High | **HIGH** |
| Memory exhaustion on large trace | Low | High | **MEDIUM** |
| Parallel decompose race condition | Low | Medium | **LOW** |

### Recommended Test Scenarios (P0 - Critical Path)

1. **ReAct Loop Safety**
   - `001.4R-UNIT-010`: Enforce max_steps limit (terminates at exactly N)
   - `001.4R-INT-004`: Execute ReAct with max_steps=3, verify termination
   - `001.4R-E2E-001`: Complete ReAct workflow with web.scrape tool

2. **Output Parsing Robustness**
   - `001.4R-UNIT-001`: Parse JSON CoT response with thinking and answer fields
   - `001.4R-UNIT-002`: Parse delimiter-based response (`<thinking>...</thinking>`)
   - `001.4R-UNIT-019`: Fallback to delimiter parsing when JSON fails
   - `001.4R-UNIT-021`: Return error with raw output on total parse failure

3. **Tool Registry Integration**
   - `001.4R-UNIT-026`: Register action in tool registry
   - `001.4R-UNIT-027`: Generate JSON Schema for action parameters
   - `001.4R-INT-014`: Execute tool via registry from ReAct

### Concerns / Blockers

1. **LLM Output Variability**: High probability of malformed output from edge-deployed models. Recommend defensive parsing with explicit fallback chains.

2. **Loop Termination**: The ReAct loop is safety-critical. `max_steps` enforcement must be tested exhaustively including edge cases (max_steps=0, max_steps=1).

3. **Tool Schema Generation**: JSON Schema generation for Rust action parameters requires careful design to ensure type fidelity.

4. **Feature Flag Testing**: Conditional compilation via `--features reasoning` needs build matrix CI coverage.

### Recommendations

- **Phase 1 (Blocking)**: Implement and pass all P0 unit tests before integration work
- **Phase 2 (High Priority)**: P0 integration tests covering ReAct and tool registry
- **Phase 3 (Pre-Release)**: E2E tests with real Ollama models
- **CI Configuration**: Add `cargo test --no-default-features` and `cargo test --features reasoning` to workflow

### Gate Decision

**Status:** READY FOR DEVELOPMENT

Story is well-defined with clear acceptance criteria and comprehensive test design. Risk areas have mitigation tests. No blockers identified.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial Rust adaptation from TEA-AGENT-001.4 | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes with test design review | Quinn (QA) |
