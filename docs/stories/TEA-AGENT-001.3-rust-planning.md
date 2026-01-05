# Story TEA-AGENT-001.3-rust: Planning & Decomposition (Rust/Embedded)

## Status

**Ready for Development**

*Updated: 2026-01-05 - QA validation passed. Test design complete with 48 scenarios covering all 8 acceptance criteria.*

## Story

**As a** developer deploying autonomous planning agents to edge environments,
**I want** a built-in planning primitive in the Rust runtime,
**so that** agents can decompose complex tasks and execute them without Python dependencies.

## Background

This is the Rust adaptation of TEA-AGENT-001.3, optimized for embedded/offline execution. Planning is a **fully portable** pattern as it's logic-based with LLM assistance.

| Aspect | Python Version | Rust Version |
|--------|---------------|--------------|
| **Plan Storage** | Python dict | Rust struct (serde) |
| **Parallel Execution** | ThreadPoolExecutor | rayon thread pool |
| **LLM Provider** | Any | Ollama, OpenAI-compatible only |
| **Checkpoint Format** | Pickle | bincode (faster, smaller) |

## Scope

### In Scope
- `plan.decompose` - LLM-based task decomposition
- `plan.execute` - Subtask execution with dependency ordering
- `plan.replan` - Re-planning on failure
- `plan.status` - Progress tracking
- DAG validation and topological sort
- Checkpoint persistence for plan state

### Out of Scope
- DSPy-enhanced planning (Python-only)

## Acceptance Criteria

### AC1: Plan Data Structure (Rust Native)
1. `Plan` struct with subtasks and dependencies
2. DAG validation via `petgraph` (cycle detection)
3. Topological sort for execution order
4. Serde serialization for checkpointing
5. Zero-copy where possible

### AC2: `plan.decompose` Action
1. Uses LLM (Ollama/OpenAI-compatible) to decompose goal
2. Planning strategies: `flat`, `hierarchical`, `iterative`
3. Structured output parsing (JSON from LLM)
4. `max_depth` limit for hierarchical plans
5. Returns validated `Plan` struct

### AC3: `plan.execute` Action
1. Executes subtasks in topological order
2. Parallel execution of independent subtasks via rayon
3. State threading: subtask receives accumulated state
4. `max_concurrent` configurable (rayon thread limit)
5. Progress tracking in state

### AC4: Subtask Failure Handling
1. `replan`: Re-plan from current state
2. `retry`: Retry with exponential backoff
3. `skip`: Mark as skipped, continue
4. `abort`: Stop execution, return partial results
5. Configurable per `plan.execute`

### AC5: `plan.replan` Action
1. Preserves completed subtasks
2. Re-plans remaining work from current state
3. `max_replans` limit enforced
4. Uses same LLM as original decomposition

### AC6: `plan.status` Action
1. Returns aggregated status counts
2. Filters: `include_completed`, `include_details`
3. Efficient: O(n) single pass over subtasks

### AC7: Checkpoint Integration
1. Plan serialized via bincode
2. Execution resumes from interrupted subtask
3. Completed subtasks not re-executed
4. Checkpoint size optimized for embedded storage

### AC8: Feature Flag
1. Actions behind `--features planning` cargo flag
2. No `petgraph` overhead when disabled

## Tasks / Subtasks

- [ ] **Task 1: Plan Data Structure** (AC: 1)
  - [ ] Define `Plan`, `Subtask`, `SubtaskStatus` structs
  - [ ] Implement DAG validation with petgraph
  - [ ] Topological sort implementation
  - [ ] Serde/bincode serialization
  - [ ] Unit tests

- [ ] **Task 2: `plan.decompose` Action** (AC: 2)
  - [ ] Implement decompose action
  - [ ] Flat strategy (simple list)
  - [ ] Hierarchical strategy (tree)
  - [ ] Iterative strategy (step-by-step)
  - [ ] LLM response parsing
  - [ ] Unit tests with mock LLM

- [ ] **Task 3: `plan.execute` Action** (AC: 3, 7)
  - [ ] Implement execute action
  - [ ] Dependency-ordered execution
  - [ ] Parallel execution via rayon
  - [ ] State threading
  - [ ] Checkpoint integration
  - [ ] Unit and integration tests

- [ ] **Task 4: Failure Handling** (AC: 4)
  - [ ] Define `OnSubtaskFailure` enum
  - [ ] Implement `replan` strategy
  - [ ] Implement `retry` strategy
  - [ ] Implement `skip` strategy
  - [ ] Implement `abort` strategy
  - [ ] Unit tests

- [ ] **Task 5: `plan.replan` Action** (AC: 5)
  - [ ] Implement replan action
  - [ ] Preserve completed subtasks
  - [ ] max_replans enforcement
  - [ ] Unit tests

- [ ] **Task 6: `plan.status` Action** (AC: 6)
  - [ ] Implement status action
  - [ ] Filtering options
  - [ ] Unit tests

- [ ] **Task 7: Feature Flag & Integration** (AC: 8)
  - [ ] Add `planning` feature to Cargo.toml
  - [ ] Conditional compilation
  - [ ] Integration tests

## Dev Notes

### Source Tree

```
rust/src/
├── engine/
│   ├── actions/
│   │   └── planning.rs       # NEW: Planning actions
│   └── plan/
│       ├── mod.rs
│       ├── types.rs          # Plan, Subtask structs
│       ├── dag.rs            # DAG validation, topo sort
│       └── executor.rs       # Parallel execution
```

### YAML Syntax

```yaml
nodes:
  - name: create_plan
    action: plan.decompose
    with:
      goal: "{{ state.user_goal }}"
      strategy: hierarchical
      max_depth: 2
      planner:
        model: ollama:llama3.2
        prompt_template: |
          Decompose this goal into subtasks:
          Goal: {{ goal }}

          Return JSON array:
          [{"id": "1", "description": "...", "dependencies": []}]

  - name: run_plan
    action: plan.execute
    with:
      plan: "{{ state.plan }}"
      parallel: true
      max_concurrent: 4
      subtask_executor:
        action: llm.call
      on_subtask_failure: retry
      max_retries: 2

  - name: check_progress
    action: plan.status
    with:
      include_completed: false
```

### Rust Types

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Plan {
    pub id: String,
    pub goal: String,
    pub strategy: PlanStrategy,
    pub subtasks: Vec<Subtask>,
    pub metadata: PlanMetadata,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subtask {
    pub id: String,
    pub description: String,
    pub dependencies: Vec<String>,
    pub status: SubtaskStatus,
    pub result: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum SubtaskStatus {
    Pending,
    InProgress,
    Completed,
    Failed,
    Skipped,
}

#[derive(Debug, Clone, Copy)]
pub enum PlanStrategy {
    Flat,
    Hierarchical { max_depth: u32 },
    Iterative,
}

#[derive(Debug, Clone, Copy)]
pub enum OnSubtaskFailure {
    Replan,
    Retry { max_retries: u32 },
    Skip,
    Abort,
}
```

### DAG Implementation

```rust
use petgraph::graph::DiGraph;
use petgraph::algo::{is_cyclic_directed, toposort};

impl Plan {
    pub fn validate(&self) -> Result<(), PlanError> {
        let mut graph = DiGraph::new();
        let mut node_indices = HashMap::new();

        // Add nodes
        for subtask in &self.subtasks {
            let idx = graph.add_node(subtask.id.clone());
            node_indices.insert(subtask.id.clone(), idx);
        }

        // Add edges (dependencies)
        for subtask in &self.subtasks {
            let to_idx = node_indices[&subtask.id];
            for dep_id in &subtask.dependencies {
                let from_idx = node_indices.get(dep_id)
                    .ok_or(PlanError::MissingDependency(dep_id.clone()))?;
                graph.add_edge(*from_idx, to_idx, ());
            }
        }

        // Check for cycles
        if is_cyclic_directed(&graph) {
            return Err(PlanError::CyclicDependency);
        }

        Ok(())
    }

    pub fn execution_order(&self) -> Vec<&Subtask> {
        // Returns subtasks in topological order
        // ...
    }
}
```

### Dependencies

```toml
[dependencies]
petgraph = "0.6"
rayon = "1.8"
bincode = "1.3"

[features]
planning = ["petgraph"]
```

### Related Stories
- TEA-RUST-001: Rust Migration Epic
- TEA-AGENT-001.3: Python version (reference)
- TEA-AGENT-001.1-rust: Multi-Agent (for subtask execution)

## Testing

### Test File Location
- `rust/tests/test_planning_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Plan Structure | 6 | P0 |
| DAG Validation | 4 | P0 |
| plan.decompose | 6 | P0 |
| plan.execute | 8 | P0 |
| Failure Handling | 6 | P0 |
| Checkpoint | 4 | P1 |

### Key Test Scenarios

1. **Cycle detection** - Invalid DAG rejected
2. **Topological order** - Dependencies respected
3. **Parallel execution** - Independent subtasks concurrent
4. **Replan on failure** - Plan adjusted correctly
5. **Checkpoint resume** - Completed subtasks skipped
6. **max_replans limit** - Stops after limit exceeded

## QA Notes

**Assessment Date:** 2026-01-05
**Test Design:** [TEA-AGENT-001.3-rust-test-design-20260105.md](../qa/assessments/TEA-AGENT-001.3-rust-test-design-20260105.md)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total scenarios** | 48 |
| **Unit tests** | 28 (58%) |
| **Integration tests** | 16 (33%) |
| **E2E tests** | 4 (8%) |
| **P0 (Critical)** | 32 |
| **P1 (High)** | 12 |
| **P2 (Medium)** | 4 |

All 8 acceptance criteria have full test coverage.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **DAG cycles cause infinite loops** | Critical | 4 P0 tests for cycle detection (direct, indirect, self-referencing) |
| **Parallel execution race conditions** | High | 3 P0 tests for rayon thread safety and state deep-copy |
| **LLM response parsing failures** | High | 4 P0 tests for structured output validation and error handling |
| **Checkpoint corruption on crash** | Medium | 4 P1 tests for recovery scenarios |
| **Memory exhaustion on large plans** | Medium | P2 test for embedded storage constraints (<1MB for 100 subtasks) |
| **Infinite replans** | Medium | P0 test for max_replans enforcement |

### Recommended Test Scenarios

**Priority 0 (Must-have before merge):**
1. DAG validation tests (cycle detection) - 001.3R-UNIT-004 through 001.3R-UNIT-006
2. Topological sort correctness - 001.3R-UNIT-007, 001.3R-UNIT-008
3. Parallel execution via rayon - 001.3R-INT-005 through 001.3R-INT-008
4. Subtask failure handling (all strategies) - 001.3R-UNIT-021 through 001.3R-UNIT-025
5. Checkpoint integration - 001.3R-INT-014 through 001.3R-INT-016
6. LLM integration boundaries - 001.3R-INT-001, 001.3R-INT-002
7. Full plan execution E2E - 001.3R-E2E-001

**Test Execution Order:**
1. P0 Unit tests (fast feedback on core logic)
2. P0 Integration tests (component boundaries)
3. P0 E2E tests (critical user journeys)
4. P1/P2 tests (secondary scenarios)

### Concerns / Blockers

1. **Mock LLM Infrastructure**: Unit tests require a mock LLM client implementation to avoid network dependencies. Recommend implementing `MockLlmClient` with predefined JSON responses before test development.

2. **Rayon Thread Determinism**: For deterministic testing, use `ThreadPoolBuilder::new().num_threads(1).build()` to force sequential execution, then test with multiple threads for race condition detection.

3. **Checkpoint Recovery Testing**: Requires injecting failures at specific subtask boundaries to simulate crashes. Consider adding a test hook or fault injection mechanism.

4. **Feature Flag Testing**: Tests for `--features planning` need to verify both enabled and disabled builds, requiring separate CI matrix entries.

5. **LLM Provider Coverage**: Currently only Ollama and OpenAI-compatible providers are in scope. Both require integration test coverage (001.3R-INT-001, 001.3R-INT-002).

### Quality Gate Status

**READY FOR DEVELOPMENT** - Test design complete with comprehensive coverage. No blocking gaps identified.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial Rust adaptation from TEA-AGENT-001.3 | Sarah (PO) |
| 2026-01-05 | 0.2 | Added QA Notes based on test design assessment | Quinn (QA) |
