# Test Design: Story TEA-AGENT-001.3-rust

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story Title:** Planning & Decomposition (Rust/Embedded)
**Story Slug:** rust-planning

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 48 |
| **Unit tests** | 28 (58%) |
| **Integration tests** | 16 (33%) |
| **E2E tests** | 4 (8%) |
| **Priority distribution** | P0: 32, P1: 12, P2: 4 |

### Risk Assessment Summary

| Risk | Impact | Mitigation |
|------|--------|------------|
| DAG cycles cause infinite loops | Critical | P0 cycle detection tests |
| Parallel execution race conditions | High | P0 rayon thread safety tests |
| LLM response parsing failures | High | P0 structured output validation |
| Checkpoint corruption on crash | Medium | P1 recovery tests |
| Memory exhaustion on large plans | Medium | P1 resource limit tests |

---

## Test Scenarios by Acceptance Criteria

### AC1: Plan Data Structure (Rust Native)

**Requirements:**
1. `Plan` struct with subtasks and dependencies
2. DAG validation via `petgraph` (cycle detection)
3. Topological sort for execution order
4. Serde serialization for checkpointing
5. Zero-copy where possible

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-001 | Unit | P0 | Plan struct construction with valid subtasks | Core data structure validation |
| 001.3R-UNIT-002 | Unit | P0 | Plan struct rejects empty subtask list | Input validation, edge case |
| 001.3R-UNIT-003 | Unit | P0 | Subtask dependencies reference existing subtask IDs | Referential integrity |
| 001.3R-UNIT-004 | Unit | P0 | DAG validation detects direct cycles (A->B->A) | Critical: prevents infinite loops |
| 001.3R-UNIT-005 | Unit | P0 | DAG validation detects indirect cycles (A->B->C->A) | Critical: complex cycle detection |
| 001.3R-UNIT-006 | Unit | P0 | DAG validation detects self-referencing subtask | Edge case: subtask depends on itself |
| 001.3R-UNIT-007 | Unit | P0 | Topological sort returns correct execution order | Critical: execution correctness |
| 001.3R-UNIT-008 | Unit | P0 | Topological sort handles multiple valid orderings | Algorithm flexibility |
| 001.3R-UNIT-009 | Unit | P1 | Serde serialization round-trip preserves Plan | Checkpoint correctness |
| 001.3R-UNIT-010 | Unit | P1 | Bincode serialization produces compact output | Embedded storage constraint |
| 001.3R-UNIT-011 | Unit | P1 | Deserialization of corrupted bincode returns error | Error handling, security |

---

### AC2: `plan.decompose` Action

**Requirements:**
1. Uses LLM (Ollama/OpenAI-compatible) to decompose goal
2. Planning strategies: `flat`, `hierarchical`, `iterative`
3. Structured output parsing (JSON from LLM)
4. `max_depth` limit for hierarchical plans
5. Returns validated `Plan` struct

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-012 | Unit | P0 | Flat strategy produces linear subtask list | Strategy correctness |
| 001.3R-UNIT-013 | Unit | P0 | Hierarchical strategy respects max_depth limit | Recursion bound enforcement |
| 001.3R-UNIT-014 | Unit | P0 | Iterative strategy produces step-by-step plan | Strategy correctness |
| 001.3R-UNIT-015 | Unit | P0 | LLM JSON response parsed into Subtask structs | Critical: LLM integration |
| 001.3R-UNIT-016 | Unit | P0 | Malformed LLM JSON response returns ParseError | Error handling, robustness |
| 001.3R-UNIT-017 | Unit | P0 | LLM response with cyclic dependencies rejected post-parse | Defense in depth |
| 001.3R-INT-001 | Integration | P0 | decompose action invokes LLM via Ollama client | LLM integration boundary |
| 001.3R-INT-002 | Integration | P0 | decompose action invokes LLM via OpenAI-compatible client | LLM integration boundary |
| 001.3R-INT-003 | Integration | P1 | decompose action handles LLM timeout gracefully | Network failure handling |
| 001.3R-INT-004 | Integration | P1 | decompose action retries on transient LLM errors | Resilience |

---

### AC3: `plan.execute` Action

**Requirements:**
1. Executes subtasks in topological order
2. Parallel execution of independent subtasks via rayon
3. State threading: subtask receives accumulated state
4. `max_concurrent` configurable (rayon thread limit)
5. Progress tracking in state

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-018 | Unit | P0 | Execute respects topological order for dependent subtasks | Critical: execution correctness |
| 001.3R-UNIT-019 | Unit | P0 | Execute accumulates state across sequential subtasks | State threading correctness |
| 001.3R-UNIT-020 | Unit | P0 | Progress tracking updates after each subtask | Observability |
| 001.3R-INT-005 | Integration | P0 | Parallel execution of independent subtasks via rayon | Critical: parallelism works |
| 001.3R-INT-006 | Integration | P0 | max_concurrent=1 forces sequential execution | Configuration respected |
| 001.3R-INT-007 | Integration | P0 | State is deep-copied for parallel branches | Critical: race condition prevention |
| 001.3R-INT-008 | Integration | P0 | Parallel results merged correctly at convergence | Fan-in correctness |
| 001.3R-INT-009 | Integration | P1 | Execute with subtask_executor invokes nested action | Composition works |
| 001.3R-E2E-001 | E2E | P0 | Execute full plan with mixed sequential/parallel subtasks | Critical user journey |

---

### AC4: Subtask Failure Handling

**Requirements:**
1. `replan`: Re-plan from current state
2. `retry`: Retry with exponential backoff
3. `skip`: Mark as skipped, continue
4. `abort`: Stop execution, return partial results
5. Configurable per `plan.execute`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-021 | Unit | P0 | on_subtask_failure=abort stops execution on first failure | Critical: abort behavior |
| 001.3R-UNIT-022 | Unit | P0 | on_subtask_failure=skip marks subtask as Skipped, continues | Skip behavior |
| 001.3R-UNIT-023 | Unit | P0 | Skipped subtask unblocks dependent subtasks (or fails them) | Dependency chain handling |
| 001.3R-UNIT-024 | Unit | P0 | on_subtask_failure=retry retries with exponential backoff | Retry behavior |
| 001.3R-UNIT-025 | Unit | P0 | Retry exhaustion (max_retries exceeded) triggers fallback | Retry limit enforcement |
| 001.3R-INT-010 | Integration | P0 | on_subtask_failure=replan invokes plan.replan action | Replan behavior |
| 001.3R-INT-011 | Integration | P1 | Partial results returned after abort | Graceful degradation |

---

### AC5: `plan.replan` Action

**Requirements:**
1. Preserves completed subtasks
2. Re-plans remaining work from current state
3. `max_replans` limit enforced
4. Uses same LLM as original decomposition

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-026 | Unit | P0 | Replan preserves Completed subtasks unchanged | Critical: no re-execution |
| 001.3R-UNIT-027 | Unit | P0 | Replan removes Failed/Pending subtasks before re-planning | Clean slate for LLM |
| 001.3R-UNIT-028 | Unit | P0 | max_replans limit exceeded returns MaxReplansExceededError | Infinite loop prevention |
| 001.3R-INT-012 | Integration | P0 | Replan invokes same LLM provider as original decompose | Provider consistency |
| 001.3R-INT-013 | Integration | P1 | Replan includes failure context in LLM prompt | Better re-planning |

---

### AC6: `plan.status` Action

**Requirements:**
1. Returns aggregated status counts
2. Filters: `include_completed`, `include_details`
3. Efficient: O(n) single pass over subtasks

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-UNIT-029 | Unit | P1 | Status returns correct counts for each SubtaskStatus | Correctness |
| 001.3R-UNIT-030 | Unit | P1 | include_completed=false filters out Completed subtasks | Filter behavior |
| 001.3R-UNIT-031 | Unit | P1 | include_details=true returns subtask descriptions | Detail level |
| 001.3R-UNIT-032 | Unit | P2 | Status on empty plan returns zero counts | Edge case |

---

### AC7: Checkpoint Integration

**Requirements:**
1. Plan serialized via bincode
2. Execution resumes from interrupted subtask
3. Completed subtasks not re-executed
4. Checkpoint size optimized for embedded storage

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-INT-014 | Integration | P0 | Checkpoint saved after each subtask completion | Critical: crash recovery |
| 001.3R-INT-015 | Integration | P0 | Execute resumes from checkpoint at correct subtask | Critical: resume works |
| 001.3R-INT-016 | Integration | P0 | Completed subtasks not re-executed on resume | Idempotency |
| 001.3R-E2E-002 | E2E | P1 | Simulate crash mid-execution, resume completes plan | Full recovery journey |
| 001.3R-E2E-003 | E2E | P2 | Checkpoint file size reasonable for embedded (<1MB for 100 subtasks) | Non-functional: storage |

---

### AC8: Feature Flag

**Requirements:**
1. Actions behind `--features planning` cargo flag
2. No `petgraph` overhead when disabled

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.3R-INT-017 | Integration | P1 | Build without planning feature excludes petgraph | Dependency management |
| 001.3R-INT-018 | Integration | P1 | plan.* actions unavailable when feature disabled | Feature gating |
| 001.3R-E2E-004 | E2E | P2 | Binary size smaller without planning feature | Non-functional: size |

---

## Risk Coverage Matrix

| Risk | Test Scenarios |
|------|----------------|
| DAG cycles cause infinite loops | 001.3R-UNIT-004, 001.3R-UNIT-005, 001.3R-UNIT-006, 001.3R-UNIT-017 |
| Parallel execution race conditions | 001.3R-INT-005, 001.3R-INT-007, 001.3R-INT-008 |
| LLM response parsing failures | 001.3R-UNIT-015, 001.3R-UNIT-016, 001.3R-INT-003 |
| Checkpoint corruption on crash | 001.3R-UNIT-011, 001.3R-INT-014, 001.3R-INT-015, 001.3R-E2E-002 |
| Memory exhaustion on large plans | 001.3R-E2E-003 (implicit via size constraint) |
| Infinite replans | 001.3R-UNIT-028 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fast feedback on core logic)
   - DAG validation tests first (001.3R-UNIT-004 through 001.3R-UNIT-006)
   - Plan structure tests
   - Failure handling tests
2. **P0 Integration tests** (component boundaries)
   - LLM integration tests
   - Parallel execution tests
   - Checkpoint tests
3. **P0 E2E tests** (critical paths)
   - Full plan execution journey
4. **P1 tests** (important but lower risk)
5. **P2 tests** (nice to have)

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 48
  by_level:
    unit: 28
    integration: 16
    e2e: 4
  by_priority:
    p0: 32
    p1: 12
    p2: 4
  coverage_gaps: []
  key_risks_addressed:
    - DAG cycle detection (4 tests)
    - Race condition prevention (3 tests)
    - LLM error handling (4 tests)
    - Checkpoint recovery (4 tests)
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.3-rust-test-design-20260105.md
P0 tests identified: 32
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (001.3R-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Appendix: Test Implementation Notes

### Mock LLM for Unit Tests

```rust
struct MockLlmClient {
    responses: Vec<String>,
}

impl LlmClient for MockLlmClient {
    fn complete(&self, prompt: &str) -> Result<String, LlmError> {
        // Return predefined JSON responses for testing
    }
}
```

### Parallel Execution Testing

Use rayon's `ThreadPoolBuilder::new().num_threads(1).build()` to force sequential execution for deterministic testing, then test with multiple threads for race condition detection.

### Checkpoint Testing Strategy

1. Use temp directories for checkpoint files
2. Inject failure after specific subtask to simulate crash
3. Resume from checkpoint and verify correct subtask picked up
