# Story TEA-AGENT-001.3: Planning & Decomposition Primitive

## Status

**Ready for Development**

*Updated: 2026-01-05 - All story checklist criteria passed. Test design complete with 58 scenarios covering all 9 ACs.*

## Story

**As a** YAML agent developer,
**I want** a built-in planning primitive,
**so that** I can decompose complex tasks into subtasks and execute them automatically.

## Background

Planning is a core agentic pattern (Chapter 6 of Agentic Design Patterns). Currently, users must:

1. Design LLM prompts for task decomposition
2. Parse plan outputs into structured subtasks
3. Determine dependency order between subtasks
4. Execute subtasks with proper state threading
5. Handle failures and re-planning manually

This story introduces `plan.*` actions that encapsulate the entire planning lifecycle.

## Acceptance Criteria

### AC1: `plan.decompose` Action
1. Uses LLM to decompose goal into subtasks
2. Returns structured plan with dependencies
3. Supports planning strategies: `flat`, `hierarchical`, `iterative`
4. Validates plan structure before returning
5. Configurable max_depth for hierarchical plans

### AC2: Plan Structure
1. Each subtask has: id, description, dependencies, status
2. Dependencies form a DAG (no cycles)
3. Status: `pending`, `in_progress`, `completed`, `failed`, `skipped`
4. Plan stored in state for checkpointing

### AC3: `plan.execute` Action
1. Executes subtasks respecting dependency order
2. Parallel execution of independent subtasks (when enabled)
3. State threading: each subtask receives accumulated state
4. Configurable `max_concurrent` for parallel execution
5. Progress tracking via state updates

### AC4: Subtask Failure Handling
1. `replan`: Re-plan from current state
2. `retry`: Retry failed subtask with backoff
3. `skip`: Mark failed subtask as skipped, continue
4. `abort`: Stop execution, return partial results
5. Configurable per `plan.execute` invocation

### AC5: `plan.replan` Action
1. Triggers re-planning from current state
2. Preserves completed subtasks
3. Adjusts remaining plan based on new context
4. Maximum re-plan attempts configurable

### AC6: `plan.status` Action
1. Returns current plan execution status
2. Includes: completed, in_progress, pending, failed counts
3. Optional: include/exclude completed subtasks
4. Optional: include subtask details

### AC7: Checkpoint Integration
1. Plan state persists across checkpoints
2. Execution can resume from interrupted subtask
3. Completed subtasks not re-executed on resume

### AC8: Python Implementation
1. New module: `python/src/the_edge_agent/actions/planning_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%

### AC9: Rust Implementation
1. New module: `rust/src/engine/actions/planning_actions.rs`
2. Feature parity with Python for core actions

## Tasks / Subtasks

- [ ] **Task 1: Plan Data Structure** (AC: 2)
  - [ ] Define `Plan` class with subtasks and dependencies
  - [ ] DAG validation (cycle detection)
  - [ ] Topological sort for execution order
  - [ ] JSON serialization for checkpointing
  - [ ] Unit tests

- [ ] **Task 2: `plan.decompose` Action** (AC: 1)
  - [ ] Implement flat strategy (simple list)
  - [ ] Implement hierarchical strategy (tree)
  - [ ] Implement iterative strategy (step-by-step)
  - [ ] LLM prompt templates for each strategy
  - [ ] Plan validation
  - [ ] Unit tests

- [ ] **Task 3: `plan.execute` Action** (AC: 3, 7)
  - [ ] Sequential execution with dependency order
  - [ ] Parallel execution of independent subtasks
  - [ ] State threading between subtasks
  - [ ] Progress tracking in state
  - [ ] Checkpoint integration
  - [ ] Unit and integration tests

- [ ] **Task 4: Failure Handling** (AC: 4)
  - [ ] Implement `replan` strategy
  - [ ] Implement `retry` strategy
  - [ ] Implement `skip` strategy
  - [ ] Implement `abort` strategy
  - [ ] Unit tests

- [ ] **Task 5: `plan.replan` Action** (AC: 5)
  - [ ] Preserve completed subtasks
  - [ ] Re-plan from current state
  - [ ] Maximum re-plan limit
  - [ ] Unit tests

- [ ] **Task 6: `plan.status` Action** (AC: 6)
  - [ ] Status aggregation
  - [ ] Filtering options
  - [ ] Unit tests

- [ ] **Task 7: Rust Implementation** (AC: 8, 9)
  - [ ] Create `planning_actions.rs` module
  - [ ] Implement Plan struct
  - [ ] Implement `plan.decompose`
  - [ ] Implement `plan.execute`
  - [ ] Implement `plan.replan`
  - [ ] Implement `plan.status`
  - [ ] Unit and integration tests

- [ ] **Task 8: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: research-planning.yaml
  - [ ] Create example: code-refactoring-plan.yaml
  - [ ] Create example: multi-step-task.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py           # Add planning_actions
│   ├── agent_actions.py      # Reference: agent.parallel
│   ├── planning_actions.py   # NEW: Planning actions
│   └── ...
└── parallel.py               # Reference: parallel execution
```

**Rust:**
```
rust/src/engine/
├── actions/
│   ├── mod.rs
│   └── planning_actions.rs   # NEW: Planning actions
```

### YAML Syntax Reference

```yaml
nodes:
  - name: plan_task
    action: plan.decompose
    with:
      goal: "{{ state.user_goal }}"
      planner:
        model: gpt-4
        strategy: hierarchical    # flat | hierarchical | iterative
        max_depth: 3              # For hierarchical
        prompt_template: |        # Optional custom prompt
          Break down this goal into subtasks:
          Goal: {{ goal }}

          Return JSON array of subtasks with dependencies.

  - name: execute_plan
    action: plan.execute
    with:
      plan: "{{ state.plan }}"    # From decompose
      parallel: true              # Execute independent tasks in parallel
      max_concurrent: 3
      subtask_executor:
        action: llm.call          # Action to execute each subtask
        # OR use agent.dispatch:
        # action: agent.dispatch
        # with:
        #   agent: worker
      on_subtask_failure: replan  # replan | retry | skip | abort
      max_replans: 2

  - name: check_status
    action: plan.status
    with:
      include_completed: false
      include_details: true
```

### Plan Structure

```json
{
  "id": "plan_123",
  "goal": "Research and summarize topic X",
  "strategy": "hierarchical",
  "subtasks": [
    {
      "id": "subtask_1",
      "description": "Search for relevant sources",
      "dependencies": [],
      "status": "completed",
      "result": "Found 5 sources..."
    },
    {
      "id": "subtask_2",
      "description": "Read and extract key points",
      "dependencies": ["subtask_1"],
      "status": "in_progress",
      "result": null
    },
    {
      "id": "subtask_3",
      "description": "Synthesize into summary",
      "dependencies": ["subtask_2"],
      "status": "pending",
      "result": null
    }
  ],
  "metadata": {
    "created_at": "2026-01-04T12:00:00Z",
    "replan_count": 0,
    "max_replans": 2
  }
}
```

### State Variables Set by Planning Actions

| Variable | Type | Description |
|----------|------|-------------|
| `plan` | dict | Current plan structure |
| `plan_status` | dict | Aggregated status counts |
| `current_subtask` | dict | Currently executing subtask |
| `subtask_results` | dict | Map of subtask_id → result |
| `replan_count` | int | Number of re-plans triggered |

### Planning Strategies

| Strategy | Description | Best For |
|----------|-------------|----------|
| `flat` | Simple list of subtasks | Linear tasks |
| `hierarchical` | Tree of subtasks with sub-subtasks | Complex goals |
| `iterative` | Plan one step at a time | Exploratory tasks |

### Dependencies
- TEA-AGENT-001.1: Multi-Agent (for agent.dispatch in subtask executor)
- TEA-AGENT-001.2: Reflection (for replan evaluation)

## Testing

### Test File Locations
- Python: `python/tests/test_planning_actions.py`
- Rust: `rust/tests/test_planning_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Plan Structure | 6 | P0 |
| plan.decompose | 8 | P0 |
| plan.execute | 10 | P0 |
| Failure Handling | 8 | P0 |
| plan.replan | 6 | P1 |
| plan.status | 4 | P1 |
| Checkpoint Integration | 4 | P1 |

### Key Test Scenarios

1. **Simple flat plan** - Linear task execution
2. **Dependency ordering** - Subtasks execute in correct order
3. **Parallel independent tasks** - Independent subtasks run concurrently
4. **Cycle detection** - Invalid DAG rejected
5. **Replan on failure** - Plan adjusted after subtask failure
6. **Checkpoint resume** - Execution resumes from interrupted subtask
7. **Max replan limit** - Stops after max_replans exceeded

## QA Notes

**Test Design Review:** 2026-01-05 | **Designer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 58 | 100% |
| Unit tests | 32 | 55% |
| Integration tests | 18 | 31% |
| E2E tests | 8 | 14% |

**Priority Distribution:** P0: 26 (critical) | P1: 22 (core) | P2: 10 (edge cases)

**Coverage Assessment:** All 9 Acceptance Criteria have appropriate test coverage. No significant gaps identified.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Cycle in dependency graph causes infinite loop | Medium | Critical | 1.3-UNIT-009, 1.3-UNIT-015 |
| Parallel execution race conditions | Medium | High | 1.3-INT-008, 1.3-INT-011 |
| State corruption during replan | Medium | High | 1.3-UNIT-024, 1.3-INT-013 |
| LLM returns malformed plan | High | Medium | 1.3-UNIT-005, 1.3-UNIT-006 |
| Checkpoint corruption | Low | Critical | 1.3-INT-016, 1.3-E2E-004 |
| Max replan exceeded without recovery | Medium | Medium | 1.3-UNIT-026, 1.3-E2E-002 |

### Recommended Test Scenarios

**Critical P0 Tests (Must Pass):**
1. **Plan Structure Validation** - DAG cycle detection, topological sort, JSON serialization
2. **plan.decompose** - All three strategies (flat, hierarchical, iterative), max_depth enforcement
3. **plan.execute** - Sequential execution order, state threading, progress tracking
4. **Failure Handling** - All four strategies (replan, retry, skip, abort)
5. **Checkpoint Integration** - Persistence, resume from interrupted subtask, idempotency

**Key E2E Scenarios:**
- 1.3-E2E-004: Full plan interrupted and resumed successfully (Critical user flow)
- 1.3-E2E-006: Python YAML agent with planning executes (Language parity)
- 1.3-E2E-007: Rust YAML agent with planning executes (Language parity)

### Concerns and Recommendations

1. **Concurrency Safety:** Parallel subtask execution requires careful state isolation testing. Tests 1.3-INT-008 and 1.3-INT-011 are critical for validating thread safety.

2. **LLM Response Robustness:** High probability of malformed LLM responses. Recommend defensive parsing with graceful degradation (covered by 1.3-UNIT-005, 1.3-UNIT-006).

3. **Cross-Runtime Parity:** Test 1.3-E2E-008 validates Python/Rust produce same results. This is P2 but important for multi-language deployments.

4. **Dependencies:** This story depends on TEA-AGENT-001.1 (Multi-Agent) and TEA-AGENT-001.2 (Reflection). Ensure those stories are complete before integration testing.

**Test Design Reference:** `docs/qa/assessments/TEA-AGENT-001.3-test-design-20260105.md`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.2 | Added QA Notes from test design review | Quinn (QA) |
| 2026-01-04 | 0.1 | Initial story draft | Sarah (PO) |
