# Test Design: Story TEA-AGENT-001.2

**Title:** Reflection Loop Primitive
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 52 |
| Unit tests | 32 (62%) |
| Integration tests | 14 (27%) |
| E2E tests | 6 (11%) |
| Priority distribution | P0: 22, P1: 20, P2: 8, P3: 2 |

### Test Strategy Rationale

This story implements a core agentic design pattern (self-correction via reflection loops). The reflection primitives are critical building blocks that will be used across many agent configurations. Therefore:

- **High unit test coverage** for core loop logic, evaluators, and strategies
- **Integration tests** for action registry, YAML engine integration, and cross-component flows
- **E2E tests** for complete agent workflows using reflection loops

---

## Test Scenarios by Acceptance Criteria

### AC1: `reflection.loop` Action

Core loop executing generate→evaluate→correct cycle.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-001 | Unit | P0 | First-pass success - generator output passes evaluation immediately | Core happy path; loop should exit after one iteration |
| 001.2-UNIT-002 | Unit | P0 | Correction success - corrector fixes issues within max_iterations | Validates correction loop execution |
| 001.2-UNIT-003 | Unit | P0 | Circuit breaker triggers after max_iterations exceeded | Prevents infinite loops; critical safety mechanism |
| 001.2-UNIT-004 | Unit | P0 | Generator action invocation with correct parameters | Validates generator configuration works |
| 001.2-UNIT-005 | Unit | P0 | Corrector receives output and errors correctly | Data flow between evaluate→correct |
| 001.2-UNIT-006 | Unit | P1 | Generator uses inline code (`run:` block) | Alternative generator configuration |
| 001.2-UNIT-007 | Unit | P1 | Corrector uses inline code (`run:` block) | Alternative corrector configuration |
| 001.2-INT-001 | Integration | P0 | Full loop with action-based generator and corrector | End-to-end action chain |
| 001.2-INT-002 | Integration | P1 | Loop registered in actions registry | Discoverable via `build_actions_registry()` |

### AC2: Schema-Based Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-008 | Unit | P0 | Valid output passes schema validation | Happy path for schema evaluator |
| 001.2-UNIT-009 | Unit | P0 | Invalid output returns structured validation errors | Errors must be usable by corrector |
| 001.2-UNIT-010 | Unit | P0 | Nested object schema validation | Complex schema support |
| 001.2-UNIT-011 | Unit | P1 | $ref external schema file resolution | External schema reference support |
| 001.2-UNIT-012 | Unit | P1 | Type coercion attempts string→int before failure | Forgiving validation behavior |
| 001.2-UNIT-013 | Unit | P1 | Type coercion attempts string→bool before failure | Forgiving validation behavior |
| 001.2-INT-003 | Integration | P0 | Schema evaluator integrates with `validate.schema` action | Reuses existing validation infrastructure |

### AC3: LLM-Based Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-014 | Unit | P1 | LLM evaluator calls model with correct prompt | Prompt configuration works |
| 001.2-UNIT-015 | Unit | P0 | LLM response parsed to structured feedback (pass/fail/reason/suggestions) | Output parsing is critical |
| 001.2-UNIT-016 | Unit | P1 | Few-shot examples included in evaluation prompt | Enhanced evaluation support |
| 001.2-UNIT-017 | Unit | P1 | Custom evaluation prompt template with state access | Jinja2 templating in prompt |
| 001.2-UNIT-018 | Unit | P0 | Malformed LLM response handled gracefully | Error resilience |
| 001.2-INT-004 | Integration | P1 | LLM evaluator with mock LLM provider | Full evaluator flow |

### AC4: Custom Evaluator

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-019 | Unit | P0 | Python inline code evaluator executes and returns result | Primary custom evaluator |
| 001.2-UNIT-020 | Unit | P1 | Lua inline code evaluator executes and returns result | Lua runtime support |
| 001.2-UNIT-021 | Unit | P1 | Prolog inline code evaluator executes and returns result | Prolog runtime support |
| 001.2-UNIT-022 | Unit | P0 | Custom evaluator receives generator output | Correct data passed |
| 001.2-UNIT-023 | Unit | P0 | Custom evaluator can access full state | Context-aware evaluation |
| 001.2-UNIT-024 | Unit | P1 | Custom evaluator exception handled as failure | Error resilience |
| 001.2-INT-005 | Integration | P1 | Custom evaluator with state mutations | State flow validation |

### AC5: Iteration Tracking

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-025 | Unit | P0 | `reflection_iteration` counter increments correctly | Iteration tracking accuracy |
| 001.2-UNIT-026 | Unit | P0 | `reflection_history` accumulates all attempts | History for debugging/return_best |
| 001.2-UNIT-027 | Unit | P0 | `reflection_errors` contains evaluation failures | Error propagation |
| 001.2-UNIT-028 | Unit | P0 | Circuit breaker triggers at exact max_iterations | Boundary condition |
| 001.2-UNIT-029 | Unit | P1 | History entry includes output, errors, and score | Complete history structure |
| 001.2-INT-006 | Integration | P0 | State variables persist across node execution | StateGraph integration |

### AC6: On-Failure Strategies

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-030 | Unit | P0 | `return_best` returns highest-scoring attempt | Best-effort result selection |
| 001.2-UNIT-031 | Unit | P0 | `return_last` returns final attempt regardless of score | Alternative strategy |
| 001.2-UNIT-032 | Unit | P0 | `raise` throws ReflectionFailedError with full history | Fail-hard strategy |
| 001.2-UNIT-033 | Unit | P1 | Default strategy is `return_best` | Configuration default |
| 001.2-INT-007 | Integration | P0 | Strategy selection via YAML `on_failure` key | YAML configuration parsing |

### AC7: `reflection.evaluate` Standalone Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-034 | Unit | P1 | Standalone evaluate with schema evaluator | Independent action use |
| 001.2-UNIT-035 | Unit | P1 | Standalone evaluate with LLM evaluator | Independent action use |
| 001.2-UNIT-036 | Unit | P1 | Standalone evaluate with custom evaluator | Independent action use |
| 001.2-INT-008 | Integration | P1 | Evaluate action registered in registry | Action discoverability |

### AC8: `reflection.correct` Standalone Action

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-037 | Unit | P1 | Standalone correct receives output and errors | Independent action use |
| 001.2-UNIT-038 | Unit | P2 | Correct action with inline code corrector | Alternative configuration |
| 001.2-INT-009 | Integration | P1 | Correct action registered in registry | Action discoverability |
| 001.2-INT-010 | Integration | P2 | Evaluate + correct used together without loop | Composable primitives |

### AC9: Python Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-011 | Integration | P0 | All reflection actions registered in `build_actions_registry()` | Discoverable actions |
| 001.2-INT-012 | Integration | P0 | Module imports without errors | Module health |
| 001.2-E2E-001 | E2E | P0 | JSON generation agent with schema reflection | Real-world use case |
| 001.2-E2E-002 | E2E | P1 | Code generation agent with LLM-as-judge reflection | Real-world use case |

### AC10: Rust Implementation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-INT-013 | Integration | P0 | Rust reflection.loop feature parity with Python | Cross-runtime consistency |
| 001.2-INT-014 | Integration | P0 | Rust schema evaluator matches Python behavior | Cross-runtime consistency |
| 001.2-E2E-003 | E2E | P1 | Same YAML agent runs identically in Python and Rust | Runtime parity verification |

---

## Error Handling & Edge Cases

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 001.2-UNIT-039 | Unit | P0 | Generator action throws exception | Error resilience |
| 001.2-UNIT-040 | Unit | P0 | Evaluator action throws exception | Error resilience |
| 001.2-UNIT-041 | Unit | P0 | Corrector action throws exception | Error resilience |
| 001.2-UNIT-042 | Unit | P0 | max_iterations = 0 raises configuration error | Invalid configuration |
| 001.2-UNIT-043 | Unit | P1 | max_iterations = 1 allows one attempt | Boundary condition |
| 001.2-UNIT-044 | Unit | P2 | Empty generator output handled | Edge case |
| 001.2-UNIT-045 | Unit | P2 | null evaluator result handled | Edge case |
| 001.2-E2E-004 | E2E | P2 | Reflection loop with checkpoint interruption | Human-in-the-loop |
| 001.2-E2E-005 | E2E | P2 | Reflection loop resumes from checkpoint | Persistence |
| 001.2-E2E-006 | E2E | P3 | Large reflection_history (100+ iterations) performance | Stress test |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Infinite loop if circuit breaker fails | 001.2-UNIT-003, 001.2-UNIT-028 |
| Errors not propagated to corrector | 001.2-UNIT-005, 001.2-UNIT-009, 001.2-UNIT-027 |
| return_best selects wrong attempt | 001.2-UNIT-030, 001.2-UNIT-029 |
| Schema validation inconsistent with validate.schema | 001.2-INT-003 |
| Python/Rust behavior mismatch | 001.2-INT-013, 001.2-INT-014, 001.2-E2E-003 |
| LLM response parsing fails silently | 001.2-UNIT-015, 001.2-UNIT-018 |
| State corruption during iteration | 001.2-INT-006 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Core loop: 001.2-UNIT-001 through 001.2-UNIT-007
   - Schema evaluator: 001.2-UNIT-008 through 001.2-UNIT-010
   - Iteration tracking: 001.2-UNIT-025 through 001.2-UNIT-028
   - On-failure strategies: 001.2-UNIT-030 through 001.2-UNIT-032
   - Error handling: 001.2-UNIT-039 through 001.2-UNIT-042

2. **P0 Integration tests**
   - 001.2-INT-001, 001.2-INT-003, 001.2-INT-006, 001.2-INT-007
   - 001.2-INT-011, 001.2-INT-012, 001.2-INT-013, 001.2-INT-014

3. **P0 E2E tests**
   - 001.2-E2E-001

4. **P1 tests in order**
   - All remaining P1 unit tests
   - P1 integration tests
   - P1 E2E tests

5. **P2+ as time permits**

---

## Gate YAML Block

```yaml
test_design:
  story_id: "TEA-AGENT-001.2"
  scenarios_total: 52
  by_level:
    unit: 32
    integration: 14
    e2e: 6
  by_priority:
    p0: 22
    p1: 20
    p2: 8
    p3: 2
  coverage_gaps: []
  ac_coverage:
    ac1: [001.2-UNIT-001, 001.2-UNIT-002, 001.2-UNIT-003, 001.2-UNIT-004, 001.2-UNIT-005, 001.2-UNIT-006, 001.2-UNIT-007, 001.2-INT-001, 001.2-INT-002]
    ac2: [001.2-UNIT-008, 001.2-UNIT-009, 001.2-UNIT-010, 001.2-UNIT-011, 001.2-UNIT-012, 001.2-UNIT-013, 001.2-INT-003]
    ac3: [001.2-UNIT-014, 001.2-UNIT-015, 001.2-UNIT-016, 001.2-UNIT-017, 001.2-UNIT-018, 001.2-INT-004]
    ac4: [001.2-UNIT-019, 001.2-UNIT-020, 001.2-UNIT-021, 001.2-UNIT-022, 001.2-UNIT-023, 001.2-UNIT-024, 001.2-INT-005]
    ac5: [001.2-UNIT-025, 001.2-UNIT-026, 001.2-UNIT-027, 001.2-UNIT-028, 001.2-UNIT-029, 001.2-INT-006]
    ac6: [001.2-UNIT-030, 001.2-UNIT-031, 001.2-UNIT-032, 001.2-UNIT-033, 001.2-INT-007]
    ac7: [001.2-UNIT-034, 001.2-UNIT-035, 001.2-UNIT-036, 001.2-INT-008]
    ac8: [001.2-UNIT-037, 001.2-UNIT-038, 001.2-INT-009, 001.2-INT-010]
    ac9: [001.2-INT-011, 001.2-INT-012, 001.2-E2E-001, 001.2-E2E-002]
    ac10: [001.2-INT-013, 001.2-INT-014, 001.2-E2E-003]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.2-test-design-20260105.md
P0 tests identified: 22
Total test scenarios: 52
AC coverage: 100% (all 10 ACs have test coverage)
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (`{epic}.{story}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Error handling scenarios included
- [x] Cross-runtime parity tests included (Python/Rust)
