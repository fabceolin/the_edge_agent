# Story TEA-AGENT-001.2: Reflection Loop Primitive

## Status

**Ready for Development**

## Story

**As a** YAML agent developer,
**I want** a built-in reflection loop primitive,
**so that** I can implement self-correcting agents without manually wiring evaluate→correct edge loops.

## Background

Self-correction is a fundamental agentic pattern (Chapter 4 of Agentic Design Patterns). Currently, users must:

1. Create separate generator, evaluator, and corrector nodes
2. Wire conditional edges between them
3. Implement iteration counting and circuit breaker logic
4. Handle failure states manually

This story introduces a single `reflection.loop` action that encapsulates the entire pattern.

## Acceptance Criteria

### AC1: `reflection.loop` Action
1. Executes generate→evaluate→correct cycle automatically
2. Configurable generator (any action or inline code)
3. Configurable evaluator (schema, LLM, or custom)
4. Configurable corrector (any action or inline code)
5. Maximum iteration limit with circuit breaker
6. Returns best result based on on_failure strategy

### AC2: Schema-Based Evaluator
1. Uses JSON Schema validation via `validate.schema`
2. Returns validation errors for corrector context
3. Supports $ref for external schema files
4. Type coercion attempts before failure

### AC3: LLM-Based Evaluator
1. Uses LLM to evaluate output quality
2. Configurable evaluation prompt
3. Returns structured feedback (pass/fail, reason, suggestions)
4. Supports few-shot examples for evaluation

### AC4: Custom Evaluator
1. Supports inline Python/Lua/Prolog code as evaluator
2. Receives generator output and returns evaluation result
3. Can access state for context-aware evaluation

### AC5: Iteration Tracking
1. State includes `reflection_iteration` counter
2. State includes `reflection_history` with all attempts
3. State includes `reflection_errors` with evaluation failures
4. Circuit breaker triggers after max_iterations

### AC6: On-Failure Strategies
1. `return_best`: Return highest-scoring attempt
2. `return_last`: Return final attempt regardless of score
3. `raise`: Raise ReflectionFailedError with history
4. Strategy configurable per reflection.loop

### AC7: `reflection.evaluate` Standalone Action
1. Can be used independently of reflection.loop
2. Same evaluator types (schema, LLM, custom)
3. Returns structured evaluation result

### AC8: `reflection.correct` Standalone Action
1. Can be used independently of reflection.loop
2. Receives original output and evaluation errors
3. Returns corrected output

### AC9: Python Implementation
1. New module: `python/src/the_edge_agent/actions/reflection_actions.py`
2. All actions registered in `build_actions_registry()`
3. Test coverage >90%

### AC10: Rust Implementation
1. New module: `rust/src/engine/actions/reflection_actions.rs`
2. Feature parity with Python

## Tasks / Subtasks

- [ ] **Task 1: Core Reflection Loop** (AC: 1, 5)
  - [ ] Implement `reflection.loop` action
  - [ ] Add iteration tracking to state
  - [ ] Implement circuit breaker logic
  - [ ] Add reflection_history accumulation
  - [ ] Unit tests for core loop

- [ ] **Task 2: Schema Evaluator** (AC: 2)
  - [ ] Implement schema evaluator type
  - [ ] Integrate with existing `validate.schema`
  - [ ] Support $ref for external schemas
  - [ ] Type coercion logic
  - [ ] Unit tests

- [ ] **Task 3: LLM Evaluator** (AC: 3)
  - [ ] Implement LLM evaluator type
  - [ ] Configurable evaluation prompt
  - [ ] Structured feedback parsing
  - [ ] Few-shot example support
  - [ ] Unit tests

- [ ] **Task 4: Custom Evaluator** (AC: 4)
  - [ ] Implement custom evaluator type
  - [ ] Support Python inline code
  - [ ] Support Lua inline code
  - [ ] Support Prolog inline code
  - [ ] Unit tests

- [ ] **Task 5: On-Failure Strategies** (AC: 6)
  - [ ] Implement `return_best` strategy
  - [ ] Implement `return_last` strategy
  - [ ] Implement `raise` strategy
  - [ ] Scoring mechanism for `return_best`
  - [ ] Unit tests

- [ ] **Task 6: Standalone Actions** (AC: 7, 8)
  - [ ] Implement `reflection.evaluate` action
  - [ ] Implement `reflection.correct` action
  - [ ] Integration tests

- [ ] **Task 7: Rust Implementation** (AC: 9, 10)
  - [ ] Create `reflection_actions.rs` module
  - [ ] Implement `reflection.loop`
  - [ ] Implement schema evaluator
  - [ ] Implement LLM evaluator
  - [ ] Implement custom evaluator
  - [ ] Unit and integration tests

- [ ] **Task 8: Documentation & Examples**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Create example: json-generation-with-reflection.yaml
  - [ ] Create example: code-generation-with-reflection.yaml
  - [ ] Create example: llm-as-judge-reflection.yaml

## Dev Notes

### Source Tree Context

**Python:**
```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py              # Add reflection_actions
│   ├── validation_actions.py    # Reference: validate.schema
│   ├── reflection_actions.py    # NEW: Reflection actions
│   └── ...
└── yaml_engine.py
```

**Rust:**
```
rust/src/engine/
├── actions/
│   ├── mod.rs
│   ├── validation_actions.rs    # Reference: validate.schema
│   └── reflection_actions.rs    # NEW: Reflection actions
```

### YAML Syntax Reference

```yaml
nodes:
  - name: generate_with_reflection
    action: reflection.loop
    with:
      # Generator configuration
      generator:
        action: llm.call           # Any action
        prompt: "Generate JSON..."
        # OR inline code:
        # run: |
        #   return {"data": ...}

      # Evaluator configuration
      evaluator:
        type: schema              # schema | llm | custom
        # For schema type:
        schema:
          type: object
          required: [name, email]
        # For LLM type:
        # prompt: "Evaluate if this is valid..."
        # model: gpt-4
        # For custom type:
        # run: |
        #   return {"valid": len(output.get("name", "")) > 0}

      # Corrector configuration
      corrector:
        action: llm.call
        prompt: |
          Fix this JSON based on errors:
          Original: {{ state.reflection_output }}
          Errors: {{ state.reflection_errors | tojson }}

      # Loop configuration
      max_iterations: 3           # Default: 3
      on_failure: return_best     # return_best | return_last | raise
```

### State Variables Set by Reflection Loop

| Variable | Type | Description |
|----------|------|-------------|
| `reflection_iteration` | int | Current iteration (1-based) |
| `reflection_output` | any | Current generator output |
| `reflection_errors` | list | Evaluation errors from current iteration |
| `reflection_history` | list | All attempts with outputs and scores |
| `reflection_best` | any | Best output so far (for return_best) |
| `reflection_best_score` | float | Score of best output |

### Scoring Mechanism

For `return_best` strategy:
- Schema evaluator: Score = 1.0 if valid, 0.0 if invalid
- LLM evaluator: Score from LLM response (0.0-1.0)
- Custom evaluator: Score from return value

### Related Stories
- TEA-BUILTIN-001.2: LLM Enhanced Actions
- TEA-YAML-005: Retry Loop Action (reference for circuit breaker)

## Testing

### Test File Locations
- Python: `python/tests/test_reflection_actions.py`
- Rust: `rust/tests/test_reflection_actions.rs`

### Test Categories

| Category | Count | Priority |
|----------|-------|----------|
| Core Loop | 8 | P0 |
| Schema Evaluator | 6 | P0 |
| LLM Evaluator | 6 | P1 |
| Custom Evaluator | 6 | P1 |
| On-Failure Strategies | 4 | P0 |
| Standalone Actions | 4 | P1 |
| Error Handling | 6 | P0 |

### Key Test Scenarios

1. **First-pass success** - Generator output passes evaluation immediately
2. **Correction success** - Corrector fixes issues within max_iterations
3. **Max iterations reached** - Circuit breaker triggers correctly
4. **Schema validation errors** - Errors passed to corrector correctly
5. **LLM evaluation parsing** - Structured feedback extracted correctly
6. **return_best selection** - Highest-scoring attempt returned
7. **raise strategy** - ReflectionFailedError contains full history

## QA Notes

**Test Architect:** Quinn | **Date:** 2026-01-05

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 52 |
| Unit tests | 32 (62%) |
| Integration tests | 14 (27%) |
| E2E tests | 6 (11%) |
| P0 (Critical) | 22 tests |
| P1 (High) | 20 tests |
| P2/P3 (Medium/Low) | 10 tests |
| AC coverage | 100% (all 10 ACs covered) |

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| **Infinite loop if circuit breaker fails** | Critical | 001.2-UNIT-003, 001.2-UNIT-028 |
| **Errors not propagated to corrector** | High | 001.2-UNIT-005, 001.2-UNIT-009, 001.2-UNIT-027 |
| **return_best selects wrong attempt** | High | 001.2-UNIT-030, 001.2-UNIT-029 |
| **Schema validation inconsistent with validate.schema** | Medium | 001.2-INT-003 |
| **Python/Rust behavior mismatch** | High | 001.2-INT-013, 001.2-INT-014, 001.2-E2E-003 |
| **LLM response parsing fails silently** | Medium | 001.2-UNIT-015, 001.2-UNIT-018 |
| **State corruption during iteration** | High | 001.2-INT-006 |

### Recommended Test Scenarios

**P0 Must-Have (22 tests):**
1. First-pass success and correction success flows
2. Circuit breaker at max_iterations boundary
3. Schema evaluator validation (valid/invalid/nested)
4. Iteration tracking variables (counter, history, errors)
5. All on-failure strategies (return_best, return_last, raise)
6. Exception handling in generator/evaluator/corrector
7. Action registry integration
8. Python/Rust feature parity

**Key E2E Scenarios:**
- JSON generation agent with schema reflection (real-world validation)
- Code generation agent with LLM-as-judge reflection
- Same YAML agent runs identically in Python and Rust

### Concerns and Blockers

| Type | Description | Recommendation |
|------|-------------|----------------|
| Dependency | LLM evaluator requires mock LLM provider for deterministic testing | Implement mock LLM before AC3 tests |
| Complexity | Three evaluator types (schema/LLM/custom) increase testing surface | Prioritize schema evaluator as reference implementation |
| Cross-runtime | Rust implementation must match Python behavior exactly | Create shared YAML test fixtures for parity verification |
| Performance | Stress test (100+ iterations) is P3 but should be monitored | Add performance baseline in CI for regression detection |

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-AGENT-001.2-test-design-20260105.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.2 | Added QA Notes section | Quinn (QA) |
| 2026-01-04 | 0.1 | Initial story draft | Sarah (PO) |
