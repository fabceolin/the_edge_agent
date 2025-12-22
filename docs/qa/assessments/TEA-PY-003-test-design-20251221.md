# Test Design: Story TEA-PY-003

Date: 2025-12-21
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 32
- Unit tests: 18 (56%)
- Integration tests: 11 (34%)
- E2E tests: 3 (10%)
- Priority distribution: P0: 12, P1: 14, P2: 6

## Summary

TEA-PY-003 implements a `type: while_loop` node for the Python YamlEngine to achieve parity with TEA-RUST-033. This feature enables autonomous iteration in workflow agents, which is a core building block for self-refining agent patterns.

**Risk Assessment:**
- **High risk**: Infinite loop prevention (safety-critical)
- **High risk**: Cross-runtime parity (core promise of the project)
- **Medium risk**: State mutation between iterations
- **Medium risk**: Event emission correctness

## Test Scenarios by Acceptance Criteria

### AC-1: `type: while_loop` nodes are parsed from YAML

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-001 | Unit | P0 | Parse valid while_loop node with all required fields | Core parsing - validates YAML structure recognition |
| TEA-PY-003-UNIT-002 | Unit | P1 | Parse while_loop node extracts WhileLoopConfig correctly | Verifies internal data structure population |
| TEA-PY-003-UNIT-003 | Unit | P1 | Parse while_loop with multi-node body creates sequential sub-nodes | Body parsing is fundamental to loop execution |

---

### AC-2: Loop condition is evaluated using Jinja2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-004 | Unit | P0 | Condition `state.count < 3` evaluates correctly with state.count=2 | Core condition evaluation logic |
| TEA-PY-003-UNIT-005 | Unit | P0 | Condition `not state.get('is_valid', False)` handles missing keys | Common pattern for optional state fields |
| TEA-PY-003-UNIT-006 | Unit | P1 | Condition with complex expression `state.quality >= 0.8 or state.attempts >= 5` | Tests compound boolean expressions |
| TEA-PY-003-INT-001 | Integration | P1 | Condition evaluation reuses existing Jinja2 `when:` infrastructure | Validates architectural alignment with edge conditions |

---

### AC-3: Loop body nodes execute sequentially on each iteration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-007 | Unit | P0 | Single-node body executes each iteration | Basic body execution |
| TEA-PY-003-INT-002 | Integration | P0 | Multi-node body executes in declared order | Critical for stateful workflows |
| TEA-PY-003-INT-003 | Integration | P1 | Body node with `uses:` action executes correctly | Tests action integration within loop |
| TEA-PY-003-INT-004 | Integration | P1 | Body node with `run:` Python code executes correctly | Tests code execution within loop |

---

### AC-4: Loop exits when condition evaluates to `False`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-008 | Unit | P0 | Loop exits immediately when condition is False before first iteration | Edge case - condition false from start |
| TEA-PY-003-INT-005 | Integration | P0 | Loop exits after N iterations when condition becomes False | Primary happy path |
| TEA-PY-003-UNIT-009 | Unit | P1 | Counter loop `state.count < 3` runs exactly 3 iterations from count=0 | Validates iteration count correctness |

---

### AC-5: Loop exits when `max_iterations` is reached

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-010 | Unit | P0 | Loop with `condition: "True"` stops at max_iterations | Safety guard - prevents infinite loops |
| TEA-PY-003-UNIT-011 | Unit | P0 | max_iterations=5 limits loop to exactly 5 iterations | Boundary condition test |
| TEA-PY-003-UNIT-012 | Unit | P1 | Loop reaching max_iterations returns last state, not error | Validates graceful exit (no exception) |

---

### AC-6: State from each iteration is passed to the next iteration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-INT-006 | Integration | P0 | Counter increment accumulates: count 0→1→2→3 across iterations | Core state propagation |
| TEA-PY-003-INT-007 | Integration | P1 | State modifications from body node A visible to body node B in same iteration | Intra-iteration state flow |
| TEA-PY-003-INT-008 | Integration | P1 | State deep-copied between iterations to prevent reference issues | Memory safety for complex state |

---

### AC-7: Final state after loop completion is passed to downstream nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-INT-009 | Integration | P0 | Node after while_loop receives final loop state | Graph connectivity validation |
| TEA-PY-003-E2E-001 | E2E | P1 | Full workflow: start → while_loop → downstream → end receives correct state | End-to-end state flow |

---

### AC-8: `max_iterations` is required; YAML parsing fails if missing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-013 | Unit | P0 | YAML without max_iterations raises ValueError | Safety validation |
| TEA-PY-003-UNIT-014 | Unit | P1 | Error message includes node name and "max_iterations" | Debuggability |

---

### AC-9: `max_iterations` must be positive integer (1-1000 range)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-015 | Unit | P0 | max_iterations=0 raises ValueError | Lower bound validation |
| TEA-PY-003-UNIT-016 | Unit | P0 | max_iterations=1001 raises ValueError | Upper bound validation |
| TEA-PY-003-UNIT-017 | Unit | P1 | max_iterations=-5 raises ValueError | Negative value rejection |
| TEA-PY-003-UNIT-018 | Unit | P2 | max_iterations="5" (string) raises ValueError | Type validation |

---

### AC-10: If loop body execution fails, error propagates immediately

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-INT-010 | Integration | P0 | Body node exception stops loop and propagates error | Error handling critical for debugging |
| TEA-PY-003-UNIT-019 | Unit | P1 | Error from iteration 2 does not re-execute iteration 1 | No retry semantics |

---

### AC-11: Nested while-loops are NOT supported (validation error)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-020 | Unit | P0 | while_loop containing while_loop in body raises ValueError | Complexity guard |
| TEA-PY-003-UNIT-021 | Unit | P2 | Error message identifies both outer and inner loop names | Debuggability |

---

### AC-12: `LoopStart` event emitted with `{node_name, max_iterations}`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-022 | Unit | P1 | LoopStart event contains correct node_name | Observability verification |
| TEA-PY-003-UNIT-023 | Unit | P1 | LoopStart event contains correct max_iterations | Observability verification |

---

### AC-13: `LoopIteration` event emitted for each iteration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-024 | Unit | P1 | 3 iterations emit 3 LoopIteration events with iteration numbers 0,1,2 | Event count matches iterations |
| TEA-PY-003-UNIT-025 | Unit | P2 | LoopIteration includes condition_result boolean | Debugging support |

---

### AC-14: `LoopEnd` event emitted with `{node_name, iterations_completed, exit_reason}`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-UNIT-026 | Unit | P1 | LoopEnd with exit_reason='condition_false' when condition becomes false | Exit reason tracking |
| TEA-PY-003-UNIT-027 | Unit | P1 | LoopEnd with exit_reason='max_iterations_reached' when hitting limit | Exit reason tracking |
| TEA-PY-003-UNIT-028 | Unit | P2 | LoopEnd iterations_completed matches actual count | Metric accuracy |

---

### AC-15: Body node events are emitted normally

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-INT-011 | Integration | P1 | NodeStart/NodeComplete events emitted for each body node execution | Standard event integration |

---

### AC-16, AC-17, AC-18: Cross-Runtime Parity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-003-E2E-002 | E2E | P0 | Same YAML produces identical final state in Python and Rust | Core parity promise |
| TEA-PY-003-E2E-003 | E2E | P0 | Same YAML produces identical event sequence in Python and Rust | Behavioral parity |
| TEA-PY-003-UNIT-029 | Unit | P1 | YAML syntax matches TEA-RUST-033 specification exactly | Schema parity |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Infinite loop causing hang | TEA-PY-003-UNIT-010, TEA-PY-003-UNIT-011, TEA-PY-003-UNIT-013, TEA-PY-003-UNIT-015, TEA-PY-003-UNIT-016 |
| Cross-runtime divergence | TEA-PY-003-E2E-002, TEA-PY-003-E2E-003, TEA-PY-003-UNIT-029 |
| State corruption between iterations | TEA-PY-003-INT-006, TEA-PY-003-INT-007, TEA-PY-003-INT-008 |
| Error masking in loop | TEA-PY-003-INT-010, TEA-PY-003-UNIT-019 |
| Missing validation on required fields | TEA-PY-003-UNIT-013, TEA-PY-003-UNIT-020 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Parsing validation (UNIT-001, UNIT-013, UNIT-015, UNIT-016, UNIT-020)
   - Condition evaluation (UNIT-004, UNIT-005)
   - Exit conditions (UNIT-008, UNIT-010, UNIT-011)
   - Body execution (UNIT-007)
2. **P0 Integration tests** (validate component interactions)
   - Sequential body execution (INT-002)
   - State propagation (INT-006)
   - Downstream flow (INT-009)
   - Error propagation (INT-010)
3. **P0 E2E tests** (cross-runtime parity)
   - TEA-PY-003-E2E-002, TEA-PY-003-E2E-003
4. **P1 tests in order** (core features and observability)
5. **P2 tests as time permits** (edge cases and debuggability)

---

## Test File Organization

Recommended test file: `python/tests/test_yaml_engine_while_loop.py`

```python
"""
Tests for While-Loop Node (TEA-PY-003).

Test Categories:
- P0: Critical functionality (parsing validation, infinite loop prevention, parity)
- P1: Core loop features (condition evaluation, state propagation, events)
- P2: Edge cases (debuggability, error messages)
"""

class TestWhileLoopParsing:
    """AC-1, AC-8, AC-9, AC-11: YAML parsing and validation."""
    pass

class TestWhileLoopCondition:
    """AC-2, AC-4: Condition evaluation."""
    pass

class TestWhileLoopExecution:
    """AC-3, AC-5, AC-6, AC-7, AC-10: Body execution and state flow."""
    pass

class TestWhileLoopEvents:
    """AC-12, AC-13, AC-14, AC-15: Observability events."""
    pass

class TestWhileLoopCrossRuntime:
    """AC-16, AC-17, AC-18: Python-Rust parity tests."""
    pass
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-PY-003
  scenarios_total: 32
  by_level:
    unit: 18
    integration: 11
    e2e: 3
  by_priority:
    p0: 12
    p1: 14
    p2: 6
  coverage_gaps: []
  key_risks_mitigated:
    - infinite_loop_prevention
    - cross_runtime_parity
    - state_propagation_correctness
  designer: Quinn (Test Architect)
  date: 2025-12-21
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (unit for logic, integration for flows, E2E for parity)
- [x] No duplicate coverage across levels
- [x] Critical paths have multiple levels (parsing, execution, parity all have unit+integration)
- [x] Priorities align with business risk (safety guards and parity are P0)
- [x] Test IDs follow naming convention (`{STORY}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-PY-003-test-design-20251221.md`
P0 tests identified: 12
