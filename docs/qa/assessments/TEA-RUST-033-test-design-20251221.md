# Test Design: Story TEA-RUST-033

Date: 2025-12-21
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 34
- Unit tests: 18 (53%)
- Integration tests: 12 (35%)
- E2E tests: 4 (12%)
- Priority distribution: P0: 12, P1: 14, P2: 8

## Test Scenarios by Acceptance Criteria

### AC-1: `type: while_loop` nodes are parsed from YAML and stored in `Node` struct

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-001 | Unit | P0 | Parse valid while_loop node with all required fields | Pure YAML parsing logic - validates struct population |
| TEA-RUST-033-UNIT-002 | Unit | P0 | Verify NodeType::WhileLoop variant stores condition correctly | Pure struct field validation |
| TEA-RUST-033-UNIT-003 | Unit | P0 | Verify NodeType::WhileLoop variant stores max_iterations correctly | Pure struct field validation |
| TEA-RUST-033-UNIT-004 | Unit | P0 | Verify NodeType::WhileLoop variant stores body nodes correctly | Pure struct population with nested nodes |

### AC-2: Loop condition is evaluated using Tera (same as `when:` conditions)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-005 | Unit | P0 | Condition evaluates to true continues loop | Pure Tera expression evaluation logic |
| TEA-RUST-033-UNIT-006 | Unit | P0 | Condition evaluates to false exits loop | Pure Tera expression evaluation logic |
| TEA-RUST-033-UNIT-007 | Unit | P1 | Condition with state.field access works correctly | Tera template variable interpolation |
| TEA-RUST-033-UNIT-008 | Unit | P1 | Condition with complex boolean expression (and/or/not) | Tera expression evaluation edge case |
| TEA-RUST-033-INT-001 | Integration | P1 | Condition uses same Tera engine as `when:` conditions | Component interaction validation |

### AC-3: Loop body nodes execute sequentially on each iteration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-002 | Integration | P0 | Body with single node executes each iteration | Multi-component flow: executor + loop + node |
| TEA-RUST-033-INT-003 | Integration | P0 | Body with multiple nodes executes in order per iteration | Sequential execution within body |
| TEA-RUST-033-INT-004 | Integration | P1 | Body node 2 receives state from body node 1 within same iteration | State propagation within loop body |

### AC-4: Loop exits when condition evaluates to `false`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-009 | Unit | P0 | Loop exits immediately when condition is false on first check | Core loop termination logic |
| TEA-RUST-033-INT-005 | Integration | P0 | Loop iterates until body modifies state causing condition to become false | Full iteration flow validation |

### AC-5: Loop exits when `max_iterations` is reached (returns last state, no error)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-010 | Unit | P0 | Loop exits at max_iterations with always-true condition | Safety guard validation |
| TEA-RUST-033-UNIT-011 | Unit | P1 | Last state is returned after max_iterations exit | State preservation on safety exit |
| TEA-RUST-033-UNIT-012 | Unit | P1 | No error is thrown when max_iterations is reached | Graceful termination verification |

### AC-6: State from each iteration is passed to the next iteration

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-006 | Integration | P0 | Iteration N+1 receives state modified by iteration N | Critical state flow between iterations |
| TEA-RUST-033-INT-007 | Integration | P1 | Counter variable increments across iterations | Common pattern validation |
| TEA-RUST-033-INT-008 | Integration | P1 | Array/list accumulates values across iterations | Complex state type handling |

### AC-7: Final state after loop completion is passed to downstream nodes

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-009 | Integration | P0 | Node after while_loop receives final loop state | Critical downstream state propagation |
| TEA-RUST-033-E2E-001 | E2E | P1 | Complete workflow: start -> while_loop -> process_result -> end | Critical path validation |

### AC-8: `max_iterations` is required; YAML parsing fails if missing

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-013 | Unit | P0 | YAML without max_iterations returns appropriate error | Safety validation - pure parsing |
| TEA-RUST-033-UNIT-014 | Unit | P1 | Error message clearly indicates missing max_iterations | Error quality for developer experience |

### AC-9: `max_iterations` must be positive integer (1-1000 range)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-015 | Unit | P0 | max_iterations = 0 is rejected | Boundary validation |
| TEA-RUST-033-UNIT-016 | Unit | P1 | max_iterations = 1 is accepted (lower bound) | Boundary validation |
| TEA-RUST-033-UNIT-017 | Unit | P1 | max_iterations = 1000 is accepted (upper bound) | Boundary validation |
| TEA-RUST-033-UNIT-018 | Unit | P1 | max_iterations = 1001 is rejected (exceeds limit) | Boundary validation |

### AC-10: If loop body execution fails, error propagates immediately (no retry by loop itself)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-010 | Integration | P0 | Body node error halts loop immediately | Error propagation flow |
| TEA-RUST-033-INT-011 | Integration | P1 | Error includes context about which iteration failed | Debuggability validation |

### AC-11: Nested while-loops are NOT supported (validation error if attempted)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-019 | Unit | P0 | YAML with nested while_loop in body returns validation error | Complexity guard - pure validation |
| TEA-RUST-033-UNIT-020 | Unit | P2 | Error message clearly states nested loops not supported | Error quality |

### AC-12: `LoopStart` event emitted with `{node_name, max_iterations}`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-012 | Integration | P1 | LoopStart event emitted before first iteration | Event timing and content |

### AC-13: `LoopIteration` event emitted for each iteration `{node_name, iteration, condition_result}`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-013 | Integration | P1 | LoopIteration events count matches actual iterations | Event accuracy |
| TEA-RUST-033-INT-014 | Integration | P2 | LoopIteration event contains correct iteration number | Event content validation |

### AC-14: `LoopEnd` event emitted with `{node_name, iterations_completed, exit_reason}`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-015 | Integration | P1 | LoopEnd event with exit_reason="condition_false" when condition exits | Event content accuracy |
| TEA-RUST-033-INT-016 | Integration | P1 | LoopEnd event with exit_reason="max_iterations_reached" at limit | Event content accuracy |

### AC-15: Body node events are emitted normally (NodeStart, NodeComplete, etc.)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-INT-017 | Integration | P2 | Body node Start/Complete events fire each iteration | Standard event contract |

### AC-16: Rust implementation matches Python behavior exactly

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-E2E-002 | E2E | P0 | Same YAML produces identical final state in Rust and Python | Cross-runtime parity - critical |

### AC-17: Same YAML file produces identical results in both runtimes

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-E2E-003 | E2E | P0 | Counter loop example produces same count in both runtimes | Parity with concrete example |
| TEA-RUST-033-E2E-004 | E2E | P1 | max_iterations exit produces same state in both runtimes | Parity for edge case |

### AC-18: Python implementation in TEA-PY-003 uses same syntax

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-033-UNIT-021 | Unit | P2 | YAML syntax matches Python parser expectations | Syntax compatibility (via shared example files) |

## Risk Coverage

| Risk | Mitigated By |
|------|--------------|
| Infinite loops crashing agent | TEA-RUST-033-UNIT-010 (max_iterations enforcement) |
| State corruption across iterations | TEA-RUST-033-INT-006, TEA-RUST-033-INT-007 |
| Silent failures in body nodes | TEA-RUST-033-INT-010 (error propagation) |
| Cross-runtime inconsistency | TEA-RUST-033-E2E-002, TEA-RUST-033-E2E-003 |
| Nested loop complexity explosion | TEA-RUST-033-UNIT-019 (nested loop rejection) |
| Missing safety guards | TEA-RUST-033-UNIT-013 (required max_iterations) |

## Recommended Execution Order

1. **P0 Unit tests (fail fast)**
   - YAML parsing tests (TEA-RUST-033-UNIT-001 through 004)
   - Condition evaluation (TEA-RUST-033-UNIT-005, 006, 009, 010)
   - Safety validation (TEA-RUST-033-UNIT-013, 015, 019)

2. **P0 Integration tests**
   - Body execution (TEA-RUST-033-INT-002, 003, 005, 006, 009, 010)

3. **P0 E2E tests**
   - Cross-runtime parity (TEA-RUST-033-E2E-002, 003)

4. **P1 tests in order**
   - Remaining unit tests for edge cases
   - Event emission tests
   - Downstream state propagation

5. **P2+ as time permits**
   - Event content validation
   - Syntax compatibility
   - Error message quality

## Test Implementation Notes

### Unit Test File Location
`rust/tests/test_while_loop.rs` - New dedicated test file

### Integration Test Strategy
- Use existing `Executor` test patterns from `test_stategraph.rs`
- Mock LuaRuntime for condition evaluation isolation
- Capture events via ExecutionEvent stream

### E2E / Cross-Runtime Tests
- Create shared YAML fixtures in `examples/while-loop/`
- Run same YAML through both Python and Rust executors
- Compare final state JSON equality

### Test Data Requirements
- Counter increment workflow (basic case)
- Validation loop (realistic LLM retry pattern)
- Always-true condition (max_iterations guard test)
- Empty body (edge case)
- Multi-node body (sequential execution)

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 18
    integration: 17
    e2e: 4
  by_priority:
    p0: 12
    p1: 14
    p2: 8
  coverage_gaps: []
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-RUST-033-test-design-20251221.md
P0 tests identified: 12
