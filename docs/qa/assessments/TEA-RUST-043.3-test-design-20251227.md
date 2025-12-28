# Test Design: Story TEA-RUST-043.3

Date: 2025-12-27
Designer: Quinn (Test Architect)
Story: Extract Edge Factory Module

## Test Strategy Overview

- Total test scenarios: 24
- Unit tests: 14 (58%)
- Integration tests: 9 (38%)
- E2E tests: 1 (4%)
- Priority distribution: P0: 8, P1: 12, P2: 4

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Goto precedence breaks | High | Critical | Test all precedence combinations |
| Implicit chaining regresses | Medium | High | Test single/multi-node graphs |
| Conditional routing fails | Medium | High | Test all goto variants |
| Target validation misses invalid | Low | Medium | Test invalid target scenarios |

## Test Scenarios by Acceptance Criteria

### AC 1-3: Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-UNIT-001 | Unit | P1 | Verify `yaml_edges.rs` compiles | Basic module validation |
| 043.3-UNIT-002 | Unit | P1 | Verify module under 450 lines | Size constraint |

### AC 4-8: EdgeFactory Struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-UNIT-003 | Unit | P0 | `EdgeFactory::new()` creates valid instance | Constructor |
| 043.3-UNIT-004 | Unit | P0 | `process_goto_and_implicit_edges()` handles empty nodes | Edge case |
| 043.3-UNIT-005 | Unit | P0 | `add_edge()` creates correct edge | Core functionality |
| 043.3-UNIT-006 | Unit | P1 | `infer_entry_finish()` sets entry point | Entry inference |

### AC 9-14: Goto Processing (TEA-YAML-002)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-INT-001 | Integration | P0 | String goto `goto: "target"` works | Unconditional goto |
| 043.3-INT-002 | Integration | P0 | List goto `goto: [{if:..., to:...}]` works | Conditional goto |
| 043.3-INT-003 | Integration | P1 | Fallback rule (no condition) works | Fallback behavior |
| 043.3-UNIT-007 | Unit | P0 | Target validation catches invalid targets | Validation |
| 043.3-UNIT-008 | Unit | P1 | `__end__` target works correctly | Special target |
| 043.3-INT-004 | Integration | P0 | Goto precedence over legacy edges | Precedence rule |

### AC 15-18: Implicit Chaining

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-INT-005 | Integration | P1 | Entry point set to first node (no __start__) | Auto-entry |
| 043.3-INT-006 | Integration | P1 | Implicit chain to next node | Chain behavior |
| 043.3-INT-007 | Integration | P1 | Last node goes to END | Auto-finish |
| 043.3-UNIT-009 | Unit | P1 | Nodes with explicit edges skip implicit | Skip logic |

### AC 19-23: Edge Types Supported

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-UNIT-010 | Unit | P1 | Normal edges work | Basic edge |
| 043.3-UNIT-011 | Unit | P1 | Parallel edges work | Fan-out pattern |
| 043.3-UNIT-012 | Unit | P1 | Conditional edges with `condition:` work | Conditional routing |
| 043.3-UNIT-013 | Unit | P1 | Entry edges from `__start__` work | Entry edge |
| 043.3-UNIT-014 | Unit | P1 | Finish edges to `__end__` work | Finish edge |

### AC 24-27: YamlEngine Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-INT-008 | Integration | P1 | Edge processing delegated to factory | Delegation |
| 043.3-INT-009 | Integration | P1 | `infer_entry_finish()` called correctly | Integration |

### AC 28-30: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-E2E-001 | E2E | P0 | All existing edge/goto tests pass | Regression |

### Code Quality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.3-UNIT-015 | Unit | P2 | `cargo clippy` passes | Code quality |
| 043.3-UNIT-016 | Unit | P2 | Error messages unchanged | Message compat |

## Existing Tests to Verify

```rust
// From rust/src/engine/yaml.rs
test_conditional_start_edges
test_parse_parallel_edges

// TEA-YAML-002 goto tests (if implemented in Rust)
// Edge-related integration tests
```

## Precedence Test Matrix

| Scenario | Goto | Legacy Edge | Implicit | Expected Behavior |
|----------|------|-------------|----------|-------------------|
| All three | Yes | Yes | Would apply | Goto wins |
| No goto | No | Yes | Would apply | Legacy edge wins |
| None explicit | No | No | Yes | Implicit chain |
| Goto to END | Yes | N/A | N/A | Goto to __end__ |

## Recommended Execution Order

1. P0 Unit tests (factory, validation)
2. P0 Integration tests (goto processing)
3. P0 E2E test (full regression)
4. P1 tests (implicit chaining, edge types)
5. P2 tests (code quality)

## Test Commands

```bash
# Run edge-related tests
cd rust && cargo test edge goto parallel conditional

# Run start/finish tests
cargo test start_edges finish

# Verify clippy
cargo clippy -- -D warnings
```

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-043.3
  scenarios_total: 24
  by_level:
    unit: 14
    integration: 9
    e2e: 1
  by_priority:
    p0: 8
    p1: 12
    p2: 4
  coverage_gaps: []
  existing_tests_required: 2+
  risk_level: high
  critical_paths:
    - goto_precedence
    - implicit_chaining
  dependencies:
    - TEA-RUST-043.1
    - TEA-RUST-043.4
```
