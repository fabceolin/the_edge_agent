# Test Design: Story TEA-RUST-043.5

Date: 2025-12-27
Designer: Quinn (Test Architect)
Story: Extract Engine Builder Module

## Test Strategy Overview

- Total test scenarios: 16
- Unit tests: 8 (50%)
- Integration tests: 7 (44%)
- E2E tests: 1 (6%)
- Priority distribution: P0: 5, P1: 8, P2: 3

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Build order changed | Medium | Critical | Verify exact operation sequence |
| Observability not attached | Low | High | Explicit observability test |
| Factory coordination breaks | Medium | High | Integration tests for wiring |
| Validation not called | Low | Medium | Verify warning output |

## Test Scenarios by Acceptance Criteria

### AC 1-3: Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-UNIT-001 | Unit | P1 | Verify `yaml_builder.rs` compiles | Basic validation |
| 043.5-UNIT-002 | Unit | P1 | Verify module under 200 lines | Size constraint |

### AC 4-8: GraphBuilder Struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-UNIT-003 | Unit | P0 | `GraphBuilder::new()` accepts TemplateProcessor | Constructor |
| 043.5-UNIT-004 | Unit | P0 | `build()` returns valid StateGraph | Core functionality |
| 043.5-INT-001 | Integration | P0 | Builder uses NodeFactory correctly | Factory wiring |
| 043.5-INT-002 | Integration | P0 | Builder uses EdgeFactory correctly | Factory wiring |

### AC 9-13: Orchestration Logic

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-INT-003 | Integration | P1 | Nodes added in declaration order | Order preservation |
| 043.5-INT-004 | Integration | P1 | Goto edges processed before legacy | Precedence |
| 043.5-INT-005 | Integration | P1 | Entry/finish inferred correctly | Inference |
| 043.5-UNIT-005 | Unit | P1 | Observability config attached to graph | Config attachment |

### AC 14-16: Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-UNIT-006 | Unit | P2 | Orphan nodes generate warning | Warning detection |
| 043.5-UNIT-007 | Unit | P2 | Error messages include context | Error quality |

### AC 17-19: YamlEngine Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-INT-006 | Integration | P1 | `load_from_string()` uses GraphBuilder | Delegation |
| 043.5-INT-007 | Integration | P1 | Template processor passed to builder | Dependency |

### AC 20-22: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-E2E-001 | E2E | P0 | All existing YAML parse/build tests pass | Regression |

### Code Quality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.5-UNIT-008 | Unit | P2 | `cargo clippy` passes | Code quality |

## Build Order Verification

The build process MUST follow this exact order:

```
1. Create StateGraph
2. Set name and description
3. Create NodeFactory
4. Create EdgeFactory
5. Build and add each node (in order)
6. Process goto and implicit edges
7. Process legacy edges
8. Infer entry/finish points
9. Attach observability config
10. Validate (warnings only)
11. Return graph
```

## Existing Tests to Verify

```rust
// All parsing tests exercise the builder
test_parse_simple_yaml
test_parse_yaml_with_action
test_parse_yaml_with_conditional
test_parse_yaml_with_retry
test_parse_yaml_with_imports
test_parse_parallel_edges
test_conditional_start_edges

// Integration tests (if applicable)
// End-to-end YAML execution tests
```

## Recommended Execution Order

1. P0 Unit tests (constructor, basic build)
2. P0 Integration tests (factory wiring)
3. P0 E2E test (full regression)
4. P1 tests (orchestration, delegation)
5. P2 tests (validation, code quality)

## Test Commands

```bash
# Run all build-related tests
cd rust && cargo test parse build

# Run full test suite (this story depends on all others)
cargo test

# Verify observability attachment
cargo test observability

# Verify clippy
cargo clippy -- -D warnings
```

## Dependency Matrix

This story is the final integration point:

| Dependency | Required For |
|------------|--------------|
| TEA-RUST-043.1 | TemplateProcessor |
| TEA-RUST-043.2 | NodeFactory |
| TEA-RUST-043.3 | EdgeFactory |
| TEA-RUST-043.4 | YamlConfig |

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-043.5
  scenarios_total: 16
  by_level:
    unit: 8
    integration: 7
    e2e: 1
  by_priority:
    p0: 5
    p1: 8
    p2: 3
  coverage_gaps: []
  existing_tests_required: 7+
  risk_level: medium
  dependencies:
    - TEA-RUST-043.1
    - TEA-RUST-043.2
    - TEA-RUST-043.3
    - TEA-RUST-043.4
  recommended_order: implement_last
```
