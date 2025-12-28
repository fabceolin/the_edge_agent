# Test Design: Story TEA-RUST-043.2

Date: 2025-12-27
Designer: Quinn (Test Architect)
Story: Extract Node Factory Module

## Test Strategy Overview

- Total test scenarios: 21
- Unit tests: 14 (67%)
- Integration tests: 6 (28%)
- E2E tests: 1 (5%)
- Priority distribution: P0: 7, P1: 10, P2: 4

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| While-loop validation breaks | Medium | High | Preserve exact error messages |
| Language detection regresses | Low | Medium | Test all detection patterns |
| Node metadata lost | Low | High | Verify metadata passthrough |
| Retry config not attached | Low | High | Explicit retry verification |

## Test Scenarios by Acceptance Criteria

### AC 1-3: Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-UNIT-001 | Unit | P1 | Verify `yaml_nodes.rs` compiles | Basic module validation |
| 043.2-UNIT-002 | Unit | P1 | Verify module under 250 lines | Size constraint |

### AC 4-7: NodeFactory Struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-UNIT-003 | Unit | P0 | `NodeFactory::new()` accepts TemplateProcessor | Constructor validation |
| 043.2-UNIT-004 | Unit | P0 | `build_node()` creates standard Node | Core functionality |
| 043.2-UNIT-005 | Unit | P0 | `build_while_loop_node()` creates while-loop Node | While-loop support |

### AC 8-13: Node Types Supported

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-INT-001 | Integration | P0 | Standard nodes with `run:` inline code work | Core node type |
| 043.2-INT-002 | Integration | P0 | Action nodes with `uses:` work | Action registration |
| 043.2-INT-003 | Integration | P1 | While-loop nodes with `type: while_loop` work | Complex node type |
| 043.2-UNIT-006 | Unit | P1 | Node metadata preserved in built node | Metadata passthrough |
| 043.2-UNIT-007 | Unit | P1 | Retry configuration attached correctly | Retry attachment |
| 043.2-UNIT-008 | Unit | P1 | Fallback node references preserved | Fallback handling |

### AC 14-17: Language Detection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-UNIT-009 | Unit | P1 | Lua code detection (default) | Detection logic |
| 043.2-UNIT-010 | Unit | P1 | Prolog detection via `:-` pattern | Pattern matching |
| 043.2-UNIT-011 | Unit | P1 | Prolog detection via `?-` pattern | Pattern matching |
| 043.2-UNIT-012 | Unit | P1 | Explicit `language:` field overrides detection | Override behavior |
| 043.2-UNIT-013 | Unit | P2 | Unsupported language returns error | Error handling |

### AC 18-21: While-Loop Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-UNIT-014 | Unit | P0 | Missing `max_iterations` returns error | Required field |
| 043.2-UNIT-015 | Unit | P0 | Missing `condition` returns error | Required field |
| 043.2-UNIT-016 | Unit | P1 | Missing `body` returns error | Required field |
| 043.2-UNIT-017 | Unit | P1 | Error messages match TEA-RUST-033 format | Message compatibility |

### AC 22-24: YamlEngine Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-INT-004 | Integration | P1 | `YamlEngine::build_node()` delegates to factory | Delegation pattern |
| 043.2-INT-005 | Integration | P1 | Factory receives template processor | Dependency injection |

### AC 25-27: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-E2E-001 | E2E | P0 | All existing `test_parse_yaml_with_*` tests pass | Regression prevention |

### Code Quality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.2-UNIT-018 | Unit | P2 | `cargo clippy` passes | Code quality |

## Existing Tests to Verify

```rust
// From rust/src/engine/yaml.rs
test_parse_simple_yaml
test_parse_yaml_with_action
test_parse_yaml_with_retry

// From rust/tests/ (if applicable)
// while_loop tests from TEA-RUST-033
```

## Recommended Execution Order

1. P0 Unit tests (core factory, while-loop validation)
2. P0 Integration tests (node type support)
3. P0 E2E test (full regression)
4. P1 tests (language detection, metadata)
5. P2 tests (code quality)

## Test Commands

```bash
# Run node-related tests
cd rust && cargo test build_node while_loop parse_yaml

# Run specific node type tests
cargo test --test integration -- node

# Verify clippy
cargo clippy -- -D warnings
```

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-043.2
  scenarios_total: 21
  by_level:
    unit: 14
    integration: 6
    e2e: 1
  by_priority:
    p0: 7
    p1: 10
    p2: 4
  coverage_gaps: []
  existing_tests_required: 4
  risk_level: medium
  dependencies:
    - TEA-RUST-043.1
    - TEA-RUST-043.4
```
