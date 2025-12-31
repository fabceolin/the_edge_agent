# Test Design: Story TEA-RUST-043.4

Date: 2025-12-27
Designer: Quinn (Test Architect)
Story: Extract Configuration Structs Module

## Test Strategy Overview

- Total test scenarios: 19
- Unit tests: 15 (79%)
- Integration tests: 3 (16%)
- E2E tests: 1 (5%)
- Priority distribution: P0: 5, P1: 9, P2: 5

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Serde deserialization breaks | Medium | Critical | Test all YAML fixtures |
| Default values change | Low | High | Explicit default verification |
| Missing re-exports | Low | High | Import path tests |
| Attribute syntax error | Low | Critical | Compile-time catch |

## Test Scenarios by Acceptance Criteria

### AC 1-3: Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-UNIT-001 | Unit | P1 | Verify `yaml_config.rs` compiles | Basic validation |
| 043.4-UNIT-002 | Unit | P1 | Verify module under 350 lines | Size constraint |

### AC 4-10: Struct Definitions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-UNIT-003 | Unit | P0 | `YamlConfig` deserializes from valid YAML | Core struct |
| 043.4-UNIT-004 | Unit | P0 | `NodeConfig` deserializes with all fields | Node struct |
| 043.4-UNIT-005 | Unit | P1 | `ImportConfig` deserializes correctly | Import struct |
| 043.4-UNIT-006 | Unit | P1 | `Goto::Simple` variant works | Enum variant |
| 043.4-UNIT-007 | Unit | P1 | `Goto::Conditional` variant works | Enum variant |
| 043.4-UNIT-008 | Unit | P1 | `GotoRule` deserializes with `if` rename | Serde rename |
| 043.4-UNIT-009 | Unit | P1 | `EdgeConfig` deserializes correctly | Edge struct |
| 043.4-UNIT-010 | Unit | P1 | `ErrorPolicyConfig` uses defaults | Default behavior |

### AC 11-14: Serde Attributes Preserved

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-INT-001 | Integration | P0 | `#[serde(default)]` works for optional fields | Attribute test |
| 043.4-INT-002 | Integration | P0 | `#[serde(rename = "with")]` works for with_params | Attribute test |
| 043.4-UNIT-011 | Unit | P1 | `#[serde(untagged)]` on Goto works | Enum attribute |
| 043.4-UNIT-012 | Unit | P2 | Optional fields accept `null`/missing | Option handling |

### AC 15-20: Default Implementations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-UNIT-013 | Unit | P1 | `default_max_retries()` returns 3 | Default value |
| 043.4-UNIT-014 | Unit | P2 | `default_backoff_base()` returns 1000 | Default value |
| 043.4-UNIT-015 | Unit | P2 | `default_backoff_max()` returns 30000 | Default value |
| 043.4-UNIT-016 | Unit | P2 | `default_jitter()` returns true | Default value |
| 043.4-UNIT-017 | Unit | P2 | `ErrorPolicyConfig::default()` works | Default impl |

### AC 21-23: Public Exports

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-INT-003 | Integration | P1 | `pub use yaml_config::*` exports all | Re-export |

### AC 24-26: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.4-E2E-001 | E2E | P0 | All existing `test_parse_*` tests pass | Regression |

## YAML Fixture Verification

These YAML patterns MUST parse identically:

```yaml
# Minimal config
name: test
nodes: []

# Full config with all optional fields
name: full-test
description: "Test workflow"
variables:
  key: value
nodes:
  - name: node1
    uses: llm.call
    with:
      model: gpt-4
    retry:
      max_retries: 3
    goto: next_node

# Conditional goto
nodes:
  - name: router
    goto:
      - if: "{{ state.done }}"
        to: __end__
      - to: continue
```

## Existing Tests to Verify

```rust
// From rust/src/engine/yaml.rs
test_parse_simple_yaml
test_parse_yaml_with_action
test_parse_yaml_with_conditional
test_parse_yaml_with_retry
test_parse_yaml_with_imports
test_parse_parallel_edges
test_conditional_start_edges
```

## Recommended Execution Order

1. P0 Unit tests (core struct deserialization)
2. P0 Integration tests (serde attributes)
3. P0 E2E test (full parse regression)
4. P1 tests (all struct variants)
5. P2 tests (defaults, code quality)

## Test Commands

```bash
# Run parsing tests
cd rust && cargo test parse

# Run specific struct tests
cargo test YamlConfig NodeConfig EdgeConfig

# Verify all exports work
cargo test --lib -- yaml_config

# Verify clippy
cargo clippy -- -D warnings
```

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-043.4
  scenarios_total: 19
  by_level:
    unit: 15
    integration: 3
    e2e: 1
  by_priority:
    p0: 5
    p1: 9
    p2: 5
  coverage_gaps: []
  existing_tests_required: 7
  risk_level: medium
  blocks:
    - TEA-RUST-043.2
    - TEA-RUST-043.3
  recommended_order: implement_first
```
