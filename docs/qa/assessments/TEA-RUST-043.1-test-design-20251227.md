# Test Design: Story TEA-RUST-043.1

Date: 2025-12-27
Designer: Quinn (Test Architect)
Story: Extract Template Processing Module

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 12 (67%)
- Integration tests: 5 (28%)
- E2E tests: 1 (5%)
- Priority distribution: P0: 6, P1: 8, P2: 4

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Template caching breaks | Medium | High | Verify cache hit/miss behavior |
| Thread safety regression | Low | Critical | Concurrent access tests |
| Tera context format changes | Low | High | Test all variable access patterns |
| Deadlock in RwLock | Low | Critical | Test nested render scenarios |

## Test Scenarios by Acceptance Criteria

### AC 1-3: Module Creation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-UNIT-001 | Unit | P1 | Verify `yaml_templates.rs` compiles | Basic module validation |
| 043.1-UNIT-002 | Unit | P1 | Verify module under 300 lines | Size constraint validation |

### AC 4-8: TemplateProcessor Struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-UNIT-003 | Unit | P0 | `TemplateProcessor::new()` creates valid instance | Core constructor |
| 043.1-UNIT-004 | Unit | P0 | `render()` processes simple template `{{ state.key }}` | Core functionality |
| 043.1-UNIT-005 | Unit | P0 | `process_params()` handles nested HashMap | Recursive processing |
| 043.1-UNIT-006 | Unit | P0 | `eval_condition()` returns correct boolean | Condition evaluation |

### AC 9-14: Template Features Preserved

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-INT-001 | Integration | P0 | `{{ state.key }}` access identical to yaml.rs | Behavioral equivalence |
| 043.1-INT-002 | Integration | P0 | `{{ variables.key }}` and `{{ secrets.key }}` work | Variable context |
| 043.1-UNIT-007 | Unit | P1 | `{{ checkpoint.dir }}` and `{{ checkpoint.last }}` work | Checkpoint context |
| 043.1-UNIT-008 | Unit | P1 | Tera filters (`| json_encode`, `| upper`) work | Filter preservation |
| 043.1-UNIT-009 | Unit | P1 | Template cache returns same result for same input | Cache correctness |
| 043.1-UNIT-010 | Unit | P1 | Error messages include template context | Error quality |

### AC 15-17: Thread Safety

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-INT-003 | Integration | P1 | Concurrent `render()` calls don't corrupt cache | Thread safety |
| 043.1-UNIT-011 | Unit | P1 | `RwLock::read()` allows multiple readers | Lock behavior |
| 043.1-UNIT-012 | Unit | P2 | Nested render calls don't deadlock | Deadlock prevention |

### AC 18-21: YamlEngine Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-INT-004 | Integration | P1 | `YamlEngine::render_template()` delegates correctly | Delegation pattern |
| 043.1-INT-005 | Integration | P1 | `YamlEngine::eval_condition()` delegates correctly | Delegation pattern |

### AC 22-24: Backward Compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-E2E-001 | E2E | P0 | All existing `test_render_template*` tests pass | Regression prevention |

### AC 25-27: Code Quality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 043.1-UNIT-013 | Unit | P2 | `cargo clippy` passes with no warnings | Code quality |
| 043.1-UNIT-014 | Unit | P2 | No `unwrap()` in non-test code | Error handling |

## Existing Tests to Verify

These existing tests MUST pass unchanged after refactoring:

```rust
// From rust/src/engine/yaml.rs
test_render_template
test_process_params
test_eval_condition_jinja2_template
test_eval_condition_bare_expression
test_eval_condition_simple_variable
test_eval_condition_negation
test_eval_condition_comparison_operators
test_eval_condition_logical_operators
test_eval_condition_truthy_falsy
test_eval_condition_empty_expression
test_eval_condition_block_template
test_cache_hit_returns_same_result
test_different_templates_cached_separately
test_template_compilation_error_not_cached
test_concurrent_cache_access
```

## Recommended Execution Order

1. P0 Unit tests (fail fast on core functionality)
2. P0 Integration tests (delegation verification)
3. P0 E2E test (full regression suite)
4. P1 tests (thread safety, features)
5. P2 tests (code quality)

## Test Commands

```bash
# Run all template-related tests
cd rust && cargo test render_template eval_condition process_params cache

# Run with thread sanitizer (if available)
RUSTFLAGS="-Z sanitizer=thread" cargo +nightly test -Zbuild-std --target x86_64-unknown-linux-gnu

# Verify clippy
cargo clippy -- -D warnings
```

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-043.1
  scenarios_total: 18
  by_level:
    unit: 12
    integration: 5
    e2e: 1
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  existing_tests_required: 15
  risk_level: medium
```
