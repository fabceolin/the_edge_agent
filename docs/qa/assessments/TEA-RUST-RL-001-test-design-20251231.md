# Test Design: Story TEA-RUST-RL-001

Date: 2025-12-31
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 42
- Unit tests: 24 (57%)
- Integration tests: 14 (33%)
- E2E tests: 4 (10%)
- Priority distribution: P0: 18, P1: 16, P2: 8

## Story Context

**Story**: Rate Limit Wrap Action (Rust)
**Feature**: `ratelimit.wrap` action that enforces shared rate limits across parallel nodes using named limiters in the Rust runtime.

## Test Levels Rationale

| Component | Level | Justification |
|-----------|-------|---------------|
| `RateLimiter` struct | Unit | Pure timing/synchronization logic, no external dependencies |
| `RateLimiterRegistry` | Unit | Collection management with thread-safe access patterns |
| Interval calculation | Unit | Pure math functions (rpm/rps conversion) |
| `ratelimit.wrap` action | Integration | Requires ActionRegistry, wrapped action execution |
| Parallel flow execution | Integration | Rayon thread pool + registry interaction |
| Settings configuration | Integration | YAML parsing + executor initialization |
| Full YAML agent execution | E2E | Complete workflow with parallel branches + rate limiting |

---

## Test Scenarios by Acceptance Criteria

### AC-1: ratelimit.wrap action wraps any action with rate limiting

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-001 | Unit | P0 | `test_ratelimiter_new_creates_with_correct_interval` | Verify constructor initializes min_interval correctly |
| TEA-RUST-RL-001-UNIT-002 | Unit | P0 | `test_ratelimiter_first_call_no_wait` | First call should return immediately (no prior request) |
| TEA-RUST-RL-001-UNIT-003 | Unit | P0 | `test_ratelimiter_second_call_waits_correct_duration` | Second call within interval should block and return wait time |
| TEA-RUST-RL-001-INT-001 | Integration | P0 | `test_ratelimit_wrap_executes_wrapped_action` | Verify wrapped action receives correct params and executes |
| TEA-RUST-RL-001-INT-002 | Integration | P0 | `test_ratelimit_wrap_waits_before_execution` | Verify wait occurs before wrapped action runs |

### AC-2: Named Limiters shared across all nodes using the same name

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-004 | Unit | P0 | `test_registry_get_or_create_new_limiter` | Creating new limiter returns fresh instance |
| TEA-RUST-RL-001-UNIT-005 | Unit | P0 | `test_registry_get_or_create_existing_returns_same` | Same name returns same Arc reference |
| TEA-RUST-RL-001-UNIT-006 | Unit | P1 | `test_registry_different_names_independent` | Different names create independent limiters |
| TEA-RUST-RL-001-INT-003 | Integration | P0 | `test_parallel_nodes_share_same_limiter` | Multiple parallel branches share limiter state |

### AC-3: RPM Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-007 | Unit | P0 | `test_calculate_interval_from_rpm_60` | 60 rpm = 1.0s interval |
| TEA-RUST-RL-001-UNIT-008 | Unit | P1 | `test_calculate_interval_from_rpm_120` | 120 rpm = 0.5s interval |
| TEA-RUST-RL-001-UNIT-009 | Unit | P1 | `test_calculate_interval_from_rpm_30` | 30 rpm = 2.0s interval |

### AC-4: RPS Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-010 | Unit | P0 | `test_calculate_interval_from_rps_10` | 10 rps = 0.1s interval |
| TEA-RUST-RL-001-UNIT-011 | Unit | P1 | `test_calculate_interval_from_rps_1` | 1 rps = 1.0s interval |
| TEA-RUST-RL-001-UNIT-012 | Unit | P1 | `test_rps_takes_precedence_over_rpm` | When both specified, rps wins |

### AC-5: Shared Across Parallel Nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-004 | Integration | P0 | `test_parallel_branches_sequential_execution` | 3 parallel branches with same limiter execute ~1s apart |
| TEA-RUST-RL-001-INT-005 | Integration | P0 | `test_parallel_total_time_matches_rate_limit` | Total time >= (n-1) * interval for n parallel calls |
| TEA-RUST-RL-001-E2E-001 | E2E | P0 | `test_yaml_parallel_fanout_respects_ratelimit` | Full YAML agent with parallel nodes shares rate limit |

### AC-6: Thread-Safe (std::sync::Mutex)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-013 | Unit | P0 | `test_ratelimiter_thread_safe_concurrent_access` | 10 threads calling wait() produce correct timing |
| TEA-RUST-RL-001-UNIT-014 | Unit | P0 | `test_registry_thread_safe_concurrent_access` | Multiple threads getting same limiter get same Arc |
| TEA-RUST-RL-001-UNIT-015 | Unit | P1 | `test_no_data_races_under_load` | Stress test with 100+ concurrent calls |

### AC-7: Fixed Window Strategy

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-016 | Unit | P0 | `test_fixed_window_constant_interval` | Calls maintain consistent spacing |
| TEA-RUST-RL-001-UNIT-017 | Unit | P1 | `test_fixed_window_no_burst_allowed` | Cannot make multiple requests without wait |

### AC-8: Per-Limiter Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-018 | Unit | P1 | `test_multiple_limiters_independent_rates` | "openai" at 60rpm, "anthropic" at 40rpm work independently |
| TEA-RUST-RL-001-INT-006 | Integration | P1 | `test_different_actions_different_rates` | Two wrapped actions with different limiters |

### AC-9: Limiter Registry at Executor Level

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-007 | Integration | P0 | `test_executor_contains_rate_limiter_registry` | Executor struct has rate_limiters field |
| TEA-RUST-RL-001-INT-008 | Integration | P1 | `test_registry_survives_parallel_execution` | Registry persists through Rayon parallel iteration |

### AC-10: Lazy Initialization

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-019 | Unit | P1 | `test_limiter_created_on_first_use` | Registry is empty until first get_or_create |
| TEA-RUST-RL-001-UNIT-020 | Unit | P2 | `test_no_upfront_allocation` | Registry HashMap doesn't preallocate |

### AC-11: Configuration Override (First-Config-Wins)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-021 | Unit | P0 | `test_first_config_wins_reuses_existing` | Second call with different interval returns existing |
| TEA-RUST-RL-001-UNIT-022 | Unit | P1 | `test_first_config_wins_logs_warning` | Warning logged when interval mismatch detected |

### AC-12: Settings Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-009 | Integration | P1 | `test_settings_rate_limiters_parsed` | YAML settings.rate_limiters section parsed correctly |
| TEA-RUST-RL-001-INT-010 | Integration | P1 | `test_settings_pre_initialize_limiters` | Pre-configured limiters available before first action |
| TEA-RUST-RL-001-E2E-002 | E2E | P1 | `test_yaml_agent_with_settings_configuration` | Full agent using settings-based rate limits |

### AC-13: Wait Time Metadata

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-011 | Integration | P1 | `test_response_includes_ratelimit_waited_ms` | Response has `_ratelimit_waited_ms` field |
| TEA-RUST-RL-001-UNIT-023 | Unit | P1 | `test_wait_returns_milliseconds` | Wait method returns f64 milliseconds |

### AC-14: Limiter Info Metadata

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-012 | Integration | P1 | `test_response_includes_ratelimit_limiter` | Response has `_ratelimit_limiter` field with name |

### AC-15: Timeout Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-024 | Unit | P0 | `test_timeout_returns_error_when_exceeded` | Wait returns TeaError::RateLimitTimeout |
| TEA-RUST-RL-001-UNIT-025 | Unit | P1 | `test_timeout_success_when_within_limit` | Wait succeeds if timeout >= required wait |
| TEA-RUST-RL-001-INT-013 | Integration | P0 | `test_timeout_error_contains_limiter_name` | Error message includes which limiter timed out |

### AC-16: Graceful Degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-UNIT-026 | Unit | P2 | `test_graceful_degradation_on_mutex_poison` | If Mutex poisoned, proceeds without limiting |
| TEA-RUST-RL-001-INT-014 | Integration | P2 | `test_graceful_degradation_logs_warning` | Warning logged when degrading |

### AC-17: Action Error Passthrough

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-015 | Integration | P0 | `test_wrapped_action_error_propagates` | TeaError from wrapped action passed through unchanged |
| TEA-RUST-RL-001-INT-016 | Integration | P1 | `test_error_type_preserved` | Error variant (InvalidInput, Http, etc.) preserved |

### AC-18: Dual Namespace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-INT-017 | Integration | P1 | `test_ratelimit_dot_wrap_registered` | `ratelimit.wrap` accessible via ActionRegistry |
| TEA-RUST-RL-001-INT-018 | Integration | P2 | `test_actions_ratelimit_wrap_registered` | `actions.ratelimit_wrap` also accessible |

### AC-19: Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-E2E-003 | E2E | P2 | `test_documentation_examples_work` | YAML examples from docs execute successfully |

### AC-20: Python Parity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-RL-001-E2E-004 | E2E | P0 | `test_python_yaml_runs_in_rust` | Same YAML that works in Python works in Rust |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Mutex contention under high load | TEA-RUST-RL-001-UNIT-015 (stress test) |
| Configuration mismatch confusion | TEA-RUST-RL-001-UNIT-021, TEA-RUST-RL-001-UNIT-022 (first-config-wins + warning) |
| Rayon thread pool interactions | TEA-RUST-RL-001-INT-004, TEA-RUST-RL-001-INT-005, TEA-RUST-RL-001-INT-008 |
| Python parity gap | TEA-RUST-RL-001-E2E-004 |

---

## Test Implementation Guidelines

### Unit Test File Location
`rust/src/actions/ratelimit.rs` (inline `#[cfg(test)]` module)

### Integration Test File Location
`rust/tests/test_ratelimit.rs`

### Test Dependencies
```toml
# Cargo.toml [dev-dependencies]
tokio = { version = "1", features = ["time", "rt-multi-thread"] }  # For async timing tests if needed
```

### Timing Test Strategy
Due to timing-sensitive tests:
1. Use intervals of 50-100ms for unit tests (fast but measurable)
2. Allow 10-20% tolerance on timing assertions
3. Use `std::time::Instant` for precise measurements
4. Run timing tests in isolation to avoid CI flakiness

### Thread Safety Test Strategy
```rust
#[test]
fn test_thread_safe_concurrent_access() {
    use std::sync::Arc;
    use std::thread;

    let limiter = Arc::new(RateLimiter::new(Duration::from_millis(50)));
    let mut handles = vec![];

    for _ in 0..10 {
        let l = Arc::clone(&limiter);
        handles.push(thread::spawn(move || {
            l.wait()
        }));
    }

    let results: Vec<_> = handles.into_iter()
        .map(|h| h.join().unwrap())
        .collect();

    // Verify all succeeded and timing is correct
    // ...
}
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - RateLimiter basics: UNIT-001 to UNIT-003
   - Registry basics: UNIT-004 to UNIT-005
   - Thread safety: UNIT-013 to UNIT-014
   - RPM/RPS conversion: UNIT-007, UNIT-010

2. **P0 Integration tests**
   - Action execution: INT-001 to INT-002
   - Parallel sharing: INT-003 to INT-005
   - Executor registry: INT-007
   - Error handling: INT-013, INT-015

3. **P0 E2E tests**
   - YAML parallel: E2E-001
   - Python parity: E2E-004

4. **P1 tests** (core functionality extensions)
5. **P2 tests** (if time permits)

---

## Quality Checklist

- [x] Every AC has test coverage (20 ACs, 42 test scenarios)
- [x] Test levels are appropriate (unit for logic, integration for system, E2E for workflows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (thread safety, parity = P0)
- [x] Test IDs follow naming convention (TEA-RUST-RL-001-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 24
    integration: 14
    e2e: 4
  by_priority:
    p0: 18
    p1: 16
    p2: 8
  coverage_gaps: []
  critical_paths:
    - "Thread-safe parallel execution with shared limiters"
    - "Python YAML parity for cross-runtime compatibility"
    - "Timeout error handling for production resilience"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RUST-RL-001-test-design-20251231.md
P0 tests identified: 18
Story reference: docs/stories/TEA-RUST-RL-001-ratelimit-wrap-action.md
```
