# Test Design: Story TEA-RELEASE-005.1

**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)
**Story:** Scryer Prolog Integration Spike
**Type:** Spike (Feasibility Validation)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 22 | 100% |
| **Unit tests** | 10 | 45% |
| **Integration tests** | 8 | 36% |
| **E2E tests** | 1 | 5% |
| **Benchmark tests** | 3 | 14% |
| **Priority distribution** | P0: 6, P1: 10, P2: 6 | - |

### Spike-Specific Testing Notes

This is a **spike** story, meaning tests serve to:
1. **Validate feasibility** - Can Scryer integrate with TEA's `PrologBackend` trait?
2. **Measure performance** - Is Scryer within acceptable bounds (2x SWI-Prolog)?
3. **Document gaps** - What syntax/features are incompatible?

Tests are designed to fail fast if spike should abort.

---

## Test Scenarios by Acceptance Criteria

### AC-1: `scryer-prolog` crate added as optional dependency with `--features scryer`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-UNIT-001 | Unit | P0 | Verify `cargo build --features scryer` compiles successfully | Build validation is gate for all other tests |
| 005.1-UNIT-002 | Unit | P1 | Verify `cargo build` (no scryer feature) still compiles | Regression prevention - default build must not break |
| 005.1-UNIT-003 | Unit | P1 | Verify `scryer-prolog` is only linked when feature enabled | Dependency isolation - avoid bloating default binary |

**Risk Mitigations:** Compilation failure blocks entire spike; early detection critical.

---

### AC-2: `ScryerBackend` implements `PrologBackend` trait

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-UNIT-004 | Unit | P0 | `ScryerBackend::new()` initializes successfully | Constructor validation |
| 005.1-UNIT-005 | Unit | P0 | `ScryerBackend` implements all `PrologBackend` trait methods | Trait compliance is spike success criterion |
| 005.1-INT-001 | Integration | P1 | `ScryerBackend` can be swapped for `SwiBackend` in factory | Backend polymorphism validation |

**Risk Mitigations:** Trait incompatibility = spike failure.

---

### AC-3: Basic Prolog queries execute correctly (`?- member(X, [1,2,3]).`)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-UNIT-006 | Unit | P0 | `member/2` query returns correct unifications `[1, 2, 3]` | Core query execution |
| 005.1-UNIT-007 | Unit | P1 | `append/3` query works correctly | Standard list predicate |
| 005.1-UNIT-008 | Unit | P1 | Arithmetic queries work (`X is 2 + 3`) | ISO Prolog arithmetic |
| 005.1-INT-002 | Integration | P1 | Query with multiple solutions returns all via backtracking | Backtracking behavior validation |
| 005.1-INT-003 | Integration | P2 | Failed query returns empty result set (not error) | Error handling for no-solutions case |

**Risk Mitigations:** Query failure = spike failure; tests specific ISO Prolog predicates.

---

### AC-4: State manipulation predicates work (`set_state/2`, `get_state/2`)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-INT-004 | Integration | P0 | `set_state/2` followed by `get_state/2` returns set value | Core TEA state integration |
| 005.1-INT-005 | Integration | P1 | State persists across multiple queries in same session | State lifecycle validation |
| 005.1-INT-006 | Integration | P2 | State isolation between different `ScryerBackend` instances | Multi-agent scenario |
| 005.1-UNIT-009 | Unit | P2 | `nb_setval/2` absence handled gracefully (documented limitation) | Known gap validation |

**Risk Mitigations:** State predicates are TEA-specific; must work for spike success.

---

### AC-5: `examples/prolog/simple-prolog-agent.yaml` ported to Scryer syntax and runs

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-E2E-001 | E2E | P0 | `simple-prolog-agent-scryer.yaml` executes to completion with `--features scryer` | Full agent execution validates real-world usage |
| 005.1-INT-007 | Integration | P1 | Ported YAML agent produces equivalent output to SWI-Prolog version | Functional equivalence |

**Risk Mitigations:** E2E validates end-to-end feasibility; failure = spike failure.

---

### AC-6: Syntax differences documented in `docs/shared/scryer-vs-swi.md`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-UNIT-010 | Unit | P2 | `scryer-vs-swi.md` file exists and is non-empty | Documentation deliverable |

**Note:** Documentation is validated by existence check only; content reviewed manually.

---

### AC-7: Benchmark results: startup time and query performance vs SWI-Prolog

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-BENCH-001 | Benchmark | P1 | Measure Scryer startup time (Machine::new()) | Baseline metric |
| 005.1-BENCH-002 | Benchmark | P1 | Measure query execution time for 1000 `member/2` queries | Query throughput |
| 005.1-BENCH-003 | Benchmark | P2 | Measure memory usage for loaded Prolog program | Resource footprint |

**Risk Mitigations:** Performance data informs go/no-go decision.

---

### AC-8: Performance within 2x of SWI-Prolog for typical queries (or documented exceptions)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 005.1-INT-008 | Integration | P1 | Assert Scryer query time ≤ 2x SWI-Prolog baseline | Spike success criterion |

**Note:** If >2x, test should PASS but output warning for manual review per spike failure criteria.

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Scryer cannot implement `PrologBackend` | Medium | Critical | 005.1-UNIT-005 |
| Performance >5x slower than SWI | Low | Critical | 005.1-INT-008, BENCH-* |
| Critical predicates missing | Medium | High | 005.1-UNIT-006-009, INT-004-006 |
| Compilation issues | Low | Critical | 005.1-UNIT-001-003 |
| State predicates incompatible | Medium | High | 005.1-INT-004-006 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on fundamental issues)
   - 005.1-UNIT-001 (compilation gate)
   - 005.1-UNIT-004, 005.1-UNIT-005 (trait implementation)
   - 005.1-UNIT-006 (basic query)

2. **P0 Integration tests**
   - 005.1-INT-004 (state predicates)

3. **P0 E2E test**
   - 005.1-E2E-001 (agent execution)

4. **P1 tests in order** (core validation)

5. **Benchmark tests** (performance data collection)

6. **P2+ as time permits**

---

## Test Implementation Recommendations

### Location Structure

```
rust/
├── src/prolog/
│   └── scryer_backend.rs     # Contains inline unit tests (#[cfg(test)])
├── tests/
│   └── test_scryer_prolog.rs # Integration tests
└── benches/
    └── scryer_benchmarks.rs  # Criterion benchmarks
```

### Feature Gating

All Scryer tests must be gated:
```rust
#[cfg(feature = "scryer")]
mod scryer_tests {
    // ...
}
```

### Benchmark Framework

Use `criterion` crate for reproducible benchmarks:
```toml
[dev-dependencies]
criterion = "0.5"

[[bench]]
name = "scryer_benchmarks"
harness = false
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (unit for logic, integration for component interaction)
- [x] No duplicate coverage across levels
- [x] Priorities align with spike success/failure criteria
- [x] Test IDs follow naming convention (`{EPIC}.{STORY}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] Spike-specific considerations documented

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 22
  by_level:
    unit: 10
    integration: 8
    e2e: 1
    benchmark: 3
  by_priority:
    p0: 6
    p1: 10
    p2: 6
  coverage_gaps: []
  spike_specific:
    fail_fast_tests:
      - 005.1-UNIT-001
      - 005.1-UNIT-005
      - 005.1-E2E-001
    performance_threshold: "2x SWI-Prolog"
    abort_threshold: "5x SWI-Prolog"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RELEASE-005.1-test-design-20260112.md
P0 tests identified: 6
Spike fail-fast tests: 3
```
