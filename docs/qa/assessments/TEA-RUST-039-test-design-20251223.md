# Test Design: Story TEA-RUST-039

**Story:** Migrate Rust Prolog Runtime to Prolog-Side Parsing
**Date:** 2025-12-23
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

This story has **three phases** with distinct testing needs:

| Phase | Focus | Test Approach |
|-------|-------|---------------|
| **Research Spike** | Evaluate alternatives | Exploratory, proof-of-concept |
| **Implementation** | Replace parsing mechanism | Regression + parity tests |
| **Documentation** | Update guides | Manual review |

### Test Distribution

- **Total test scenarios:** 28
- **Unit tests:** 6 (21%)
- **Integration tests:** 14 (50%)
- **E2E tests:** 8 (29%)
- **Priority distribution:** P0: 12, P1: 10, P2: 6

---

## Test Scenarios by Acceptance Criteria

### Phase 1: Research Spike (AC 1-4)

#### AC-1: Investigate `.pl` file loading via swipl-rs `consult/1`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-INT-001 | Integration | P1 | Load `.pl` file with TEA predicates via consult/1 | Core spike requirement |
| 039-INT-002 | Integration | P1 | Verify predicates callable after consult | Validates approach viability |
| 039-INT-003 | Integration | P1 | Test complex rule bodies load correctly | Previous failure point |
| 039-INT-004 | Integration | P1 | Test recursive predicates (tea_load_terms) | Previous failure point |

#### AC-2: Test swipl-rs version updates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-INT-005 | Integration | P2 | Compare term_from_string behavior across versions | Version comparison |
| 039-INT-006 | Integration | P2 | Document API differences if version upgraded | Change tracking |

#### AC-3: Evaluate alternative bindings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-INT-007 | Integration | P2 | Test scryer-prolog string consult capability | Alternative evaluation |
| 039-INT-008 | Integration | P2 | Test trealla string consult capability | Alternative evaluation |
| 039-UNIT-001 | Unit | P2 | Verify CLP(FD) feature parity in alternatives | Feature gap analysis |

#### AC-4: Document findings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-E2E-001 | E2E | P1 | Each approach has reproducible test case | Documentation quality |

---

### Phase 2: Implementation (AC 5-10)

#### AC-5: Implement Prolog-side parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-UNIT-002 | Unit | P0 | TEA predicates file exists and is valid Prolog | Build-time validation |
| 039-INT-009 | Integration | P0 | tea_load_code/1 parses simple facts | Core functionality |
| 039-INT-010 | Integration | P0 | tea_load_code/1 parses rules with complex bodies | Previous edge case |
| 039-INT-011 | Integration | P0 | tea_load_code/1 handles commas in quoted strings | Known edge case |
| 039-INT-012 | Integration | P0 | tea_cleanup_facts/0 removes asserted facts | Cleanup verification |
| 039-UNIT-003 | Unit | P0 | Error handling for malformed Prolog code | Error path coverage |

#### AC-6: Remove heuristic parsing code

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-UNIT-004 | Unit | P1 | looks_like_fact() removed or deprecated | Code cleanup verification |
| 039-UNIT-005 | Unit | P1 | parse_prolog_code() removed or deprecated | Code cleanup verification |

#### AC-7: All 23 parity test fixtures pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-E2E-002 | E2E | P0 | basic-state-access.yaml passes | Parity validation |
| 039-E2E-003 | E2E | P0 | type-coercion.yaml passes | Parity validation |
| 039-E2E-004 | E2E | P0 | All 23 parity fixtures pass (batch) | Full regression |

#### AC-8: All 4 neurosymbolic examples work

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-E2E-005 | E2E | P0 | classifier-rules.yaml produces correct output | Real-world validation |
| 039-E2E-006 | E2E | P1 | clpfd-scheduling.yaml finds valid schedule | CLP(FD) validation |
| 039-E2E-007 | E2E | P1 | knowledge-graph.yaml infers relationships | Inference validation |
| 039-E2E-008 | E2E | P1 | reasoning-chain.yaml multi-step works | Complex flow validation |

#### AC-9: Performance < 10% regression

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-INT-013 | Integration | P1 | Benchmark single node execution time | Performance baseline |
| 039-INT-014 | Integration | P1 | Benchmark 100-iteration loop performance | Throughput testing |
| 039-UNIT-006 | Unit | P1 | Memory usage comparison before/after | Resource usage |

#### AC-10: No regression in 80+ existing tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 039-E2E-009 | E2E | P0 | Full `cargo test --features prolog` passes | Regression prevention |

---

### Phase 3: Documentation (AC 11-13)

Documentation updates are verified through **manual review** during code review process.

---

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| No viable approach found | 039-INT-001 through 039-INT-008 | 8 tests explore alternatives |
| Performance regression | 039-INT-013, 039-INT-014, 039-UNIT-006 | 3 performance tests |
| Breaking existing tests | 039-E2E-009 | Full regression suite |
| Edge case failures | 039-INT-011 | Comma-in-string edge case |

---

## Test Implementation Notes

### Existing Test Files to Leverage

```
rust/tests/test_prolog_parity.rs   - Parity test runner (modify)
rust/tests/test_prolog_runtime.rs  - Unit tests (extend)
examples/prolog/parity/*.yaml      - 11 parity fixtures (use)
examples/prolog/neurosymbolic/*.yaml - 4 examples (use)
```

### New Tests to Create

```rust
// In rust/tests/test_prolog_runtime.rs

#[test]
fn test_tea_predicates_file_loads() {
    // 039-UNIT-002: Verify .pl file is valid
}

#[test]
fn test_prolog_side_parsing_simple_facts() {
    // 039-INT-009: tea_load_code/1 with facts
}

#[test]
fn test_prolog_side_parsing_complex_rules() {
    // 039-INT-010: Rules with cuts, conditionals
}

#[test]
fn test_comma_in_quoted_string_edge_case() {
    // 039-INT-011: Known edge case
    let code = r#"person('John, Jr.', 30)."#;
    // Should parse correctly with Prolog-side parsing
}

#[test]
fn test_cleanup_removes_facts() {
    // 039-INT-012: tea_cleanup_facts/0
}
```

### Performance Benchmark Template

```rust
// In rust/benches/prolog_bench.rs (if using criterion)

fn bench_single_node(c: &mut Criterion) {
    // 039-INT-013: Single node execution
    c.bench_function("prolog_node_execution", |b| {
        b.iter(|| {
            runtime.execute_node_code("state(x, X), Y is X + 1, return(y, Y).", &state)
        })
    });
}
```

---

## Recommended Execution Order

### During Research Spike

1. Run 039-INT-001 to 039-INT-004 for each approach
2. Document results before proceeding

### During Implementation

1. **P0 Unit tests first** (fail fast on fundamental issues)
   - 039-UNIT-002, 039-UNIT-003
2. **P0 Integration tests** (core parsing)
   - 039-INT-009 through 039-INT-012
3. **P0 E2E tests** (parity validation)
   - 039-E2E-002 through 039-E2E-004, 039-E2E-009
4. **P1 tests** (secondary validation)
   - Performance, neurosymbolic examples
5. **P2 tests** (cleanup verification)
   - Code removal checks

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (039-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-039
  date: 2025-12-23
  scenarios_total: 28
  by_level:
    unit: 6
    integration: 14
    e2e: 8
  by_priority:
    p0: 12
    p1: 10
    p2: 6
  coverage_gaps: []
  notes:
    - "Research spike tests (AC 1-4) are exploratory - results inform implementation"
    - "Documentation ACs verified through manual review"
    - "Performance tests require baseline capture before changes"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RUST-039-test-design-20251223.md
P0 tests identified: 12
P1 tests identified: 10
P2 tests identified: 6
```
