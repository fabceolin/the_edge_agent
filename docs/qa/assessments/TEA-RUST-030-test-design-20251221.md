# Test Design: Story TEA-RUST-030 - Parallel Lua VM Isolation

Date: 2024-12-21
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 26
- **Unit tests:** 8 (31%)
- **Integration tests:** 13 (50%)
- **E2E tests:** 5 (19%)
- **Priority distribution:** P0: 9, P1: 10, P2: 7

## Story Analysis Summary

This story enables TRUE parallel execution of Lua code in fan-out branches by creating per-branch Lua VM instances. The critical aspect is ensuring **complete isolation** between branches while maintaining thread safety of shared components (ActionRegistry).

### Key Technical Constraints
- `mlua::Lua` is `!Send + !Sync` - cannot share across threads
- Solution: Create new `LuaRuntime::new()` per rayon thread
- ActionRegistry is already `Arc<RwLock<HashMap<...>>>` (thread-safe)
- YamlEngine needs `Arc<RwLock<>>` wrapper for `last_checkpoint`

### Risk Profile
| Risk | Severity | Test Focus |
|------|----------|------------|
| Memory pressure with many branches | Medium | Performance/stress tests |
| Lua VM creation overhead | Low | Benchmark timing tests |
| Cross-branch state contamination | High | Isolation tests (P0) |
| Retry double-execution | Medium | Retry behavior tests |
| Interrupt during parallel | Medium | Documented constraint |

---

## Test Scenarios by Acceptance Criteria

### AC1: Parallel branches execute concurrently using rayon's `par_iter()`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-INT-001 | Integration | P0 | **True parallelism timing verification** - Execute 4 branches with 100ms Lua sleep each, total time should be ~100-150ms NOT 400ms | Critical validation that rayon is actually parallelizing; observable timing behavior |
| RUST-030-INT-002 | Integration | P1 | **CPU-bound parallel execution** - Execute computationally heavy Lua in 4 branches, verify speedup on multi-core | Validates parallelism for real workloads, not just sleep-based |
| RUST-030-UNIT-001 | Unit | P2 | **ParallelExecutor uses rayon correctly** - Verify `par_iter()` is called on branches collection | Unit-level verification of implementation pattern |

---

### AC2: Each parallel branch creates its own `LuaRuntime` instance

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-UNIT-002 | Unit | P0 | **LuaRuntime::new() is independent** - Creating multiple LuaRuntime instances produces isolated Lua VMs | Core requirement for isolation; pure unit test |
| RUST-030-INT-003 | Integration | P1 | **Per-branch Lua VM creation in execute_parallel** - Verify a new LuaRuntime is created inside each parallel closure | Component interaction: executor + LuaRuntime factory |
| RUST-030-UNIT-003 | Unit | P1 | **YamlEngine is cloneable with Arc wrapper** - Verify `last_checkpoint` wrapped in `Arc<RwLock<>>` allows Clone | Prerequisite for parallel executor; pure struct test |

---

### AC3: ActionRegistry remains shared across branches (already thread-safe via `Arc<>`)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-UNIT-004 | Unit | P0 | **ActionRegistry is Arc-clonable** - Verify `Arc<ActionRegistry>` clone shares underlying data | Thread-safety foundation |
| RUST-030-INT-004 | Integration | P0 | **Parallel branches share ActionRegistry** - Multiple branches can invoke the same registered action concurrently | Critical: proves shared action invocation works |
| RUST-030-INT-005 | Integration | P1 | **ActionRegistry lookup under contention** - Concurrent reads from ActionRegistry don't deadlock | Stress test for `parking_lot::RwLock` behavior |

---

### AC4: Fan-in node correctly receives `parallel_results` from all branches

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-INT-006 | Integration | P0 | **Fan-in receives all branch results** - Fan-out to 3 branches, each returns different data; fan-in receives `parallel_results` with all 3 | Critical path: complete fan-out/fan-in cycle |
| RUST-030-INT-007 | Integration | P1 | **Fan-in handles mixed success/failure** - Some branches succeed, some fail; fan-in receives accurate result status for each | Error handling in parallel context |
| RUST-030-INT-008 | Integration | P1 | **Fan-in result ordering** - Verify results are properly associated with branch names regardless of completion order | Parallel execution may complete in any order |

---

### AC5: Lua globals created in Branch A are NOT visible to Branch B

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-INT-009 | Integration | P0 | **Global variable isolation** - Branch A sets `cached = 123`; Branch B reads `cached` -> should be nil | Core isolation requirement |
| RUST-030-INT-010 | Integration | P0 | **Table mutation isolation** - Branch A modifies `_G.config = {}`; Branch B's `_G.config` is unaffected | Tests mutable global state isolation |
| RUST-030-E2E-001 | E2E | P1 | **Full workflow with global isolation** - YAML agent with parallel branches that write conflicting globals; verify clean execution | End-to-end YAML-based isolation test |

---

### AC6: Lua functions defined in Branch A do NOT affect Branch B

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-INT-011 | Integration | P0 | **Function definition isolation** - Branch A defines `function helper() return 1 end`; Branch B's call to `helper()` fails as undefined | Function namespace isolation |
| RUST-030-UNIT-005 | Unit | P1 | **Function table isolation** - Verify each LuaRuntime has independent function registry | Unit test of Lua VM isolation |

---

### AC7: Each branch's Lua timeout is independent

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-INT-012 | Integration | P1 | **Independent timeout enforcement** - Branch A with 50ms timeout times out; Branch B with 200ms completes; parallel execution continues | Timeout doesn't affect sibling branches |
| RUST-030-INT-013 | Integration | P2 | **Timeout doesn't cascade to fan-in** - One branch times out, others complete; fan-in receives partial results | Error boundary testing |

---

### AC8: Existing sequential execution tests continue to pass

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-E2E-002 | E2E | P0 | **Sequential Lua execution unaffected** - Run existing test suite for single-path Lua execution | Regression prevention |
| RUST-030-E2E-003 | E2E | P1 | **Mixed sequential + parallel workflow** - Workflow with sequential nodes before/after parallel section | Integration of new with existing |

---

### AC9: New parallel Lua tests demonstrate isolation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-E2E-004 | E2E | P1 | **YAML-defined parallel Lua agent** - Load YAML with parallel fan-out using Lua code; verify isolation | YAML engine integration |

---

### AC10: Memory usage is documented (N x Lua VM overhead)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-UNIT-006 | Unit | P2 | **Lua VM memory footprint** - Measure memory of single LuaRuntime::new() | Documentation baseline |
| RUST-030-UNIT-007 | Unit | P2 | **10-branch memory overhead** - Create 10 LuaRuntime instances, verify total < 500KB | Per story estimate validation |
| RUST-030-UNIT-008 | Unit | P2 | **LuaRuntime cleanup** - Verify VM is properly dropped after branch completion | Memory leak prevention |

---

### AC11: No breaking changes to public API

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-030-E2E-005 | E2E | P0 | **Public API stability** - Compile and run existing integration tests without modification | API contract preservation |

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Cross-branch state contamination | RUST-030-INT-009, INT-010, INT-011, E2E-001 | Multiple isolation scenarios at different levels |
| Memory pressure | RUST-030-UNIT-006, UNIT-007, UNIT-008 | Memory benchmarks with documented thresholds |
| Lua VM creation overhead | RUST-030-INT-001, INT-002 | Timing-based parallelism validation |
| Retry double-execution | RUST-030-INT-007 | Tested via mixed success/failure scenario |
| Interrupt during parallel | N/A | Documented constraint, not testable (unsupported) |

---

## Test Implementation Patterns

### Rust Test File Structure

```
rust/tests/test_parallel_lua.rs (new file)
├── test_parallel_lua_timing()           # RUST-030-INT-001
├── test_parallel_cpu_bound()            # RUST-030-INT-002
├── test_lua_global_isolation()          # RUST-030-INT-009
├── test_lua_table_isolation()           # RUST-030-INT-010
├── test_lua_function_isolation()        # RUST-030-INT-011
├── test_fan_in_all_results()            # RUST-030-INT-006
├── test_fan_in_mixed_results()          # RUST-030-INT-007
├── test_fan_in_result_ordering()        # RUST-030-INT-008
├── test_independent_timeout()           # RUST-030-INT-012
├── test_shared_action_registry()        # RUST-030-INT-004
└── test_action_registry_contention()    # RUST-030-INT-005

rust/tests/fixtures/parallel_lua_isolation.yaml (new file)
├── Test agent YAML for E2E-001, E2E-004
```

### Sample Test Implementation

```rust
#[test]
fn test_parallel_lua_isolation() {
    // Branch A: sets global `cached = 123`
    let branch_a_code = "cached = 123; return state";

    // Branch B: reads global `cached` -> should be nil
    let branch_b_code = "return { cached_value = cached }";

    // Execute in parallel
    let results = executor.execute_parallel(
        &["branch_a", "branch_b"],
        &state,
        &config,
    ).unwrap();

    // Verify Branch B sees nil for `cached`
    let branch_b_result = results.iter().find(|r| r.branch == "branch_b").unwrap();
    assert_eq!(branch_b_result.state["cached_value"], json!(null));
}

#[test]
fn test_parallel_lua_timing() {
    // 4 branches x 100ms sleep each
    // Use Lua's os.execute or a custom sleep function
    let start = std::time::Instant::now();

    let branches: Vec<String> = (0..4).map(|i| format!("branch_{}", i)).collect();
    let results = executor.execute_parallel(&branches, &state, &config).unwrap();

    let elapsed = start.elapsed();

    // Total time should be ~100-150ms, NOT 400ms
    assert!(elapsed.as_millis() < 200,
        "Expected parallel execution (~100ms), got {}ms", elapsed.as_millis());
    assert_eq!(results.len(), 4);
}
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on fundamental issues)
   - RUST-030-UNIT-002 (LuaRuntime independence)
   - RUST-030-UNIT-004 (ActionRegistry Arc-cloneable)

2. **P0 Integration tests** (core isolation)
   - RUST-030-INT-001 (true parallelism timing)
   - RUST-030-INT-004 (shared ActionRegistry)
   - RUST-030-INT-006 (fan-in receives all)
   - RUST-030-INT-009 (global isolation)
   - RUST-030-INT-010 (table isolation)
   - RUST-030-INT-011 (function isolation)

3. **P0 E2E tests** (regression prevention)
   - RUST-030-E2E-002 (sequential unaffected)
   - RUST-030-E2E-005 (API stability)

4. **P1 tests in order** (secondary paths)
   - INT-002, INT-003, UNIT-003, INT-005, INT-007, INT-008, E2E-001, UNIT-005, INT-012, E2E-003, E2E-004

5. **P2 tests as time permits** (documentation/edge cases)
   - UNIT-001, UNIT-006, UNIT-007, UNIT-008, INT-013

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Critical paths have multiple levels (isolation tested at unit + integration + e2e)
- [x] Priorities align with business risk (isolation = P0, memory = P2)
- [x] Test IDs follow naming convention (RUST-030-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Dependencies for Testing

| Dependency | Needed For | Status |
|------------|------------|--------|
| rayon 1.10 | True parallelism | Already in Cargo.toml |
| mlua 0.9 | Lua VM creation | Already in Cargo.toml |
| criterion | Memory benchmarks (P2) | Add to dev-dependencies |
| parking_lot | ActionRegistry thread-safety | Already in use |

---

## Notes for Developers

1. **Test isolation**: Each test must create fresh `LuaRuntime` instances; don't reuse across tests
2. **Timing tests**: Use generous margins (100-200ms) to account for CI environment variability
3. **Memory tests**: Run with `RUST_TEST_THREADS=1` to get accurate measurements
4. **YAML fixtures**: Create `rust/tests/fixtures/parallel_lua_isolation.yaml` for E2E tests
5. **Interrupt constraint**: Document that interrupts are NOT supported in parallel branches - this is by design

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 26
  by_level:
    unit: 8
    integration: 13
    e2e: 5
  by_priority:
    p0: 9
    p1: 10
    p2: 7
  coverage_gaps: []
  notes:
    - "Interrupt during parallel is documented constraint, not testable"
    - "Memory benchmarks require criterion crate for P2 tests"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RUST-030-test-design-20251221.md
P0 tests identified: 9
P1 tests identified: 10
P2 tests identified: 7
Total coverage: 11 acceptance criteria → 26 test scenarios
```
