# Test Design: Epic TEA-WASM-001 (WASM YAML Engine Expansion)

**Date:** 2026-01-17
**Designer:** Quinn (Test Architect)
**Scope:** 7 Stories (TEA-WASM-001.1 through TEA-WASM-001.7)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total Test Scenarios** | 89 | 100% |
| **Unit Tests** | 42 | 47% |
| **Integration Tests** | 31 | 35% |
| **E2E Tests** | 16 | 18% |

### Priority Distribution
| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 28 | Security, data integrity, core functionality |
| **P1** | 35 | Feature completeness, error handling |
| **P2** | 19 | Edge cases, performance |
| **P3** | 7 | Nice-to-have, stretch goals |

---

## Test Scenarios by Story

---

## Story 1.1: YAML Config Parsing

### Acceptance Criteria Summary
1. Parse standard TEA YAML format
2. `WasmYamlConfig` struct matches Python/Rust schema
3. Required field validation with descriptive errors
4. Invalid YAML returns error with line number
5. Parse all YAML files in `examples/`
6. Config struct accessible for execution

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.1-UNIT-001 | Unit | P0 | Parse minimal valid YAML (name + 1 node) | 1 | Core parsing |
| 1.1-UNIT-002 | Unit | P0 | Parse full YAML with all sections | 1 | Complete schema |
| 1.1-UNIT-003 | Unit | P0 | WasmYamlConfig fields match expected types | 2 | Schema parity |
| 1.1-UNIT-004 | Unit | P0 | WasmNodeConfig fields match expected types | 2 | Schema parity |
| 1.1-UNIT-005 | Unit | P0 | WasmEdgeConfig fields match expected types | 2 | Schema parity |
| 1.1-UNIT-006 | Unit | P1 | Missing `name` field returns validation error | 3 | Required field |
| 1.1-UNIT-007 | Unit | P1 | Empty `nodes` array returns validation error | 3 | Required field |
| 1.1-UNIT-008 | Unit | P1 | Duplicate node names returns validation error | 3 | Uniqueness |
| 1.1-UNIT-009 | Unit | P1 | Edge referencing non-existent node returns error | 3 | Ref integrity |
| 1.1-UNIT-010 | Unit | P1 | Invalid YAML syntax returns error with line number | 4 | Debug UX |
| 1.1-UNIT-011 | Unit | P2 | Malformed YAML (unclosed quotes) error context | 4 | Edge case |
| 1.1-INT-001 | Integration | P0 | Parse `examples/simple-agent.yaml` | 5 | Real file |
| 1.1-INT-002 | Integration | P1 | Parse `examples/prolog/*.yaml` files | 5 | Real files |
| 1.1-INT-003 | Integration | P1 | Parse `examples/workflows/*.yaml` files | 5 | Real files |
| 1.1-INT-004 | Integration | P0 | Parsed config feeds into executor | 6 | Integration |

**Total: 15 scenarios** (11 Unit, 4 Integration)

---

## Story 1.2: Tera Template Integration

### Acceptance Criteria Summary
1. Tera engine functional in WASM
2. Nested state access `{{ state.key.nested }}`
3. `tojson` filter works
4. Conditionals evaluate correctly
5. Loops iterate correctly
6. Object passthrough for single expressions
7. Template errors include context
8. Template caching implemented
9. **(SEC-001)** Execution timeout enforced
10. **(SEC-001)** Loop iteration limit enforced
11. **(SEC-001)** Recursion depth limited
12. **(SEC-001)** Filter allowlist enforced
13. **(SEC-001)** State values sanitized
14. **(SEC-001)** Error messages sanitized

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.2-UNIT-001 | Unit | P0 | Simple variable `{{ state.name }}` renders | 2 | Core feature |
| 1.2-UNIT-002 | Unit | P0 | Nested access `{{ state.user.profile.name }}` | 2 | Dot notation |
| 1.2-UNIT-003 | Unit | P0 | `tojson` filter serializes object | 3 | JSON filter |
| 1.2-UNIT-004 | Unit | P0 | `fromjson` filter parses string | 3 | JSON filter |
| 1.2-UNIT-005 | Unit | P0 | `{% if state.active %}` true branch | 4 | Conditionals |
| 1.2-UNIT-006 | Unit | P0 | `{% if state.active %}` false branch | 4 | Conditionals |
| 1.2-UNIT-007 | Unit | P0 | `{% for item in state.items %}` iterates | 5 | Loops |
| 1.2-UNIT-008 | Unit | P0 | Single expression returns JsonValue | 6 | Passthrough |
| 1.2-UNIT-009 | Unit | P1 | Multi-expression returns string | 6 | String mode |
| 1.2-UNIT-010 | Unit | P1 | Undefined variable error includes name | 7 | Error UX |
| 1.2-UNIT-011 | Unit | P2 | Syntax error includes line context | 7 | Error UX |
| 1.2-UNIT-012 | Unit | P2 | Cache hit returns same result | 8 | Performance |
| 1.2-UNIT-013 | Unit | **P0** | Iteration limit (10K) stops large loop | 10 | **SEC-001** |
| 1.2-UNIT-014 | Unit | **P0** | Recursion limit (64) stops deep macro | 11 | **SEC-001** |
| 1.2-UNIT-015 | Unit | **P0** | Blocked filter (`shell`) returns error | 12 | **SEC-001** |
| 1.2-UNIT-016 | Unit | **P0** | `include` directive disabled | 12 | **SEC-001** |
| 1.2-UNIT-017 | Unit | **P0** | `__proto__` key stripped from state | 13 | **SEC-001** |
| 1.2-UNIT-018 | Unit | **P0** | `constructor` key stripped from state | 13 | **SEC-001** |
| 1.2-UNIT-019 | Unit | **P0** | Deep nesting (>32) rejected | 13 | **SEC-001** |
| 1.2-UNIT-020 | Unit | **P0** | Error message doesn't contain paths | 14 | **SEC-001** |
| 1.2-INT-001 | Integration | P0 | Template + action parameter processing | 1 | E2E template |
| 1.2-INT-002 | Integration | P1 | Timeout stops infinite macro recursion | 9 | **SEC-001** |
| 1.2-INT-003 | Integration | P1 | Allowlisted filters all work | 12 | Filter validation |
| 1.2-E2E-001 | E2E | P1 | Complex template in browser WASM | 1 | Browser |

**Total: 24 scenarios** (20 Unit, 3 Integration, 1 E2E)

---

## Story 1.3: Conditional Edge Routing

### Acceptance Criteria Summary
1. Parse `goto: nodeB` simple syntax
2. Parse `goto: nodeB, when: condition` syntax
3. Parse array-based conditional edges
4. Evaluate `when` conditions via Tera
5. Support `__start__` and `__end__` nodes
6. Fall back to sequential execution
7. First unmatched falls through to default
8. Circular edge detection with warning

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.3-UNIT-001 | Unit | P0 | Parse simple `goto: nodeB` | 1 | Basic parsing |
| 1.3-UNIT-002 | Unit | P0 | Parse conditional `goto: {to, when}` | 2 | Conditional |
| 1.3-UNIT-003 | Unit | P0 | Parse array of conditional gotos | 3 | Multi-condition |
| 1.3-UNIT-004 | Unit | P0 | Condition `state.score > 0.8` evaluates true | 4 | Condition eval |
| 1.3-UNIT-005 | Unit | P0 | Condition `state.score > 0.8` evaluates false | 4 | Condition eval |
| 1.3-UNIT-006 | Unit | P0 | `__start__` finds first node | 5 | Special node |
| 1.3-UNIT-007 | Unit | P0 | `__end__` terminates execution | 5 | Special node |
| 1.3-UNIT-008 | Unit | P1 | No matching edge falls to sequential | 6 | Fallback |
| 1.3-UNIT-009 | Unit | P1 | First unmatched condition falls through | 7 | Default case |
| 1.3-UNIT-010 | Unit | P1 | Cycle detected triggers warning | 8 | Safety |
| 1.3-UNIT-011 | Unit | **P0** | Iteration limit (1000) prevents infinite loop | 8 | **TECH-003** |
| 1.3-INT-001 | Integration | P0 | Simple goto navigates correctly | 1 | E2E routing |
| 1.3-INT-002 | Integration | P0 | Conditional goto branches correctly | 2,4 | E2E routing |
| 1.3-INT-003 | Integration | P1 | Multi-condition array selects correct | 3 | E2E routing |
| 1.3-E2E-001 | E2E | P1 | Sentiment router workflow in browser | 4,5 | Browser |

**Total: 15 scenarios** (11 Unit, 3 Integration, 1 E2E)

---

## Story 1.4: Async Node Executor

### Acceptance Criteria Summary
1. All node execution is async
2. State mutations persist across transitions
3. Errors include node name and context
4. No blocking operations in main thread
5. *(Stretch)* Cancellation support
6. *(Stretch)* Progress callbacks

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.4-UNIT-001 | Unit | P0 | `execute_node_async` returns Promise | 1 | Async signature |
| 1.4-UNIT-002 | Unit | P0 | State persists across 2 nodes | 2 | State flow |
| 1.4-UNIT-003 | Unit | P0 | State persists across 5 nodes | 2 | State flow |
| 1.4-UNIT-004 | Unit | P0 | Error includes node name | 3 | Error context |
| 1.4-UNIT-005 | Unit | P0 | Error includes action name | 3 | Error context |
| 1.4-UNIT-006 | Unit | P1 | Nested error preserves context chain | 3 | Error context |
| 1.4-INT-001 | Integration | P0 | Full workflow completes async | 1 | E2E async |
| 1.4-INT-002 | Integration | P0 | LLM action doesn't block | 4 | Non-blocking |
| 1.4-INT-003 | Integration | P1 | Storage action doesn't block | 4 | Non-blocking |
| 1.4-INT-004 | Integration | P3 | AbortController cancels execution | 5 | Stretch |
| 1.4-INT-005 | Integration | P3 | Progress callback fires per node | 6 | Stretch |
| 1.4-E2E-001 | E2E | P0 | Browser UI responsive during execution | 4 | UX critical |
| 1.4-E2E-002 | E2E | P1 | Multi-node workflow in browser | 1,2 | Browser |

**Total: 13 scenarios** (6 Unit, 5 Integration, 2 E2E)

---

## Story 1.5: Simulated Parallel Execution

### Acceptance Criteria Summary
1. Detect parallel edge patterns
2. Execute branches sequentially via async
3. Aggregate results into `parallel_results`
4. Fan-in receives combined state
5. Execution order is deterministic
6. Works with conditional parallels
7. Nested parallel patterns work

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.5-UNIT-001 | Unit | P0 | Detect 2 edges from same source | 1 | Detection |
| 1.5-UNIT-002 | Unit | P0 | Detect fan-in node (2 edges to same target) | 1 | Detection |
| 1.5-UNIT-003 | Unit | P0 | Branch A executes before Branch B | 5 | Order |
| 1.5-UNIT-004 | Unit | P0 | `parallel_results` is array | 3 | Structure |
| 1.5-UNIT-005 | Unit | P0 | `parallel_results[0]` is Branch A state | 3 | Index order |
| 1.5-UNIT-006 | Unit | P0 | `parallel_results[1]` is Branch B state | 3 | Index order |
| 1.5-UNIT-007 | Unit | **P0** | Conflicting state keys: last-write-wins | 4 | **TECH-001** |
| 1.5-UNIT-008 | Unit | **P0** | Merge strategy documented behavior | 4 | **TECH-001** |
| 1.5-UNIT-009 | Unit | P1 | Conditional branch skipped when false | 6 | Conditional |
| 1.5-UNIT-010 | Unit | P1 | Only true branches in `parallel_results` | 6 | Conditional |
| 1.5-UNIT-011 | Unit | P2 | Nested parallel scopes correctly | 7 | Nested |
| 1.5-UNIT-012 | Unit | P2 | Inner `parallel_results` independent | 7 | Nested scope |
| 1.5-INT-001 | Integration | P0 | 2-branch fan-out/fan-in executes | 1,2,3 | E2E parallel |
| 1.5-INT-002 | Integration | P0 | 3-branch fan-out aggregates all | 3 | Multi-branch |
| 1.5-INT-003 | Integration | P1 | Conditional parallel filters branches | 6 | E2E conditional |
| 1.5-E2E-001 | E2E | P1 | Multi-analysis workflow in browser | 1-5 | Browser |

**Total: 16 scenarios** (12 Unit, 3 Integration, 1 E2E)

---

## Story 1.6: Action Parameter Standardization

### Acceptance Criteria Summary
1. `with:` block parameters template-rendered
2. `output:` stores result at state path
3. Nested output paths work
4. Missing required params produce errors
5. Optional params use defaults
6. Results accessible via `{{ result.field }}`

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.6-UNIT-001 | Unit | P0 | Template in `with:` param renders | 1 | Core feature |
| 1.6-UNIT-002 | Unit | P0 | Non-string `with:` passes through | 1 | Type handling |
| 1.6-UNIT-003 | Unit | P0 | `output: result` stores at path | 2 | Simple path |
| 1.6-UNIT-004 | Unit | P0 | `output: result.data.items` nested path | 3 | Nested path |
| 1.6-UNIT-005 | Unit | **P0** | Nested path creates intermediate objects | 3 | **DATA-001** |
| 1.6-UNIT-006 | Unit | **P0** | Path conflict with existing non-object errors | 3 | **DATA-001** |
| 1.6-UNIT-007 | Unit | P0 | Missing required param error includes name | 4 | Error UX |
| 1.6-UNIT-008 | Unit | P0 | Missing required param error includes action | 4 | Error UX |
| 1.6-UNIT-009 | Unit | P1 | Optional param uses default | 5 | Defaults |
| 1.6-UNIT-010 | Unit | P1 | Provided optional overrides default | 5 | Override |
| 1.6-UNIT-011 | Unit | P1 | `{{ result.field }}` in output template | 6 | Result access |
| 1.6-INT-001 | Integration | P0 | `llm.call` validates `prompt` required | 4 | Action validation |
| 1.6-INT-002 | Integration | P1 | `llm.call` uses default `temperature` | 5 | Action defaults |
| 1.6-INT-003 | Integration | P1 | `storage.read` validates `uri` required | 4 | Action validation |

**Total: 14 scenarios** (11 Unit, 3 Integration)

---

## Story 1.7: Integration Testing & Documentation

### Acceptance Criteria Summary
1. 10+ example YAML files execute in WASM
2. Browser test harness validates features
3. wasm-demo showcases new features
4. README documents new capabilities
5. API documentation for public functions
6. Breaking changes documented
7. Migration guide (if needed)

### Test Scenarios

| ID | Level | Priority | Test Description | AC | Justification |
|----|-------|----------|------------------|----|--------------|
| 1.7-INT-001 | Integration | P0 | `greeting-agent.yaml` executes | 1 | Example file |
| 1.7-INT-002 | Integration | P0 | `sentiment-router.yaml` executes | 1 | Example file |
| 1.7-INT-003 | Integration | P0 | `multi-analysis.yaml` executes | 1 | Example file |
| 1.7-INT-004 | Integration | P1 | All 10+ example files pass | 1 | Completeness |
| 1.7-INT-005 | Integration | P0 | Browser test harness loads | 2 | Infrastructure |
| 1.7-INT-006 | Integration | P1 | Browser tests all pass | 2 | Validation |
| 1.7-E2E-001 | E2E | P1 | wasm-demo conditional routing works | 3 | Demo |
| 1.7-E2E-002 | E2E | P1 | wasm-demo template filters work | 3 | Demo |
| 1.7-E2E-003 | E2E | P2 | wasm-demo parallel workflow works | 3 | Demo |
| 1.7-E2E-004 | E2E | P2 | README examples execute correctly | 4 | Doc validation |
| 1.7-E2E-005 | E2E | P2 | API docs match actual signatures | 5 | Doc accuracy |
| 1.7-E2E-006 | E2E | P3 | Breaking changes list is complete | 6 | Doc accuracy |

**Total: 12 scenarios** (6 Integration, 6 E2E)

---

## Risk Coverage Matrix

| Risk ID | Score | Stories | Test IDs |
|---------|-------|---------|----------|
| **SEC-001** | 9 (Critical) | 1.2 | 1.2-UNIT-013 through 1.2-UNIT-020, 1.2-INT-002 |
| **TECH-001** | 9 (Critical) | 1.5 | 1.5-UNIT-007, 1.5-UNIT-008 |
| **TECH-002** | 6 (High) | 1.4 | 1.4-INT-001, 1.4-INT-002, 1.4-E2E-001 |
| **TECH-003** | 6 (High) | 1.3 | 1.3-UNIT-011 |
| **TECH-004** | 6 (High) | 1.1 | 1.1-INT-001 through 1.1-INT-003 |
| **PERF-001** | 6 (High) | 1.5 | 1.5-UNIT-003, 1.5-INT-001 |
| **DATA-001** | 6 (High) | 1.6 | 1.6-UNIT-005, 1.6-UNIT-006 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
Run all P0 unit tests first to catch fundamental issues:
```bash
# Story 1.1
cargo test test_parse_minimal_yaml
cargo test test_parse_full_yaml
cargo test test_wasmyamlconfig_fields

# Story 1.2
cargo test test_simple_variable
cargo test test_nested_access
cargo test test_tojson_filter
cargo test test_iteration_limit  # SEC-001
cargo test test_prototype_pollution  # SEC-001

# Story 1.3
cargo test test_parse_simple_goto
cargo test test_condition_evaluates

# Continue for all P0 unit tests...
```

### Phase 2: P0 Integration Tests
```bash
# Story 1.1
cargo test test_parse_examples_simple
cargo test test_config_feeds_executor

# Story 1.4
wasm-pack test --headless --chrome test_async_execution

# Continue for all P0 integration tests...
```

### Phase 3: P0 E2E Tests
```bash
# Browser responsiveness
wasm-pack test --headless --chrome test_browser_responsive

# Full workflow
wasm-pack test --headless --chrome test_multi_node_browser
```

### Phase 4: P1 Tests (Feature Completeness)
Run all P1 tests in priority order.

### Phase 5: P2+ Tests (As Time Permits)
Run remaining tests for edge cases and stretch goals.

---

## Test Data Requirements

### Fixture Files Needed
```
rust/tea-wasm-llm/tests/fixtures/
├── valid/
│   ├── minimal.yaml           # name + 1 node
│   ├── full.yaml              # all sections
│   ├── conditional.yaml       # goto with when
│   ├── parallel.yaml          # fan-out/fan-in
│   └── nested-parallel.yaml   # nested patterns
├── invalid/
│   ├── missing-name.yaml
│   ├── empty-nodes.yaml
│   ├── duplicate-nodes.yaml
│   ├── bad-edge-ref.yaml
│   └── malformed.yaml
└── security/
    ├── large-loop.yaml        # 100K iterations
    ├── deep-recursion.yaml    # 1000 levels
    ├── proto-pollution.yaml   # __proto__ key
    └── path-traversal.yaml    # include attack
```

### Mock Services
- **LLM Mock**: Returns predefined responses, configurable delay
- **Storage Mock**: In-memory key-value store
- **Timer Mock**: Controllable time for timeout tests

---

## CI Integration

### Recommended Workflow
```yaml
# .github/workflows/wasm-tests.yaml
name: WASM Tests

on: [push, pull_request]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install wasm-pack
        run: cargo install wasm-pack
      - name: Run unit tests
        run: |
          cd rust/tea-wasm-llm
          cargo test --lib

  integration-tests:
    runs-on: ubuntu-latest
    needs: unit-tests
    steps:
      - uses: actions/checkout@v4
      - name: Install wasm-pack
        run: cargo install wasm-pack
      - name: Run WASM tests in browser
        run: |
          cd rust/tea-wasm-llm
          wasm-pack test --headless --chrome

  security-tests:
    runs-on: ubuntu-latest
    needs: unit-tests
    steps:
      - name: Run security test suite
        run: |
          cd rust/tea-wasm-llm
          cargo test security --features security-tests
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 89
  by_level:
    unit: 42
    integration: 31
    e2e: 16
  by_priority:
    p0: 28
    p1: 35
    p2: 19
    p3: 7
  coverage_gaps: []
  critical_risk_tests:
    SEC-001: 9  # Template injection
    TECH-001: 2  # Fan-in merge
    TECH-003: 1  # Infinite loops
    DATA-001: 2  # State corruption
  recommendations:
    - "Create security test fixtures before Story 1.2 implementation"
    - "Implement LLM mock service early for async testing"
    - "Set up browser CI before Story 1.7"
```

---

## Summary

This test design provides **89 test scenarios** covering all 7 stories in the TEA-WASM-001 epic:

| Story | Tests | P0 | Coverage Focus |
|-------|-------|----|-----------------|
| 1.1 YAML Parsing | 15 | 5 | Schema parity, validation |
| 1.2 Tera Templates | 24 | 13 | **Security (SEC-001)**, filters |
| 1.3 Conditional Routing | 15 | 7 | Condition eval, cycle safety |
| 1.4 Async Executor | 13 | 6 | Non-blocking, error context |
| 1.5 Simulated Parallel | 16 | 8 | **Merge strategy (TECH-001)** |
| 1.6 Parameter Standardization | 14 | 6 | **Path handling (DATA-001)** |
| 1.7 Integration & Docs | 12 | 3 | Cross-runtime validation |

**Key Testing Priorities:**
1. **Security tests (SEC-001)** - 9 scenarios specifically for template injection prevention
2. **State integrity (TECH-001, DATA-001)** - 4 scenarios for merge and path handling
3. **Cross-runtime portability** - All example YAML files must pass in WASM

---

**Test Design Location:** `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
**P0 Tests Identified:** 28
**Stories with Critical Risk Tests:** 1.2, 1.5, 1.6
