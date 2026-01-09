# Test Design: Story TEA-RELEASE-004.3a

**Story:** WASM LLM Core Package (Phi-4-mini)
**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 32 | 100% |
| **Unit tests** | 14 | 44% |
| **Integration tests** | 12 | 37% |
| **E2E tests** | 6 | 19% |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 11 | Critical - Must pass before release |
| P1 | 13 | High - Core functionality validation |
| P2 | 6 | Medium - Secondary scenarios |
| P3 | 2 | Low - Edge cases, nice-to-have |

---

## Test Scenarios by Acceptance Criteria

### AC-1: `llm.call` action works in browser using wllama callback bridge

**Risk Level:** High - Core functionality, JavaScript/Rust interop boundary

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-001 | Unit | P0 | LLM handler registration stores callback correctly | Pure state management in Rust |
| 4.3a-UNIT-002 | Unit | P0 | LLM handler validates callback is a function | Input validation logic |
| 4.3a-UNIT-003 | Unit | P1 | JSON params serialization/deserialization | Pure transformation logic |
| 4.3a-INT-001 | Integration | P0 | `llm_call_async` invokes registered JS handler | Critical WASM-JS interop |
| 4.3a-INT-002 | Integration | P0 | Handler receives correct params JSON | Cross-boundary data flow |
| 4.3a-INT-003 | Integration | P1 | Handler async Promise resolution passes result back | Promise interop verification |
| 4.3a-E2E-001 | E2E | P0 | Complete `llm.call` action with wllama in browser | Full workflow validation |

---

### AC-2: Multi-threading supported when COOP/COEP headers present

**Risk Level:** Medium - Browser environment dependent, graceful fallback exists

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-004 | Unit | P1 | SharedArrayBuffer detection logic | Pure browser feature check |
| 4.3a-UNIT-005 | Unit | P2 | Thread count calculation based on navigator.hardwareConcurrency | Pure logic |
| 4.3a-INT-004 | Integration | P1 | Multi-threaded mode activates with COOP/COEP headers | Environment integration |
| 4.3a-INT-005 | Integration | P1 | Falls back to single-thread without COOP/COEP | Fallback path validation |
| 4.3a-E2E-002 | E2E | P2 | LLM execution in multi-threaded mode | Full threading validation |

---

### AC-3: `initTeaLlm()` initializes WASM module and registers wllama handler

**Risk Level:** High - Entry point for all functionality

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-006 | Unit | P0 | TeaLlmConfig interface validates required fields | Type validation |
| 4.3a-UNIT-007 | Unit | P1 | Initialization state tracking prevents double init | State management logic |
| 4.3a-INT-006 | Integration | P0 | `initTeaLlm` loads WASM module successfully | WASM loading integration |
| 4.3a-INT-007 | Integration | P0 | `initTeaLlm` registers provided LLM handler | Handler registration flow |
| 4.3a-INT-008 | Integration | P1 | Repeated `initTeaLlm` calls are idempotent | Robustness verification |

---

### AC-4: `executeLlmYaml()` executes YAML workflows with LLM actions

**Risk Level:** High - Core execution engine, complex data flow

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-008 | Unit | P0 | YAML parsing extracts nodes and edges correctly | Pure parsing logic |
| 4.3a-UNIT-009 | Unit | P1 | State JSON serialization round-trips correctly | Data integrity |
| 4.3a-INT-009 | Integration | P0 | `executeLlmYaml` processes simple workflow end-to-end | Core execution flow |
| 4.3a-INT-010 | Integration | P1 | Workflow state passed correctly between nodes | Multi-node flow |
| 4.3a-E2E-003 | E2E | P0 | Complete YAML workflow with LLM action in browser | Full system validation |

---

### AC-5: wasm-pack builds `tea-wasm-llm` package successfully

**Risk Level:** Medium - Build toolchain, one-time verification

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-INT-011 | Integration | P0 | `wasm-pack build --target web` succeeds | Build toolchain integration |
| 4.3a-UNIT-010 | Unit | P2 | Cargo.toml has correct wasm-bindgen configuration | Configuration validation |

---

### AC-6: Package includes TypeScript definitions (`.d.ts` files)

**Risk Level:** Low - Developer experience, non-functional

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-INT-012 | Integration | P1 | TypeScript definitions generated in pkg/ | Build output verification |
| 4.3a-UNIT-011 | Unit | P2 | Type definitions export all public interfaces | Type completeness |

---

### AC-7: ES module build provided for browser usage

**Risk Level:** Medium - Distribution format critical for browser usage

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-012 | Unit | P1 | ESM import syntax works in modern browsers | Module format validation |
| 4.3a-E2E-004 | E2E | P1 | Package imports successfully in browser `<script type="module">` | Real browser validation |

---

### AC-8: Unit test with tiny test model (stories260K.gguf ~500KB) passes

**Risk Level:** High - Core validation path for CI/CD

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-E2E-005 | E2E | P0 | Tiny model (stories260K.gguf) loads and generates text | End-to-end model validation |
| 4.3a-UNIT-013 | Unit | P1 | Test harness HTML structure is valid | Test infrastructure |

---

### AC-9: Existing tea-wasm spike remains functional (separate crate)

**Risk Level:** Low - Regression prevention, isolated crate

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-INT-013 | Integration | P1 | wasm-spike crate builds independently | Crate isolation verification |
| 4.3a-UNIT-014 | Unit | P3 | No shared mutable state between crates | Isolation validation |

---

### AC-10: No WASM compilation warnings

**Risk Level:** Low - Code quality, non-functional

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-INT-014 | Integration | P2 | `cargo build --release` produces zero warnings | Build quality gate |

---

### AC-11: Unit tests for error paths (no handler registered, invalid JSON params)

**Risk Level:** High - Error handling critical for debugging

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 4.3a-UNIT-015 | Unit | P0 | `llm_call_async` errors when no handler registered | Error path validation |
| 4.3a-UNIT-016 | Unit | P0 | Invalid JSON params return descriptive error | Input validation |
| 4.3a-UNIT-017 | Unit | P1 | Handler rejection propagates error correctly | Error propagation |
| 4.3a-E2E-006 | E2E | P2 | Browser console shows meaningful error messages | User-facing errors |

---

## Error Path Coverage Matrix

| Error Condition | Unit Test | Integration Test | E2E Test |
|-----------------|-----------|------------------|----------|
| No LLM handler registered | 4.3a-UNIT-015 | - | - |
| Invalid JSON params | 4.3a-UNIT-016 | - | - |
| Handler Promise rejection | 4.3a-UNIT-017 | - | - |
| WASM module load failure | - | 4.3a-INT-006 (negative case) | - |
| Invalid YAML syntax | 4.3a-UNIT-008 (negative case) | - | - |
| Browser console errors | - | - | 4.3a-E2E-006 |

---

## Risk Coverage

| Risk | Probability | Impact | Test Scenarios Mitigating |
|------|-------------|--------|---------------------------|
| WASM-JS interop failure | Medium | High | 4.3a-INT-001, 4.3a-INT-002, 4.3a-INT-003 |
| Async Promise handling bugs | Medium | High | 4.3a-INT-003, 4.3a-UNIT-017 |
| Multi-threading crashes | Low | Medium | 4.3a-INT-004, 4.3a-INT-005 |
| JSON serialization errors | Low | High | 4.3a-UNIT-003, 4.3a-UNIT-009 |
| Build toolchain breakage | Low | High | 4.3a-INT-011, 4.3a-INT-014 |
| Regression in spike crate | Low | Low | 4.3a-INT-013 |

---

## Recommended Execution Order

### Phase 1: Unit Tests (Fail Fast)
Execute all P0 unit tests first to catch fundamental logic errors:
1. 4.3a-UNIT-001, 4.3a-UNIT-002 (handler registration)
2. 4.3a-UNIT-006, 4.3a-UNIT-008 (config/parsing)
3. 4.3a-UNIT-015, 4.3a-UNIT-016 (error paths)

### Phase 2: Integration Tests (Component Boundaries)
Execute P0 integration tests to validate WASM-JS boundaries:
1. 4.3a-INT-011 (build verification)
2. 4.3a-INT-001, 4.3a-INT-002 (handler invocation)
3. 4.3a-INT-006, 4.3a-INT-007 (initialization)
4. 4.3a-INT-009 (YAML execution)

### Phase 3: E2E Tests (Full Validation)
Execute P0 E2E tests to validate complete browser workflows:
1. 4.3a-E2E-001 (llm.call in browser)
2. 4.3a-E2E-003 (YAML workflow)
3. 4.3a-E2E-005 (tiny model test)

### Phase 4: P1 Tests
Execute remaining P1 tests in order: Unit -> Integration -> E2E

### Phase 5: P2+ Tests
Execute P2 and P3 tests as time permits.

---

## Test Environment Requirements

### Unit Tests
- **Runtime:** Rust `cargo test` with `wasm32-unknown-unknown` target
- **Dependencies:** None (mocked JS interop)
- **Execution time:** < 1 minute

### Integration Tests
- **Runtime:** wasm-pack test with headless browser (Chrome/Firefox)
- **Dependencies:** Node.js, wasm-pack, headless browser
- **Execution time:** < 5 minutes

### E2E Tests
- **Runtime:** Real browser with test HTML harness
- **Dependencies:** Web server with COOP/COEP headers, tiny model file
- **Model:** stories260K.gguf (~500KB) from HuggingFace
- **Execution time:** < 10 minutes

---

## Test Data Requirements

| Test Category | Data Required | Source |
|---------------|---------------|--------|
| Unit tests | Mock JSON payloads | Inline test fixtures |
| Integration tests | Simple YAML workflows | Test fixtures in `tests/` |
| E2E tests | Tiny GGUF model | `ggml-org/models/tinyllamas/stories260K.gguf` |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `4.3a-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Error paths have dedicated tests (AC-11)
- [x] Regression prevention for spike crate (AC-9)

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RELEASE-004.3a
  date: 2026-01-08
  scenarios_total: 32
  by_level:
    unit: 14
    integration: 12
    e2e: 6
  by_priority:
    p0: 11
    p1: 13
    p2: 6
    p3: 2
  coverage_gaps: []
  key_risks_addressed:
    - WASM-JS async interop
    - Error handling paths
    - Multi-threading fallback
    - Build toolchain stability
  test_environment:
    unit: cargo test --target wasm32-unknown-unknown
    integration: wasm-pack test --headless --chrome
    e2e: browser test harness with COOP/COEP headers
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.3a-test-design-20260108.md
P0 tests identified: 11
Total scenarios: 32
Coverage: 11 ACs fully covered
```
