# Test Design: Story TEA-WASM-002

**Date:** 2026-01-09
**Designer:** Quinn (Test Architect)
**Story:** Lua and Prolog JS Bridge Callbacks

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 10 (42%) |
| Integration tests | 8 (33%) |
| E2E tests | 6 (25%) |
| Priority distribution | P0: 8, P1: 10, P2: 6 |

### Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Callback not invoked | High | Medium | Unit tests verify invocation path |
| State corruption | High | Low | Integration tests with real Lua/Prolog |
| Memory leaks | Medium | Medium | E2E tests with repeated executions |
| Type mismatches | Medium | High | TypeScript declaration validation |
| Missing error handling | High | Medium | Error path unit tests |

## Test Scenarios by Acceptance Criteria

---

### AC-1: Export `set_lua_callback(handler: Function)` from tea-wasm-llm

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-001 | Unit | P0 | `set_lua_callback` is exported and callable | Core export validation |
| WASM-002-UNIT-002 | Unit | P1 | `set_lua_callback` accepts Function type | Type contract validation |
| WASM-002-UNIT-003 | Unit | P1 | `clear_lua_callback` removes registered callback | Cleanup path |

---

### AC-2: `lua.eval` action invokes the registered callback

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-004 | Unit | P0 | YAML engine routes `lua.eval` to callback | Core routing logic |
| WASM-002-INT-001 | Integration | P0 | `lua.eval` in YAML workflow invokes registered handler | Multi-component flow |

---

### AC-3: Callback receives `(code: string, stateJson: string)` parameters

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-002 | Integration | P0 | Callback receives correct code parameter | Parameter passing |
| WASM-002-INT-003 | Integration | P0 | Callback receives serialized state JSON | State serialization |
| WASM-002-UNIT-005 | Unit | P1 | Empty code string handled gracefully | Edge case |

---

### AC-4: Result from callback is merged into workflow state

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-004 | Integration | P0 | Callback result merged into state | State management |
| WASM-002-INT-005 | Integration | P1 | Nested objects in result merge correctly | Complex data handling |
| WASM-002-UNIT-006 | Unit | P2 | Malformed JSON result produces clear error | Error handling |

---

### AC-5: Clear error if `lua.eval` called without registered callback

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-007 | Unit | P0 | `lua.eval` without callback throws descriptive error | Fail-fast principle |
| WASM-002-UNIT-008 | Unit | P1 | Error message includes action name and suggestion | DX quality |

---

### AC-6: Export `set_prolog_handler(handler: Function)` from tea-wasm-llm

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-009 | Unit | P0 | `set_prolog_handler` is exported and callable | Core export validation |
| WASM-002-UNIT-010 | Unit | P1 | `clear_prolog_handler` removes registered handler | Cleanup path |

---

### AC-7: `prolog.query` action invokes the registered handler

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-006 | Integration | P0 | `prolog.query` in YAML workflow invokes handler | Multi-component flow |

---

### AC-8: Handler receives `(queryJson: string)` with code and optional facts

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-007 | Integration | P1 | Handler receives queryJson with code field | Parameter structure |
| WASM-002-INT-008 | Integration | P1 | Handler receives optional facts in queryJson | Optional parameter |

---

### AC-9: Query results are returned as JSON array of bindings

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-E2E-001 | E2E | P1 | Prolog query returns multiple bindings | Real Prolog execution |

---

### AC-10: Clear error if `prolog.query` called without registered handler

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-011 | Unit | P1 | `prolog.query` without handler throws descriptive error | Fail-fast |

**Note:** Unit test count adjusted - adding this brings total to 11 unit tests (updating overview).

---

### AC-11: Both callbacks are optional (package works without them)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-E2E-002 | E2E | P0 | Package initializes and runs LLM workflows without Lua/Prolog | Regression prevention |

---

### AC-12: Type declarations (.d.ts) include callback signatures

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-012 | Unit | P2 | TypeScript compilation succeeds with callback types | Build verification |
| WASM-002-UNIT-013 | Unit | P2 | Type errors for incorrect callback signatures | Type safety |

---

### AC-13: README documents wasmoon and trealla integration examples

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-UNIT-014 | Unit | P2 | README contains wasmoon example code block | Documentation check |
| WASM-002-UNIT-015 | Unit | P2 | README contains trealla example code block | Documentation check |

---

### AC-14: Unit test for Lua callback invocation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-009 | Integration | P1 | Mock callback receives invocation with correct args | Test infrastructure |

---

### AC-15: Unit test for Prolog handler invocation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-INT-010 | Integration | P1 | Mock handler receives invocation with correct args | Test infrastructure |

---

### AC-16: Integration test with real wasmoon in Playwright

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-E2E-003 | E2E | P1 | Browser loads wasmoon, registers callback, executes Lua | Real browser execution |
| WASM-002-E2E-004 | E2E | P2 | Lua script accesses and modifies workflow state | State integration |

---

### AC-17: Integration test with real trealla in Playwright

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| WASM-002-E2E-005 | E2E | P1 | Browser loads trealla, registers handler, executes Prolog | Real browser execution |
| WASM-002-E2E-006 | E2E | P2 | Prolog facts are consulted and query returns bindings | Query execution |

---

## Updated Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 28 |
| Unit tests | 15 (54%) |
| Integration tests | 10 (36%) |
| E2E tests | 6 (21%) |
| Priority distribution | P0: 8, P1: 12, P2: 8 |

## Risk Coverage Matrix

| Risk | Mitigating Tests |
|------|------------------|
| Callback not invoked | WASM-002-UNIT-004, WASM-002-INT-001, WASM-002-INT-006 |
| State corruption | WASM-002-INT-004, WASM-002-INT-005, WASM-002-E2E-004 |
| Memory leaks | WASM-002-E2E-003, WASM-002-E2E-005 (repeated execution) |
| Type mismatches | WASM-002-UNIT-012, WASM-002-UNIT-013 |
| Missing error handling | WASM-002-UNIT-006, WASM-002-UNIT-007, WASM-002-UNIT-011 |
| Regression on LLM | WASM-002-E2E-002 |

## Recommended Execution Order

### Phase 1: Fast Feedback (P0 Unit + Integration)
1. WASM-002-UNIT-001 - Export validation
2. WASM-002-UNIT-004 - Routing logic
3. WASM-002-UNIT-007 - Error handling (no callback)
4. WASM-002-UNIT-009 - Prolog export validation
5. WASM-002-INT-001 - Lua callback invocation
6. WASM-002-INT-002 - Parameter passing (code)
7. WASM-002-INT-003 - Parameter passing (state)
8. WASM-002-INT-004 - State merge

### Phase 2: Core Validation (P0 E2E + P1)
9. WASM-002-E2E-002 - No regression on LLM-only workflows
10. WASM-002-INT-006 - Prolog handler invocation
11. WASM-002-E2E-003 - Real wasmoon in browser
12. WASM-002-E2E-005 - Real trealla in browser

### Phase 3: Completeness (P2)
13. Remaining P1 tests
14. P2 tests (documentation, type safety)

## Test Implementation Notes

### Unit Tests (Vitest/Jest)

```typescript
// Example unit test structure
describe('Lua Callback', () => {
  it('WASM-002-UNIT-001: exports set_lua_callback', () => {
    expect(typeof set_lua_callback).toBe('function');
  });

  it('WASM-002-UNIT-007: throws without registered callback', async () => {
    clear_lua_callback();
    await expect(executeLuaAction({ code: 'return 1' }))
      .rejects.toThrow(/lua callback not registered/i);
  });
});
```

### E2E Tests (Playwright)

```typescript
// Example E2E test structure
test('WASM-002-E2E-003: Lua via wasmoon in browser', async ({ page }) => {
  await page.goto('/test-harness.html');

  // Load wasmoon
  await page.evaluate(async () => {
    const { LuaFactory } = await import('wasmoon');
    const lua = await (new LuaFactory()).createEngine();
    window.lua = lua;
  });

  // Register callback
  await page.evaluate(() => {
    set_lua_callback(async (code, stateJson) => {
      window.lua.global.set('state', JSON.parse(stateJson));
      return JSON.stringify({ result: await window.lua.doString(code) });
    });
  });

  // Execute workflow
  const result = await page.evaluate(() =>
    executeLlmYaml(luaWorkflow, { value: 42 })
  );

  expect(result.lua_result).toBeDefined();
});
```

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (shifted left where possible)
- [x] No duplicate coverage across levels
- [x] Critical paths (callback invocation, state merge) have multiple levels
- [x] Risk mitigations are addressed
- [x] Test IDs follow naming convention: `WASM-002-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Error paths are covered

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-WASM-002
  date: 2026-01-09
  designer: Quinn
  scenarios_total: 28
  by_level:
    unit: 15
    integration: 10
    e2e: 6
  by_priority:
    p0: 8
    p1: 12
    p2: 8
  coverage_gaps: []
  risks_mitigated:
    - callback_not_invoked
    - state_corruption
    - type_mismatches
    - missing_error_handling
    - llm_regression
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-WASM-002-test-design-20260109.md
P0 tests identified: 8
P1 tests identified: 12
Story location: docs/stories/TEA-WASM-002.wasm-lua-prolog-bridges.md
```
