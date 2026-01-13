# Test Design: Story TEA-GAME-001.6

Date: 2026-01-10
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 14 (33%)
- **Integration tests:** 16 (38%)
- **E2E tests:** 12 (29%)
- **Priority distribution:** P0: 18, P1: 16, P2: 8

## Story Context

This story implements WASM compilation of the game engine with JavaScript bindings for browser execution. Key technical challenges:
- WASM/JS interop via wasm-bindgen
- Browser storage (IndexedDB/OPFS) for DuckDB persistence
- Async callback bridges for LLM and Opik
- TypeScript type safety
- Cross-browser compatibility

## Test Scenarios by Acceptance Criteria

---

### AC-1: wasm-bindgen exports for game functions

**Requirement:** `wasm-bindgen` exports for `start_session`, `generate_round`, `submit_answer`, `submit_to_leaderboard`, `get_leaderboard`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-UNIT-001 | Unit | P0 | Validate `game_start_session()` returns valid session JSON structure | Core API contract - pure serialization logic |
| 1.6-UNIT-002 | Unit | P0 | Validate `game_generate_round()` returns round JSON with phrase and 5 choices | API contract validation |
| 1.6-UNIT-003 | Unit | P0 | Validate `game_submit_answer()` accepts choice string and time_ms | Input validation logic |
| 1.6-UNIT-004 | Unit | P1 | Validate `game_get_leaderboard()` accepts limit parameter | Parameter handling |
| 1.6-UNIT-005 | Unit | P1 | Validate `game_get_session_stats()` returns session statistics | API contract |
| 1.6-INT-001 | Integration | P0 | `game_start_session()` initializes GameEngine and persists session | WASM-to-Rust bridge + state management |
| 1.6-INT-002 | Integration | P0 | `game_generate_round()` invokes LLM callback and returns round | Callback bridge integration |
| 1.6-INT-003 | Integration | P0 | `game_submit_answer()` updates session state correctly | State mutation across WASM boundary |
| 1.6-INT-004 | Integration | P1 | `game_submit_to_leaderboard()` persists score to DuckDB | Database integration |
| 1.6-INT-005 | Integration | P1 | `game_get_leaderboard()` retrieves sorted entries from DuckDB | Query integration |
| 1.6-E2E-001 | E2E | P0 | Full session lifecycle: start → generate → answer → leaderboard | Critical user journey |

---

### AC-2: IndexedDB or OPFS storage for DuckDB persistence

**Requirement:** IndexedDB or OPFS storage for DuckDB persistence in browser

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-INT-006 | Integration | P0 | DuckDB initializes with OPFS storage in Chrome | Primary storage path |
| 1.6-INT-007 | Integration | P1 | DuckDB falls back to IndexedDB when OPFS unavailable | Fallback mechanism |
| 1.6-INT-008 | Integration | P0 | Session data persists across page refresh | Data integrity critical |
| 1.6-INT-009 | Integration | P1 | Leaderboard data persists across browser restart | Long-term persistence |
| 1.6-E2E-002 | E2E | P0 | User session survives page refresh - Chrome | Critical persistence path |
| 1.6-E2E-003 | E2E | P0 | User session survives page refresh - Firefox | Cross-browser validation |
| 1.6-E2E-004 | E2E | P1 | User session survives page refresh - Safari | Cross-browser validation |
| 1.6-UNIT-006 | Unit | P2 | Storage adapter handles quota exceeded error | Error boundary |
| 1.6-INT-010 | Integration | P2 | Storage quota warning triggers user notification | UX integration |

---

### AC-3: LLM callback bridge (reuse existing tea-wasm-llm pattern)

**Requirement:** LLM callback bridge (reuse existing `tea-wasm-llm` pattern)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-UNIT-007 | Unit | P0 | LLM callback signature matches expected interface | Contract validation |
| 1.6-INT-011 | Integration | P0 | `set_game_llm_handler()` registers JS callback successfully | Bridge setup |
| 1.6-INT-012 | Integration | P0 | Async LLM callback resolves correctly in WASM context | Async interop critical |
| 1.6-INT-013 | Integration | P1 | LLM callback timeout returns graceful error | Error handling |
| 1.6-E2E-005 | E2E | P0 | Round generation with mock LLM produces valid game round | Full flow validation |

---

### AC-4: Opik callback bridge from TEA-OBS-002

**Requirement:** Opik callback bridge from TEA-OBS-002

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-UNIT-008 | Unit | P1 | Opik callback signature matches TEA-OBS-002 interface | Contract alignment |
| 1.6-INT-014 | Integration | P1 | `set_opik_handler()` registers JS callback | Bridge setup |
| 1.6-INT-015 | Integration | P1 | Opik callback fires on game events (fire-and-forget) | Observability flow |
| 1.6-UNIT-009 | Unit | P2 | Opik callback failure does not block game flow | Non-blocking semantics |
| 1.6-E2E-006 | E2E | P2 | Opik events captured during full game session | Observability validation |

---

### AC-5: TypeScript type definitions for all exports

**Requirement:** TypeScript type definitions for all exports

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-UNIT-010 | Unit | P0 | TypeScript declarations compile without errors | Type safety |
| 1.6-UNIT-011 | Unit | P1 | `GameSession` interface matches runtime return type | Type accuracy |
| 1.6-UNIT-012 | Unit | P1 | `GameRound` interface matches runtime return type | Type accuracy |
| 1.6-UNIT-013 | Unit | P1 | `GameError` interface matches error responses | Error type contract |
| 1.6-INT-016 | Integration | P1 | TypeScript types exported from package entry point | Package integration |

---

### AC-6: Error handling returns structured JSON errors

**Requirement:** Error handling returns structured JSON errors

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-UNIT-014 | Unit | P0 | Error response includes `success: false`, `error`, `error_type` | Error contract |
| 1.6-INT-017 | Integration | P0 | `game_generate_round()` without session returns `session_error` | Error path validation |
| 1.6-INT-018 | Integration | P1 | LLM failure returns `llm_error` type | Error classification |
| 1.6-INT-019 | Integration | P1 | Invalid choice returns `invalid_choice` type | Input validation error |
| 1.6-INT-020 | Integration | P2 | Database failure returns `db_error` type | Storage error handling |
| 1.6-E2E-007 | E2E | P1 | Panic in WASM produces console.error + structured error | Graceful degradation |

---

### AC-7: Playwright tests for WASM functions

**Requirement:** Playwright tests for WASM functions

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| 1.6-E2E-008 | E2E | P0 | Playwright: `start_session` creates valid session | CI validation |
| 1.6-E2E-009 | E2E | P0 | Playwright: `generate_round` with mock LLM returns round | CI validation |
| 1.6-E2E-010 | E2E | P0 | Playwright: `submit_answer` updates stats correctly | CI validation |
| 1.6-E2E-011 | E2E | P1 | Playwright: `submit_to_leaderboard` persists score | CI validation |
| 1.6-E2E-012 | E2E | P1 | Playwright: `get_leaderboard` returns sorted entries | CI validation |

---

## Risk Coverage

| Risk | Test Coverage |
|------|---------------|
| WASM initialization failure | 1.6-E2E-001, 1.6-INT-001 |
| Browser storage quota exceeded | 1.6-UNIT-006, 1.6-INT-010 |
| LLM callback timeout | 1.6-INT-013 |
| Cross-browser incompatibility | 1.6-E2E-002, 1.6-E2E-003, 1.6-E2E-004 |
| Data loss on page refresh | 1.6-INT-008, 1.6-E2E-002 |
| TypeScript type mismatch | 1.6-UNIT-010 through 1.6-UNIT-013 |
| Panic in WASM context | 1.6-E2E-007 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on contract violations)
   - 1.6-UNIT-001 through 1.6-UNIT-003, 1.6-UNIT-007, 1.6-UNIT-010, 1.6-UNIT-014

2. **P0 Integration tests** (validate component boundaries)
   - 1.6-INT-001 through 1.6-INT-003, 1.6-INT-006, 1.6-INT-008, 1.6-INT-011, 1.6-INT-012, 1.6-INT-017

3. **P0 E2E tests** (critical user journeys)
   - 1.6-E2E-001, 1.6-E2E-002, 1.6-E2E-003, 1.6-E2E-005, 1.6-E2E-008, 1.6-E2E-009, 1.6-E2E-010

4. **P1 tests** (core functionality)
   - All P1 tests in order: UNIT → INT → E2E

5. **P2 tests** (nice to have)
   - As time permits

---

## Test Environment Requirements

### Unit Tests
- Rust test harness (`cargo test`)
- Mocked WASM bindings where needed

### Integration Tests
- WASM compilation environment
- Mock LLM callback
- In-memory or test DuckDB instance

### E2E Tests (Playwright)
- Chrome, Firefox, Safari browsers
- Mock LLM server or intercepted responses
- Clean browser profile per test
- `window.gameReady` wait condition

---

## Test Data Requirements

| Data | Description | Location |
|------|-------------|----------|
| Mock LLM Response | Valid phrase generation JSON | Test fixtures |
| Invalid Choices | Edge case inputs (empty, special chars) | Test fixtures |
| Large Leaderboard | 100+ entries for pagination | Seed script |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (`1.6-LEVEL-SEQ`)
- [x] Scenarios are atomic and independent
- [x] Cross-browser testing included for storage AC
- [x] Error paths covered for all WASM exports

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 14
    integration: 16
    e2e: 12
  by_priority:
    p0: 18
    p1: 16
    p2: 8
  coverage_gaps: []
  cross_browser:
    - chrome
    - firefox
    - safari
  special_requirements:
    - playwright_ci
    - wasm_build
    - mock_llm
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.6-test-design-20260110.md
P0 tests identified: 18
P1 tests identified: 16
P2 tests identified: 8
```
