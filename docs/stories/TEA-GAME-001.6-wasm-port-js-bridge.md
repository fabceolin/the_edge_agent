# Story TEA-GAME-001.6: WASM Port and JavaScript Bridge

## Status

Ready for Development

**Updated:** 2026-01-10
**Reason:** QA review complete with full test coverage (42 scenarios across all 7 ACs). No blockers identified. Minor concerns documented with mitigations.

## Story

**As a** developer,
**I want** the game engine compiled to WASM with JavaScript bindings,
**So that** it can run in the browser alongside the existing demo.

## Story Context

**Existing System Integration:**

- Integrates with: `tea-wasm-llm` existing WASM infrastructure
- Technology: Rust, wasm-bindgen, wasm-pack, TypeScript
- Follows pattern: `rust/tea-wasm-llm/src/lib.rs` callback bridges
- Touch points: New exports in `tea-wasm-llm` or separate `tea-wasm-game` crate

**Dependencies:**

- Story 5 (TEA-GAME-001.5): Complete `GameEngine`
- TEA-OBS-002: Opik callback bridge pattern

## Acceptance Criteria

1. **AC-1**: `wasm-bindgen` exports for `start_session`, `generate_round`, `submit_answer`, `submit_to_leaderboard`, `get_leaderboard`
2. **AC-2**: IndexedDB or OPFS storage for DuckDB persistence in browser
3. **AC-3**: LLM callback bridge (reuse existing `tea-wasm-llm` pattern)
4. **AC-4**: Opik callback bridge from TEA-OBS-002
5. **AC-5**: TypeScript type definitions for all exports
6. **AC-6**: Error handling returns structured JSON errors
7. **AC-7**: Playwright tests for WASM functions

## Tasks / Subtasks

- [ ] Decide on crate structure (AC-1)
  - [ ] Option A: Add to existing `tea-wasm-llm` crate
  - [ ] Option B: Create new `tea-wasm-game` crate
  - [ ] Document decision rationale

- [ ] Create WASM exports (AC-1)
  - [ ] `game_start_session() -> Promise<string>` (returns session JSON)
  - [ ] `game_generate_round() -> Promise<string>` (returns round JSON)
  - [ ] `game_submit_answer(choice: string, time_ms: u32) -> Promise<string>`
  - [ ] `game_submit_to_leaderboard() -> Promise<string>`
  - [ ] `game_get_leaderboard(limit: u32) -> Promise<string>`
  - [ ] `game_get_session_stats() -> Promise<string>`

- [ ] Implement browser storage (AC-2)
  - [ ] Research DuckDB WASM storage options (IndexedDB, OPFS)
  - [ ] Implement storage adapter for game database
  - [ ] Handle storage quota and persistence
  - [ ] Test across browsers (Chrome, Firefox, Safari)

- [ ] Wire LLM callback (AC-3)
  - [ ] Reuse existing `set_llm_handler()` pattern
  - [ ] Or create game-specific `set_game_llm_handler()`
  - [ ] Ensure async callback works with phrase generator

- [ ] Wire Opik callback (AC-4)
  - [ ] Reuse TEA-OBS-002 `set_opik_handler()` pattern
  - [ ] Connect to `GameEngine` Opik callback
  - [ ] Ensure fire-and-forget semantics

- [ ] Generate TypeScript declarations (AC-5)
  - [ ] Create `game.d.ts` with all function signatures
  - [ ] Define `GameSession`, `GameRound`, `LeaderboardEntry` interfaces
  - [ ] Export from package entry point

- [ ] Implement error handling (AC-6)
  - [ ] All WASM functions return JSON with `success` boolean
  - [ ] Error responses include `error` and `error_type` fields
  - [ ] Handle panics gracefully (console.error + structured error)

- [ ] Write Playwright tests (AC-7)
  - [ ] Test `start_session` creates valid session
  - [ ] Test `generate_round` with mock LLM
  - [ ] Test `submit_answer` updates stats correctly
  - [ ] Test `submit_to_leaderboard` persists score
  - [ ] Test `get_leaderboard` returns sorted entries
  - [ ] Test error handling for edge cases

## Dev Notes

### WASM Export Structure

```rust
// rust/tea-wasm-llm/src/game.rs (or tea-wasm-game/src/lib.rs)

use wasm_bindgen::prelude::*;
use std::cell::RefCell;

thread_local! {
    static GAME_ENGINE: RefCell<Option<GameEngine>> = RefCell::new(None);
}

#[wasm_bindgen]
pub async fn game_start_session() -> Result<JsValue, JsValue> {
    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();

        // Initialize if needed
        if engine.is_none() {
            let config = GameEngineConfig::default();
            *engine = Some(GameEngine::new(config)?);
        }

        engine.as_mut().unwrap().start_session()
    });

    match result {
        Ok(session) => Ok(JsValue::from_str(&serde_json::to_string(&session)?)),
        Err(e) => Ok(JsValue::from_str(&json!({
            "success": false,
            "error": e.to_string(),
            "error_type": "session_error"
        }).to_string())),
    }
}

#[wasm_bindgen]
pub async fn game_generate_round() -> Result<JsValue, JsValue> {
    // Get LLM callback from registered handler
    let llm_callback = get_llm_callback()?;

    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        let engine = engine.as_mut().ok_or(GameError::NoSession)?;
        engine.generate_round(llm_callback).await
    });

    to_json_result(result)
}

#[wasm_bindgen]
pub async fn game_submit_answer(choice: String, time_ms: u32) -> Result<JsValue, JsValue> {
    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        let engine = engine.as_mut().ok_or(GameError::NoSession)?;
        engine.submit_answer(&choice, time_ms)
    });

    to_json_result(result)
}

fn to_json_result<T: Serialize>(result: Result<T, GameError>) -> Result<JsValue, JsValue> {
    match result {
        Ok(value) => {
            let json = serde_json::json!({
                "success": true,
                "data": value
            });
            Ok(JsValue::from_str(&json.to_string()))
        }
        Err(e) => {
            let json = serde_json::json!({
                "success": false,
                "error": e.to_string(),
                "error_type": error_type(&e)
            });
            Ok(JsValue::from_str(&json.to_string()))
        }
    }
}
```

### TypeScript Declarations

```typescript
// game.d.ts

export interface GameSession {
  id: string;
  username: string;
  total_answers: number;
  correct_answers: number;
  current_difficulty: number;
  score: number;
}

export interface GameRound {
  id: string;
  phrase: string;
  choices: string[];  // 5 shuffled words
  // correct_word not exposed to client
}

export interface AnswerResult {
  success: boolean;
  is_correct: boolean;
  correct_word: string;
  current_score: number;
  current_difficulty: number;
}

export interface LeaderboardEntry {
  rank: number;
  username: string;
  score: number;
  accuracy: number;
  total_answers: number;
  avg_difficulty: number;
}

export interface GameError {
  success: false;
  error: string;
  error_type: 'session_error' | 'llm_error' | 'db_error' | 'invalid_choice';
}

// Function signatures
export function game_start_session(): Promise<GameSession | GameError>;
export function game_generate_round(): Promise<GameRound | GameError>;
export function game_submit_answer(choice: string, time_ms: number): Promise<AnswerResult | GameError>;
export function game_submit_to_leaderboard(): Promise<{ success: boolean; rank?: number } | GameError>;
export function game_get_leaderboard(limit?: number): Promise<LeaderboardEntry[] | GameError>;
export function game_get_session_stats(): Promise<GameSession | GameError>;
```

### DuckDB WASM Storage

```javascript
// Browser storage options for DuckDB WASM

// Option 1: OPFS (Origin Private File System) - Chrome 102+
const db = await duckdb.open({
  path: 'game.duckdb',
  accessMode: duckdb.AccessMode.READ_WRITE,
});

// Option 2: IndexedDB via duckdb-wasm
const db = await duckdb.open({
  path: ':indexeddb:game.duckdb',
});

// Option 3: In-memory (loses data on refresh)
const db = await duckdb.open({
  path: ':memory:',
});
```

### Playwright Test Example

```typescript
// rust/tea-wasm-llm/tests/game.spec.ts

import { test, expect } from '@playwright/test';

test.describe('Know Your Model Game', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/wasm-demo/');
    await page.waitForFunction(() => window.gameReady === true);
  });

  test('should start a new session', async ({ page }) => {
    const result = await page.evaluate(async () => {
      return await window.game_start_session();
    });

    expect(result.success).toBe(true);
    expect(result.data.username).toMatch(/^[A-Z][a-z]+[A-Z][a-z]+\d{2}$/);
  });

  test('should generate a round with 5 choices', async ({ page }) => {
    await page.evaluate(() => window.game_start_session());

    // Mock LLM response
    await page.evaluate(() => {
      window.mockLlmResponse = '{"phrase": "The ___ is bright.", "word": "sun"}';
    });

    const result = await page.evaluate(async () => {
      return await window.game_generate_round();
    });

    expect(result.success).toBe(true);
    expect(result.data.choices).toHaveLength(5);
    expect(result.data.phrase).toContain('___');
  });
});
```

### Relevant Source Tree

```
rust/
├── tea-wasm-llm/
│   ├── src/
│   │   ├── lib.rs           # Add: mod game; re-exports
│   │   ├── game.rs          # NEW: WASM game exports
│   │   └── ...
│   ├── js/
│   │   ├── index.ts         # Add game exports
│   │   └── game.d.ts        # NEW: TypeScript declarations
│   └── tests/
│       └── game.spec.ts     # NEW: Playwright tests
```

### Build Commands

```bash
# Build WASM with game feature
cd rust/tea-wasm-llm
wasm-pack build --target web --features game

# Run Playwright tests
npx playwright test tests/game.spec.ts
```

## Definition of Done

- [ ] All WASM exports work correctly in browser
- [ ] DuckDB persistence survives page refresh
- [ ] LLM callback integrates with phrase generator
- [ ] Opik callback records game events
- [ ] TypeScript declarations are accurate
- [ ] All errors return structured JSON
- [ ] Playwright tests pass in CI

---

## QA Notes

**QA Review Date:** 2026-01-10
**Reviewed by:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 42 |
| **Unit Tests** | 14 (33%) |
| **Integration Tests** | 16 (38%) |
| **E2E Tests** | 12 (29%) |
| **P0 (Critical)** | 18 tests |
| **P1 (High)** | 16 tests |
| **P2 (Medium)** | 8 tests |

**Coverage by Acceptance Criteria:**
- AC-1 (WASM exports): 11 tests - Full coverage
- AC-2 (Browser storage): 9 tests - Full coverage including cross-browser
- AC-3 (LLM callback): 5 tests - Full coverage
- AC-4 (Opik callback): 5 tests - Full coverage
- AC-5 (TypeScript types): 5 tests - Full coverage
- AC-6 (Error handling): 6 tests - Full coverage
- AC-7 (Playwright tests): 5 explicit E2E tests + E2E coverage from other ACs

### Risk Areas Identified

| Risk | Severity | Mitigation | Test Coverage |
|------|----------|------------|---------------|
| **WASM initialization failure** | High | Fail-fast error handling with structured JSON | 1.6-E2E-001, 1.6-INT-001 |
| **Cross-browser storage incompatibility** | High | OPFS with IndexedDB fallback | 1.6-E2E-002, 1.6-E2E-003, 1.6-E2E-004 |
| **Data loss on page refresh** | High | Persistence validation tests | 1.6-INT-008, 1.6-E2E-002 |
| **LLM callback timeout** | Medium | Timeout error handling | 1.6-INT-013 |
| **Browser storage quota exceeded** | Medium | Error boundary + user notification | 1.6-UNIT-006, 1.6-INT-010 |
| **TypeScript type drift** | Medium | Compile-time validation | 1.6-UNIT-010 through 1.6-UNIT-013 |
| **WASM panic handling** | Medium | Graceful degradation to structured error | 1.6-E2E-007 |

### Recommended Test Scenarios

**P0 Critical Path (Execute First):**
1. Full session lifecycle: start → generate → answer → leaderboard (1.6-E2E-001)
2. Session persistence across page refresh - Chrome (1.6-E2E-002)
3. Async LLM callback resolution in WASM context (1.6-INT-012)
4. DuckDB OPFS storage initialization (1.6-INT-006)
5. TypeScript declarations compile without errors (1.6-UNIT-010)
6. Error response contract validation (1.6-UNIT-014)

**Cross-Browser Validation (Required):**
- Chrome (OPFS primary)
- Firefox (IndexedDB fallback)
- Safari (IndexedDB fallback)

**Special Test Requirements:**
- Playwright CI integration
- WASM build pipeline
- Mock LLM server or intercepted responses
- Clean browser profile per test

### Concerns and Blockers

**Blockers:** None identified

**Concerns:**
1. **Safari OPFS Support:** Safari has limited OPFS support. Ensure IndexedDB fallback is robust and tested.
2. **DuckDB WASM Maturity:** DuckDB WASM is relatively new; monitor for browser-specific quirks.
3. **Async Callback Complexity:** The JS ↔ WASM ↔ Rust callback chain for LLM is complex. Ensure timeout handling covers all failure modes.
4. **Storage Quota:** Browser storage quotas vary. Consider adding user-facing quota warnings before exceeding limits.
5. **Cold Start Performance:** WASM + DuckDB initialization may have noticeable latency. Consider lazy loading or progress indicators.

### Test Environment Requirements

- **Unit Tests:** Rust test harness (`cargo test`) with mocked WASM bindings
- **Integration Tests:** Full WASM compilation, mock LLM callback, test DuckDB instance
- **E2E Tests:** Playwright with Chrome, Firefox, Safari; mock LLM server; `window.gameReady` wait condition

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-GAME-001.6-test-design-20260110.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Notes section | Quinn (Test Architect) |
