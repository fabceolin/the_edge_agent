# Story TEA-GAME-001.8: Opik Integration and Analytics

## Status

Ready for Development

**Validated:** 2026-01-10
**Validation Notes:** All checklist criteria passed. Test design complete with 24 scenarios covering all 7 ACs. No coverage gaps identified.

## Story

**As a** developer,
**I want** all game actions traced to Opik,
**So that** I can analyze user behavior and model performance.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-OBS-002 (Opik WASM integration)
- Technology: Opik/Comet JS SDK, TEA observability patterns
- Follows pattern: `docs/stories/TEA-OBS-002.opik-wasm-rust-integration.md`
- Touch points: `GameEngine`, WASM exports, `enableOpikTracing()`

**Dependencies:**

- Story 5 (TEA-GAME-001.5): `GameEngine` with tracing hooks
- Story 6 (TEA-GAME-001.6): WASM exports with Opik callback
- TEA-OBS-002: Opik WASM infrastructure

## Acceptance Criteria

1. **AC-1**: Each game round creates an Opik trace with: `phrase`, `choices`, `correct_word`, `selected_word`, `is_correct`, `response_time_ms`, `difficulty`
2. **AC-2**: Session-level trace groups all rounds
3. **AC-3**: LLM phrase generation calls traced with token usage
4. **AC-4**: Leaderboard submissions traced
5. **AC-5**: Uses TEA-OBS-002 `enableOpikTracing()` API
6. **AC-6**: Graceful degradation if Opik not configured
7. **AC-7**: Integration test with mock Opik handler

## Tasks / Subtasks

- [ ] Define Opik span schema (AC-1, AC-2, AC-3, AC-4)
  - [ ] Document span types: `game_session`, `game_round`, `llm_phrase`, `leaderboard_submit`
  - [ ] Define required and optional fields for each span type
  - [ ] Create `OpikGameSpan` struct with serde serialization

- [ ] Implement session-level tracing (AC-2)
  - [ ] Create session trace on `start_session()`
  - [ ] Add session_id as parent for all round traces
  - [ ] Close session trace on `submit_to_leaderboard()` or timeout

- [ ] Implement round tracing (AC-1)
  - [ ] Trace on each `submit_answer()` call
  - [ ] Include all round details in span metadata
  - [ ] Link to parent session trace

- [ ] Implement LLM tracing (AC-3)
  - [ ] Trace phrase generation LLM calls
  - [ ] Capture prompt tokens and completion tokens
  - [ ] Record generation latency

- [ ] Implement leaderboard tracing (AC-4)
  - [ ] Trace on `submit_to_leaderboard()` call
  - [ ] Include final score, rank, and session stats

- [ ] Integrate with TEA-OBS-002 (AC-5)
  - [ ] Use `set_opik_handler()` callback bridge
  - [ ] Fire-and-forget semantics (don't block game flow)
  - [ ] Configure project_name from YAML or settings

- [ ] Implement graceful degradation (AC-6)
  - [ ] Check if Opik handler is registered
  - [ ] Skip tracing if not configured
  - [ ] Log debug message when tracing disabled
  - [ ] No errors or warnings to user

- [ ] Write integration tests (AC-7)
  - [ ] Mock Opik handler that captures spans
  - [ ] Verify span structure and content
  - [ ] Test all span types generated
  - [ ] Test graceful degradation

## Dev Notes

### Opik Span Schema

```rust
#[derive(Debug, Clone, Serialize)]
pub struct OpikGameSpan {
    pub span_id: String,
    pub parent_id: Option<String>,
    pub trace_id: String,
    pub name: String,
    pub span_type: GameSpanType,
    pub start_time: f64,
    pub end_time: Option<f64>,
    pub duration_ms: Option<f64>,
    pub status: String,
    pub metadata: serde_json::Value,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GameSpanType {
    Session,
    Round,
    LlmPhrase,
    LeaderboardSubmit,
}
```

### Session Trace Structure

```json
{
  "span_type": "session",
  "name": "game_session",
  "trace_id": "sess_abc123",
  "metadata": {
    "username": "SwiftFox42",
    "started_at": "2026-01-10T12:00:00Z",
    "project_name": "know-your-model"
  }
}
```

### Round Trace Structure

```json
{
  "span_type": "round",
  "name": "game_round",
  "parent_id": "sess_abc123",
  "metadata": {
    "round_number": 5,
    "phrase": "The ___ shines brightly.",
    "choices": ["sun", "moon", "star", "light", "lamp"],
    "correct_word": "sun",
    "selected_word": "moon",
    "is_correct": false,
    "response_time_ms": 2340,
    "difficulty": 0.65,
    "running_accuracy": 0.6,
    "running_score": 0.32
  }
}
```

### LLM Phrase Trace Structure

```json
{
  "span_type": "llm_phrase",
  "name": "phrase_generation",
  "parent_id": "round_xyz789",
  "metadata": {
    "model": "gemma-3-1b-it",
    "prompt_tokens": 150,
    "completion_tokens": 25,
    "total_tokens": 175,
    "latency_ms": 1200,
    "phrase": "The ___ shines brightly.",
    "word": "sun"
  }
}
```

### Leaderboard Submit Trace

```json
{
  "span_type": "leaderboard_submit",
  "name": "leaderboard_submit",
  "parent_id": "sess_abc123",
  "metadata": {
    "username": "SwiftFox42",
    "final_score": 0.42,
    "accuracy": 0.7,
    "total_answers": 10,
    "avg_difficulty": 0.6,
    "rank": 3,
    "is_new_best": true
  }
}
```

### Rust Integration

```rust
// In GameEngine

impl GameEngine {
    fn trace_round(&self, round: &GameRound, result: &AnswerResult) {
        if let Some(ref opik) = self.config.opik_callback {
            let span = OpikGameSpan {
                span_id: Uuid::new_v4().to_string(),
                parent_id: self.session.as_ref().map(|s| s.id.clone()),
                trace_id: self.session.as_ref().map(|s| s.id.clone()).unwrap_or_default(),
                name: "game_round".to_string(),
                span_type: GameSpanType::Round,
                start_time: round.started_at,
                end_time: Some(now()),
                duration_ms: Some(round.response_time_ms.unwrap_or(0) as f64),
                status: if result.is_correct { "success" } else { "failure" }.to_string(),
                metadata: serde_json::json!({
                    "phrase": round.phrase,
                    "choices": round.choices,
                    "correct_word": round.correct_word,
                    "selected_word": round.selected_word,
                    "is_correct": result.is_correct,
                    "response_time_ms": round.response_time_ms,
                    "difficulty": self.session.as_ref().map(|s| s.current_difficulty),
                }),
            };

            // Fire and forget
            let _ = opik.send_span(span);
        }
    }
}
```

### WASM Opik Callback Bridge

```rust
// Reuse TEA-OBS-002 pattern

thread_local! {
    static GAME_OPIK_HANDLER: RefCell<Option<js_sys::Function>> = RefCell::new(None);
}

#[wasm_bindgen]
pub fn set_game_opik_handler(handler: js_sys::Function) {
    GAME_OPIK_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
}

pub async fn send_opik_span(span: &OpikGameSpan) {
    let handler = GAME_OPIK_HANDLER.with(|h| h.borrow().clone());

    if let Some(handler) = handler {
        let span_json = serde_json::to_string(span).unwrap_or_default();
        let _ = handler.call1(&JsValue::NULL, &JsValue::from_str(&span_json));
    }
}
```

### JavaScript Integration

```javascript
// Enable Opik tracing for game

import { enableOpikTracing } from 'tea-wasm-llm/opik';
import { set_game_opik_handler } from './pkg/tea_wasm_llm.js';

async function initGameTracing() {
  // Use TEA-OBS-002 infrastructure
  await enableOpikTracing({
    projectName: 'know-your-model',
  });

  // Register game-specific handler
  set_game_opik_handler(async (spanJson) => {
    const span = JSON.parse(spanJson);

    // Forward to Opik via TEA-OBS-002 infrastructure
    await window.opikClient?.trace({
      name: span.name,
      input: span.metadata,
      output: { status: span.status },
    });

    return JSON.stringify({ success: true });
  });
}
```

### Graceful Degradation

```rust
impl GameEngine {
    fn should_trace(&self) -> bool {
        self.config.opik_callback.is_some()
    }

    fn trace_if_enabled<F>(&self, trace_fn: F)
    where
        F: FnOnce(&OpikCallback),
    {
        if let Some(ref opik) = self.config.opik_callback {
            if let Err(e) = std::panic::catch_unwind(|| trace_fn(opik)) {
                log::debug!("Opik trace failed (non-blocking): {:?}", e);
            }
        }
    }
}
```

### Playwright Test with Mock Opik

```typescript
test('should trace game rounds to Opik', async ({ page }) => {
  const capturedSpans: any[] = [];

  // Register mock Opik handler
  await page.evaluate(() => {
    window.mockOpikSpans = [];
    set_game_opik_handler((spanJson: string) => {
      window.mockOpikSpans.push(JSON.parse(spanJson));
      return JSON.stringify({ success: true });
    });
  });

  // Play a round
  await page.evaluate(() => window.game_start_session());
  await page.evaluate(() => window.game_generate_round());
  await page.evaluate(() => window.game_submit_answer('sun', 1500));

  // Verify spans captured
  const spans = await page.evaluate(() => window.mockOpikSpans);

  expect(spans).toHaveLength(2); // session + round
  expect(spans[1].span_type).toBe('round');
  expect(spans[1].metadata.phrase).toBeDefined();
});
```

### Relevant Source Tree

```
rust/
├── src/
│   └── games/
│       ├── engine.rs        # MODIFIED: Add trace calls
│       └── opik.rs          # NEW: OpikGameSpan, tracing helpers
├── tea-wasm-llm/
│   ├── src/
│   │   ├── game.rs          # MODIFIED: Wire Opik callback
│   │   └── opik.rs          # TEA-OBS-002 infrastructure
│   └── js/
│       └── opik.ts          # MODIFIED: Game tracing setup
```

## Definition of Done

- [ ] All game events create Opik spans
- [ ] Session traces group child round traces
- [ ] LLM calls traced with token usage
- [ ] Leaderboard submissions traced
- [ ] Graceful degradation when Opik not configured
- [ ] No performance impact on game flow
- [ ] Integration tests verify span content
- [ ] Spans visible in Opik dashboard

---

## QA Results

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 9 (37.5%) |
| Integration tests | 10 (41.7%) |
| E2E tests | 5 (20.8%) |
| AC coverage | 100% (7/7 ACs) |

**Priority Distribution:** 8 P0 (Critical), 11 P1 (High), 5 P2 (Medium)

### Risk Areas Identified

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Tracing blocks game flow | HIGH | GAME-001.8-INT-011, GAME-001.8-UNIT-007 |
| Span data corruption/schema drift | MEDIUM | GAME-001.8-UNIT-001, GAME-001.8-E2E-005 |
| Parent-child correlation failure | MEDIUM | GAME-001.8-INT-005, GAME-001.8-INT-006 |
| WASM bridge failure | MEDIUM | GAME-001.8-INT-010, GAME-001.8-E2E-003 |
| Performance degradation | LOW | Implicit via INT-011 (game functions normally) |

### Recommended Test Scenarios

**P0 (Critical - Must Pass for Release):**
1. `GAME-001.8-UNIT-001`: OpikGameSpan serialization to correct JSON schema
2. `GAME-001.8-UNIT-007`: should_trace() returns false when opik_callback is None
3. `GAME-001.8-INT-004`: Session trace created on start_session()
4. `GAME-001.8-INT-005`: Round spans include session trace_id as parent_id
5. `GAME-001.8-INT-010`: set_game_opik_handler() registers JS callback in WASM
6. `GAME-001.8-INT-011`: Game functions normally when Opik handler not registered
7. `GAME-001.8-INT-001`: Round submission triggers span creation with correct metadata
8. `GAME-001.8-E2E-004`: Mock Opik handler captures all span types during game session

**P1 (High - Strongly Recommended):**
- Session hierarchy validation across 3+ rounds
- LLM span latency capture and token tracking
- Leaderboard submit span linked to session
- TEA-OBS-002 enableOpikTracing() integration

### Concerns / Blockers

1. **Dependency on TEA-OBS-002**: Opik WASM infrastructure must be complete per story dependencies
2. **Async Testing Complexity**: Fire-and-forget semantics require careful handling in tests to avoid race conditions
3. **Token Usage Testing**: Depends on LLM integration from Story TEA-GAME-001.4 being complete
4. **WASM Context Required**: E2E tests require Playwright browser context for WASM bridge testing
5. **Security Note**: Span metadata may contain user identifiers (username) - ensure no PII leakage in logs

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-GAME-001.8-test-design-20260110.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Results section with test design analysis | Quinn (QA Agent) |
