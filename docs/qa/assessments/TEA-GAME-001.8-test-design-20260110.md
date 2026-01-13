# Test Design: Story TEA-GAME-001.8

**Title:** Opik Integration and Analytics
**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 9 (37.5%) |
| Integration tests | 10 (41.7%) |
| E2E tests | 5 (20.8%) |
| P0 (Critical) | 8 |
| P1 (High) | 11 |
| P2 (Medium) | 5 |

### Risk Assessment

This story involves **observability infrastructure** which is:
- **Not revenue-critical** (tracing is telemetry, not core functionality)
- **High importance** for debugging and analytics
- **Must not impact game performance** (fire-and-forget semantics)
- **Security consideration**: Span data may contain user identifiers

### Testing Philosophy

1. **Unit tests** for span construction and serialization logic
2. **Integration tests** for WASM bridge, callback handling, and tracing flow
3. **E2E tests** for complete tracing workflow with mock Opik handler

---

## Test Scenarios by Acceptance Criteria

### AC-1: Round Tracing with Metadata

Each game round creates an Opik trace with: `phrase`, `choices`, `correct_word`, `selected_word`, `is_correct`, `response_time_ms`, `difficulty`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-UNIT-001 | Unit | P0 | Verify `OpikGameSpan` struct serializes to correct JSON schema | Core data contract - serialization correctness critical |
| GAME-001.8-UNIT-002 | Unit | P1 | Verify `GameSpanType::Round` variant serializes to "round" | Ensures span type discrimination works |
| GAME-001.8-UNIT-003 | Unit | P1 | Verify span metadata includes all required round fields | Validates completeness of captured data |
| GAME-001.8-INT-001 | Integration | P0 | Round submission triggers span creation with correct metadata | Validates round tracing flow end-to-end |
| GAME-001.8-INT-002 | Integration | P1 | Span captures correct response_time_ms from round timing | Validates timing accuracy |
| GAME-001.8-INT-003 | Integration | P1 | Span captures difficulty from session state | Cross-component data flow |

### AC-2: Session-Level Trace Grouping

Session-level trace groups all rounds

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-UNIT-004 | Unit | P1 | Verify session span has unique trace_id | Uniqueness critical for correlation |
| GAME-001.8-INT-004 | Integration | P0 | Session trace created on `start_session()` | Critical initialization flow |
| GAME-001.8-INT-005 | Integration | P0 | Round spans include session trace_id as parent_id | Parent-child correlation is core functionality |
| GAME-001.8-INT-006 | Integration | P1 | Multiple rounds share same parent session trace_id | Verifies session grouping works across rounds |
| GAME-001.8-E2E-001 | E2E | P1 | Complete session with 3+ rounds all traced with correct hierarchy | Validates full session tracing workflow |

### AC-3: LLM Phrase Generation Tracing with Token Usage

LLM phrase generation calls traced with token usage

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-UNIT-005 | Unit | P1 | Verify LLM span metadata includes token counts (prompt, completion, total) | Schema compliance |
| GAME-001.8-INT-007 | Integration | P1 | LLM phrase generation creates span with latency_ms | Validates latency capture |
| GAME-001.8-INT-008 | Integration | P2 | LLM span links to parent round span | Hierarchy correctness |

### AC-4: Leaderboard Submission Tracing

Leaderboard submissions traced

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-UNIT-006 | Unit | P1 | Verify leaderboard span metadata includes final_score, accuracy, rank | Schema compliance |
| GAME-001.8-INT-009 | Integration | P1 | Leaderboard submit creates span linked to session | Validates leaderboard tracing flow |
| GAME-001.8-E2E-002 | E2E | P2 | Complete game session ending with leaderboard submit produces all span types | Full workflow validation |

### AC-5: TEA-OBS-002 API Integration

Uses TEA-OBS-002 `enableOpikTracing()` API

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-INT-010 | Integration | P0 | `set_game_opik_handler()` registers JS callback in WASM | WASM bridge is critical integration point |
| GAME-001.8-E2E-003 | E2E | P1 | Game tracing works with `enableOpikTracing()` from TEA-OBS-002 | Validates integration with existing infrastructure |

### AC-6: Graceful Degradation

Graceful degradation if Opik not configured

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-UNIT-007 | Unit | P0 | `should_trace()` returns false when opik_callback is None | Core degradation logic |
| GAME-001.8-UNIT-008 | Unit | P2 | `trace_if_enabled()` catches panics without propagating | Defensive programming validation |
| GAME-001.8-INT-011 | Integration | P0 | Game functions normally when Opik handler not registered | Critical - must not block game |
| GAME-001.8-INT-012 | Integration | P2 | No errors/warnings logged to user when Opik disabled | UX requirement |

### AC-7: Integration Tests with Mock Handler

Integration test with mock Opik handler

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| GAME-001.8-E2E-004 | E2E | P0 | Mock Opik handler captures all span types during game session | Validates complete tracing workflow |
| GAME-001.8-E2E-005 | E2E | P1 | Mock handler receives spans with correct structure and content | Content validation |
| GAME-001.8-UNIT-009 | Unit | P2 | Span JSON can be deserialized back to struct (round-trip) | Ensures JSON is valid and parseable |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Tracing blocks game flow | GAME-001.8-INT-011, GAME-001.8-UNIT-007 |
| Span data corruption | GAME-001.8-UNIT-001, GAME-001.8-E2E-005 |
| Parent-child correlation failure | GAME-001.8-INT-005, GAME-001.8-INT-006 |
| WASM bridge failure | GAME-001.8-INT-010, GAME-001.8-E2E-003 |
| Performance degradation | GAME-001.8-INT-011 (implicit - game functions normally) |

---

## Recommended Execution Order

### Phase 1: Unit Tests (Fast Feedback)
1. GAME-001.8-UNIT-001 (P0) - Serialization
2. GAME-001.8-UNIT-007 (P0) - Degradation check
3. GAME-001.8-UNIT-004 (P1) - Session trace_id
4. GAME-001.8-UNIT-002 through UNIT-006 (P1) - Schema validation

### Phase 2: Integration Tests
1. GAME-001.8-INT-004 (P0) - Session creation
2. GAME-001.8-INT-005 (P0) - Parent-child linkage
3. GAME-001.8-INT-010 (P0) - WASM bridge
4. GAME-001.8-INT-011 (P0) - Graceful degradation
5. GAME-001.8-INT-001 (P0) - Round tracing
6. P1 integration tests in order

### Phase 3: E2E Tests
1. GAME-001.8-E2E-004 (P0) - Mock handler captures spans
2. GAME-001.8-E2E-001 (P1) - Full session hierarchy
3. GAME-001.8-E2E-003 (P1) - TEA-OBS-002 integration
4. GAME-001.8-E2E-005 (P1) - Content validation
5. GAME-001.8-E2E-002 (P2) - Leaderboard flow

---

## Test Implementation Guidance

### Unit Tests (Rust)

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;

    // GAME-001.8-UNIT-001
    #[test]
    fn test_opik_game_span_serialization() {
        let span = OpikGameSpan {
            span_id: "span_123".into(),
            parent_id: Some("sess_abc".into()),
            trace_id: "sess_abc".into(),
            name: "game_round".into(),
            span_type: GameSpanType::Round,
            start_time: 1704844800.0,
            end_time: Some(1704844802.0),
            duration_ms: Some(2000.0),
            status: "success".into(),
            metadata: serde_json::json!({
                "phrase": "The ___ shines.",
                "choices": ["sun", "moon"],
                "correct_word": "sun",
                "selected_word": "sun",
                "is_correct": true,
                "response_time_ms": 2000,
                "difficulty": 0.5
            }),
        };

        let json = serde_json::to_string(&span).unwrap();
        assert!(json.contains("\"span_type\":\"round\""));
        assert!(json.contains("\"is_correct\":true"));
    }

    // GAME-001.8-UNIT-007
    #[test]
    fn test_should_trace_returns_false_when_no_handler() {
        let engine = GameEngine::new(GameConfig {
            opik_callback: None,
            ..Default::default()
        });
        assert!(!engine.should_trace());
    }
}
```

### Integration Tests (Playwright)

```typescript
// GAME-001.8-E2E-004
test('mock Opik handler captures all span types', async ({ page }) => {
  const capturedSpans: any[] = [];

  await page.evaluate(() => {
    window.mockOpikSpans = [];
    set_game_opik_handler((spanJson: string) => {
      window.mockOpikSpans.push(JSON.parse(spanJson));
      return JSON.stringify({ success: true });
    });
  });

  // Play complete game session
  await page.evaluate(() => window.game_start_session());
  await page.evaluate(() => window.game_generate_round());
  await page.evaluate(() => window.game_submit_answer('sun', 1500));
  await page.evaluate(() => window.game_submit_to_leaderboard('TestUser'));

  const spans = await page.evaluate(() => window.mockOpikSpans);
  const spanTypes = spans.map(s => s.span_type);

  expect(spanTypes).toContain('session');
  expect(spanTypes).toContain('round');
  expect(spanTypes).toContain('llm_phrase');
  expect(spanTypes).toContain('leaderboard_submit');
});
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention: `GAME-001.8-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-GAME-001.8
  story_title: Opik Integration and Analytics
  designer: Quinn (Test Architect)
  date: 2026-01-10
  scenarios_total: 24
  by_level:
    unit: 9
    integration: 12
    e2e: 5
  by_priority:
    p0: 8
    p1: 11
    p2: 5
    p3: 0
  coverage_gaps: []
  notes:
    - Fire-and-forget semantics require careful async testing
    - WASM bridge testing requires Playwright browser context
    - Token usage testing depends on LLM integration from Story 4
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.8-test-design-20260110.md
P0 tests identified: 8
Test coverage: 100% of acceptance criteria
Recommended test framework: Rust #[test] for unit, Playwright for E2E
```
