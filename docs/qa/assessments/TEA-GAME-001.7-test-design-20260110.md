# Test Design: Story TEA-GAME-001.7

**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)
**Story:** Browser UI Implementation

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 38 |
| Unit tests | 12 (32%) |
| Integration tests | 14 (37%) |
| E2E tests | 12 (31%) |

**Priority Distribution:**

| Priority | Count | Percentage |
|----------|-------|------------|
| P0 | 8 | 21% |
| P1 | 16 | 42% |
| P2 | 10 | 26% |
| P3 | 4 | 11% |

## Test Scenarios by Acceptance Criteria

---

### AC-1: New "Game" tab in `index.html` alongside YAML workflow tab

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-001 | Unit | P2 | Tab state management toggles active tab correctly | Pure state logic |
| TEA-GAME-001.7-INT-001 | Integration | P1 | Tab click switches visible content container | DOM interaction with state |
| TEA-GAME-001.7-E2E-001 | E2E | P1 | User can navigate between Workflow and Game tabs | User journey validation |

**Notes:** Tab navigation is foundational - if broken, entire game is inaccessible.

---

### AC-2: Welcome screen with random username display and "Start Game" button

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-002 | Unit | P2 | Username generation returns valid format (e.g., AdjectiveNounNumber) | Pure function logic |
| TEA-GAME-001.7-INT-002 | Integration | P0 | Start Game button triggers WASM `game_start_session()` call | Critical WASM bridge |
| TEA-GAME-001.7-INT-003 | Integration | P1 | Session start returns username and initializes game state | State initialization |
| TEA-GAME-001.7-E2E-002 | E2E | P1 | Welcome screen displays username and allows game start | Core user journey |

**Notes:** Session start is P0 as it gates all gameplay functionality.

---

### AC-3: Game screen shows phrase with blank, 5 word buttons, current stats

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-003 | Unit | P1 | Phrase rendering replaces `___` with styled blank element | Template transformation |
| TEA-GAME-001.7-UNIT-004 | Unit | P1 | Stats calculation (accuracy %) returns correct percentage | Math logic |
| TEA-GAME-001.7-INT-004 | Integration | P0 | `game_generate_round()` returns valid phrase and 5 choices | Critical WASM data flow |
| TEA-GAME-001.7-INT-005 | Integration | P1 | Word buttons populate from round data | DOM population |
| TEA-GAME-001.7-E2E-003 | E2E | P0 | Game screen displays phrase, 5 word choices, and stats correctly | Core gameplay rendering |

**Notes:** Round generation is P0 - broken round generation means no gameplay.

---

### AC-4: Visual feedback on correct/incorrect answer (green/red flash)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-005 | Unit | P1 | Answer comparison returns correct/incorrect boolean | Pure comparison logic |
| TEA-GAME-001.7-INT-006 | Integration | P1 | Correct answer adds `.correct` class to button | CSS class application |
| TEA-GAME-001.7-INT-007 | Integration | P1 | Incorrect answer adds `.incorrect` class and reveals correct word | CSS class + reveal logic |
| TEA-GAME-001.7-E2E-004 | E2E | P1 | User sees green flash on correct guess | Visual feedback validation |
| TEA-GAME-001.7-E2E-005 | E2E | P1 | User sees red flash and correct answer revealed on wrong guess | Visual feedback validation |

**Notes:** Feedback is essential for game experience but not system-critical.

---

### AC-5: Difficulty indicator (progress bar or gauge)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-006 | Unit | P2 | Difficulty percentage maps to progress bar width | Pure calculation |
| TEA-GAME-001.7-UNIT-007 | Unit | P2 | Difficulty label updates correctly (Easy/Medium/Hard) | State-to-label mapping |
| TEA-GAME-001.7-INT-008 | Integration | P2 | Difficulty bar updates after each round | State sync with DOM |
| TEA-GAME-001.7-E2E-006 | E2E | P2 | Difficulty indicator visually reflects game progression | User experience |

**Notes:** Lower priority - enhances UX but game functions without it.

---

### AC-6: "Give Up & Submit" button always visible during game

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-INT-009 | Integration | P0 | Give Up button triggers score submission to WASM | Critical state persistence |
| TEA-GAME-001.7-INT-010 | Integration | P1 | Give Up transitions to leaderboard screen | Screen transition |
| TEA-GAME-001.7-E2E-007 | E2E | P0 | User can give up at any round and see their score on leaderboard | Critical user escape path |

**Notes:** P0 - this is the ONLY way to end a game and save progress.

---

### AC-7: Leaderboard screen shows top 10 with user's position highlighted

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-008 | Unit | P1 | Leaderboard sorting returns correct rank order | Sort logic |
| TEA-GAME-001.7-UNIT-009 | Unit | P1 | Current user detection returns correct boolean | Identity comparison |
| TEA-GAME-001.7-INT-011 | Integration | P1 | Leaderboard fetch returns top 10 entries with user position | WASM data retrieval |
| TEA-GAME-001.7-INT-012 | Integration | P1 | Current user row receives `.current-user` class | CSS highlighting |
| TEA-GAME-001.7-E2E-008 | E2E | P1 | Leaderboard displays correctly with user highlighted | User journey completion |

**Notes:** Leaderboard is the payoff - validates game completion worked.

---

### AC-8: "Play Again" button from leaderboard screen

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-INT-013 | Integration | P1 | Play Again resets game state and starts new session | State reset |
| TEA-GAME-001.7-E2E-009 | E2E | P1 | User can play multiple games in succession | Replayability journey |

**Notes:** Ensures users can replay without page refresh.

---

### AC-9: Loading states during LLM calls

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-010 | Unit | P2 | Loading state disables button interactions | State management |
| TEA-GAME-001.7-INT-014 | Integration | P0 | Loading indicator shows during `game_generate_round()` | Async feedback |
| TEA-GAME-001.7-E2E-010 | E2E | P1 | User sees spinner and "Generating question..." during LLM calls | User experience |

**Notes:** P0 for integration - without loading feedback, users spam-click or think app is frozen.

---

### AC-10: Responsive design matching existing demo style

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.7-UNIT-011 | Unit | P3 | Media query breakpoints apply correct layouts | CSS logic |
| TEA-GAME-001.7-UNIT-012 | Unit | P3 | Color variables match existing demo theme | Style consistency |
| TEA-GAME-001.7-E2E-011 | E2E | P2 | Game UI renders correctly on mobile viewport (375px) | Mobile user journey |
| TEA-GAME-001.7-E2E-012 | E2E | P3 | Dark theme consistency across all game screens | Visual regression |

**Notes:** Lower priority - functional correctness takes precedence.

---

## Risk Coverage

| Risk | Test IDs Mitigating |
|------|---------------------|
| WASM bridge failure | TEA-GAME-001.7-INT-002, INT-004, INT-009, INT-011 |
| State corruption during transitions | TEA-GAME-001.7-INT-003, INT-010, INT-013 |
| LLM timeout/failure | TEA-GAME-001.7-INT-014, E2E-010 |
| User unable to complete game | TEA-GAME-001.7-INT-009, E2E-007 |
| Mobile usability issues | TEA-GAME-001.7-E2E-011 |

## Recommended Execution Order

### Phase 1: Critical Path (P0)
1. TEA-GAME-001.7-INT-002 - Start game WASM call
2. TEA-GAME-001.7-INT-004 - Round generation WASM call
3. TEA-GAME-001.7-INT-009 - Give up submission
4. TEA-GAME-001.7-INT-014 - Loading state during LLM
5. TEA-GAME-001.7-E2E-003 - Game screen rendering
6. TEA-GAME-001.7-E2E-007 - Give up user journey

### Phase 2: Core Functionality (P1)
7. All UNIT P1 tests (001.7-UNIT-003 through UNIT-005, UNIT-008, UNIT-009)
8. All INT P1 tests (001.7-INT-001, INT-003, INT-005 through INT-008, INT-010 through INT-013)
9. All E2E P1 tests (001.7-E2E-001, E2E-002, E2E-004, E2E-005, E2E-008 through E2E-010)

### Phase 3: Secondary (P2+)
10. P2 tests: UNIT-001, UNIT-002, UNIT-006, UNIT-007, INT-008, E2E-006, E2E-011
11. P3 tests: UNIT-011, UNIT-012, E2E-012

## Implementation Notes

### Testing Technology Recommendations

| Level | Recommended Tool | Rationale |
|-------|------------------|-----------|
| Unit | Vitest or Jest | Fast JS testing, DOM mocking with jsdom |
| Integration | Playwright Component Testing | Real browser, WASM support |
| E2E | Playwright | Existing demo uses browser, full stack validation |

### WASM Testing Considerations

1. **Mock Strategy**: Create mock implementations of `game_start_session()`, `game_generate_round()`, `game_submit_answer()`, and `game_get_leaderboard()` for unit/integration tests
2. **Real WASM for E2E**: E2E tests should use actual compiled WASM to validate full stack
3. **Timeout Handling**: LLM calls may take 2-10 seconds; set appropriate timeouts in integration tests

### Test Data Requirements

```javascript
// Mock session data
const mockSession = {
  session_id: "test-session-123",
  username: "SwiftFox42",
  started_at: "2026-01-10T10:00:00Z"
};

// Mock round data
const mockRound = {
  phrase: "The ___ shines brightly in the sky.",
  choices: ["sun", "moon", "star", "light", "lamp"],
  correct_answer: "sun",
  difficulty: 0.5
};

// Mock leaderboard data
const mockLeaderboard = [
  { rank: 1, username: "ProGamer99", score: 95.5, accuracy: 0.92, rounds: 10 },
  { rank: 2, username: "SwiftFox42", score: 88.0, accuracy: 0.85, rounds: 8, current: true },
  // ... more entries
];
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shifted left where possible)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (WASM bridge = P0)
- [x] Test IDs follow naming convention (`TEA-GAME-001.7-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-GAME-001.7
  scenarios_total: 38
  by_level:
    unit: 12
    integration: 14
    e2e: 12
  by_priority:
    p0: 8
    p1: 16
    p2: 10
    p3: 4
  coverage_gaps: []
  critical_paths:
    - "WASM bridge initialization"
    - "Round generation flow"
    - "Score submission and leaderboard"
  test_data_dependencies:
    - "Mock WASM functions"
    - "Sample session/round/leaderboard data"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.7-test-design-20260110.md
P0 tests identified: 8
Critical integration points: game_start_session, game_generate_round, game_submit_answer, game_get_leaderboard
```
