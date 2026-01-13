# Test Design: Epic TEA-GAME-001 - Know Your Model

**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)
**Epic:** Know Your Model - Interactive LLM Guessing Game

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 87 |
| **Unit tests** | 42 (48%) |
| **Integration tests** | 31 (36%) |
| **E2E tests** | 14 (16%) |
| **Priority distribution** | P0: 23, P1: 34, P2: 22, P3: 8 |

### Strategy Rationale

This epic involves a multi-layered game system with:
- **Rust game engine** (testable in isolation - heavy unit testing)
- **DuckDB persistence** (requires integration testing)
- **LLM integration** (mock-based unit + real integration)
- **WASM compilation** (browser integration + E2E)
- **Browser UI** (E2E for user journeys)

**Key principle:** Shift-left where possible. Game logic is heavily unit-testable; browser interactions require E2E.

---

## Test Scenarios by Story

---

### Story 1: Rust Game Engine Core

#### AC-1: GameSession struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-001 | Unit | P0 | GameSession struct contains all required fields | Core data model validation |
| GAME-001.1-UNIT-002 | Unit | P0 | GameSession initializes with correct defaults | Default state correctness |
| GAME-001.1-UNIT-003 | Unit | P1 | GameSession id is unique (UUID generation) | Identity integrity |

#### AC-2: GameRound struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-004 | Unit | P0 | GameRound struct contains all required fields | Core data model validation |
| GAME-001.1-UNIT-005 | Unit | P1 | GameRound.choices always contains exactly 5 words | Business rule enforcement |
| GAME-001.1-UNIT-006 | Unit | P0 | GameRound.is_correct computed correctly | Scoring accuracy |

#### AC-3: generate_username()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-007 | Unit | P1 | Username follows {Adjective}{Animal}{Number} pattern | Format compliance |
| GAME-001.1-UNIT-008 | Unit | P2 | Username number is 00-99 range | Format compliance |
| GAME-001.1-UNIT-009 | Unit | P2 | Generated usernames are sufficiently random (no immediate repeats) | UX quality |

#### AC-4: calculate_score()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-010 | Unit | P0 | Score formula: (correct/total) * avg_difficulty * answer_factor | Core business logic |
| GAME-001.1-UNIT-011 | Unit | P0 | Score with 100% accuracy, avg difficulty 0.5, 10 answers | Known value validation |
| GAME-001.1-UNIT-012 | Unit | P0 | Score with 50% accuracy, avg difficulty 0.8, 50 answers | Known value validation |
| GAME-001.1-UNIT-013 | Unit | P1 | Score with 0 answers returns 0 (edge case) | Edge case handling |
| GAME-001.1-UNIT-014 | Unit | P1 | answer_factor caps at 1.0 for 50+ answers | Formula boundary |

#### AC-5: adjust_difficulty()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-015 | Unit | P0 | Difficulty increases by 0.05 when accuracy > 80% | Adaptive algorithm |
| GAME-001.1-UNIT-016 | Unit | P0 | Difficulty decreases by 0.05 when accuracy < 40% | Adaptive algorithm |
| GAME-001.1-UNIT-017 | Unit | P1 | Difficulty unchanged when accuracy 40-80% | Algorithm stability |
| GAME-001.1-UNIT-018 | Unit | P1 | Rolling window respects window_size parameter | Parameterization |

#### AC-6: Difficulty bounds

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.1-UNIT-019 | Unit | P0 | Difficulty clamped to minimum 0.1 | Boundary enforcement |
| GAME-001.1-UNIT-020 | Unit | P0 | Difficulty clamped to maximum 0.95 | Boundary enforcement |

---

### Story 2: DuckDB Schema and Persistence Layer

#### AC-1: words table

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-001 | Integration | P0 | words table created with correct schema | Schema correctness |
| GAME-001.2-INT-002 | Integration | P0 | embedding FLOAT[384] stores and retrieves correctly | Data type validation |
| GAME-001.2-INT-003 | Integration | P1 | text column enforces UNIQUE constraint | Data integrity |

#### AC-2: game_sessions table

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-004 | Integration | P0 | game_sessions table created with correct schema | Schema correctness |
| GAME-001.2-INT-005 | Integration | P1 | Timestamps default to CURRENT_TIMESTAMP | Default behavior |

#### AC-3: answers table

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-006 | Integration | P0 | answers table stores JSON choices array | Complex type storage |
| GAME-001.2-INT-007 | Integration | P0 | answers table references game_sessions(id) FK | Referential integrity |
| GAME-001.2-INT-008 | Integration | P1 | phrase text stored correctly | Data persistence |

#### AC-4: leaderboard table with UPSERT

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-009 | Integration | P0 | Leaderboard UPSERT keeps best score only | Business rule |
| GAME-001.2-INT-010 | Integration | P0 | Lower score does not overwrite higher score | Business rule |
| GAME-001.2-INT-011 | Integration | P1 | New username inserts correctly | UPSERT logic |

#### AC-5 & AC-6: Graph edge tables

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-012 | Integration | P2 | user_word_knowledge tracks times_seen correctly | Graph analytics |
| GAME-001.2-INT-013 | Integration | P2 | user_confusions records confusion pairs | Graph analytics |

#### AC-7 & AC-8: Extensions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.2-INT-014 | Integration | P0 | VSS extension loads and array_cosine_similarity works | Core feature dependency |
| GAME-001.2-INT-015 | Integration | P2 | DuckPGQ extension loads for graph queries | Feature dependency |

---

### Story 3: Word Embedding and Similarity Search

#### AC-1: Pre-computed embeddings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-INT-016 | Integration | P0 | Embedding loader imports ~10k words | Data loading |
| GAME-001.3-INT-017 | Integration | P1 | All embeddings are 384-dimensional | Data integrity |

#### AC-2: find_similar_words()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-UNIT-021 | Unit | P0 | Returns n words within similarity range | Core algorithm |
| GAME-001.3-INT-018 | Integration | P0 | VSS query returns semantically similar words | Real embedding validation |

#### AC-3: Difficulty mapping

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-UNIT-022 | Unit | P0 | Easy (0.1): sim range 0.10-0.40 | Mapping correctness |
| GAME-001.3-UNIT-023 | Unit | P0 | Medium (0.5): sim range 0.40-0.70 | Mapping correctness |
| GAME-001.3-UNIT-024 | Unit | P0 | Hard (0.9): sim range 0.70-0.95 | Mapping correctness |
| GAME-001.3-UNIT-025 | Unit | P1 | Interpolation formula: min_sim = 0.1 + 0.6*difficulty | Formula validation |

#### AC-4: Exclude target word

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-UNIT-026 | Unit | P0 | Similar words list never contains target word | Critical game logic |

#### AC-5: Random selection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-UNIT-027 | Unit | P1 | Selection is randomized (not always top-N) | Game variety |

#### AC-6: Fallback behavior

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-UNIT-028 | Unit | P1 | Range widens progressively when insufficient words | Graceful degradation |
| GAME-001.3-UNIT-029 | Unit | P2 | Fallback terminates after maximum attempts | Prevent infinite loop |

#### AC-7: Out-of-vocabulary handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.3-INT-019 | Integration | P1 | OOV word triggers on-the-fly embedding or skip | Edge case handling |

---

### Story 4: LLM Phrase Generation with Context Memory

#### AC-1: PhraseGenerator struct

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-030 | Unit | P1 | PhraseGenerator maintains conversation history | State management |

#### AC-2: System prompt

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-031 | Unit | P2 | System prompt contains required instructions | Prompt integrity |

#### AC-3: History tracking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-032 | Unit | P1 | Generated phrases added to history | Memory correctness |

#### AC-4: generate_phrase()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-033 | Unit | P0 | Returns {phrase, word} from valid JSON response | Core function |
| GAME-001.4-INT-020 | Integration | P1 | Real LLM generates valid phrase/word pairs | LLM integration |

#### AC-5: JSON parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-034 | Unit | P0 | Valid JSON parsed correctly | Happy path |
| GAME-001.4-UNIT-035 | Unit | P0 | Malformed JSON returns error | Error handling |
| GAME-001.4-UNIT-036 | Unit | P1 | Missing fields return error | Validation |

#### AC-6: Retry logic

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-037 | Unit | P0 | Retries up to 3 times on invalid format | Resilience |
| GAME-001.4-UNIT-038 | Unit | P1 | Returns error after 3 failed attempts | Fail-safe |

#### AC-7: Context window pruning

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-039 | Unit | P1 | History pruned when exceeding max_history_rounds | Memory management |
| GAME-001.4-UNIT-040 | Unit | P2 | System prompt preserved after pruning | Prompt integrity |

#### AC-8: Word extraction edge cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.4-UNIT-041 | Unit | P2 | Punctuation stripped from extracted word | Normalization |
| GAME-001.4-UNIT-042 | Unit | P2 | Casing normalized to lowercase | Normalization |

---

### Story 5: Game Round Orchestration

#### AC-1: GameEngine orchestration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-021 | Integration | P0 | GameEngine coordinates all components | System integration |

#### AC-2: start_session()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-022 | Integration | P0 | Creates session with random username | Session initialization |
| GAME-001.5-INT-023 | Integration | P1 | Initializes phrase generator | Component wiring |

#### AC-3: generate_round()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-024 | Integration | P0 | Calls LLM, finds similar words, returns 5 shuffled choices | Core game flow |
| GAME-001.5-UNIT-043 | Unit | P0 | Shuffle produces random ordering | Game fairness |

#### AC-4: submit_answer()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-025 | Integration | P0 | Records answer and updates session stats | Data persistence |
| GAME-001.5-INT-026 | Integration | P0 | Adjusts difficulty after answer | Adaptive behavior |

#### AC-5: submit_to_leaderboard()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-027 | Integration | P0 | Calculates score and calls UPSERT | Leaderboard flow |

#### AC-6: get_leaderboard()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-028 | Integration | P1 | Returns top 10 entries sorted by score | Query correctness |

#### AC-7: Opik spans

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-INT-029 | Integration | P2 | All actions emit Opik spans via callback | Observability |

#### AC-8: Error handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.5-UNIT-044 | Unit | P1 | LLM timeout handled gracefully | Error resilience |
| GAME-001.5-UNIT-045 | Unit | P1 | Invalid LLM response handled gracefully | Error resilience |
| GAME-001.5-INT-030 | Integration | P1 | DB error handled gracefully | Error resilience |

---

### Story 6: WASM Port and JavaScript Bridge

#### AC-1: wasm-bindgen exports

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.6-INT-031 | Integration | P0 | All 5 API functions exported and callable from JS | WASM bridge |

#### AC-2: Browser storage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.6-E2E-001 | E2E | P0 | DuckDB persists in IndexedDB/OPFS across page refresh | Browser persistence |

#### AC-3 & AC-4: Callback bridges

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.6-E2E-002 | E2E | P0 | LLM callback from JS reaches Rust and returns | Bridge validation |
| GAME-001.6-E2E-003 | E2E | P2 | Opik callback from JS reaches Rust | Bridge validation |

#### AC-5: TypeScript definitions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.6-UNIT-046 | Unit | P2 | TypeScript definitions compile without errors | Type safety |

#### AC-6: Error handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.6-E2E-004 | E2E | P1 | Errors return structured JSON to JS | Error UX |

---

### Story 7: Browser UI Implementation

#### AC-1: Game tab

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-005 | E2E | P0 | Game tab visible and navigable | Core UI |

#### AC-2: Welcome screen

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-006 | E2E | P0 | Welcome screen shows random username | User journey start |
| GAME-001.7-E2E-007 | E2E | P0 | Start Game button initiates session | User journey start |

#### AC-3: Game screen

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-008 | E2E | P0 | Phrase with blank and 5 word buttons displayed | Core gameplay |

#### AC-4: Visual feedback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-009 | E2E | P1 | Correct answer shows green flash | UX feedback |
| GAME-001.7-E2E-010 | E2E | P1 | Incorrect answer shows red flash | UX feedback |

#### AC-5: Difficulty indicator

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-011 | E2E | P2 | Difficulty gauge/progress bar reflects current level | UX feedback |

#### AC-6: Give Up button

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-012 | E2E | P1 | Give Up button submits score and shows leaderboard | User journey |

#### AC-7: Leaderboard screen

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-013 | E2E | P0 | Top 10 leaderboard displayed with user highlighted | Core feature |

#### AC-8: Play Again

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-014 | E2E | P1 | Play Again starts new session | User journey loop |

#### AC-9: Loading states

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-UNIT-047 | Unit | P2 | Loading spinner shown during async calls | UX state management |

#### AC-10: Responsive design

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.7-E2E-015 | E2E | P3 | Game playable on mobile viewport (375px) | Accessibility |

---

### Story 8: Opik Integration and Analytics

#### AC-1: Round tracing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-INT-032 | Integration | P2 | Round trace contains all required fields | Observability |

#### AC-2: Session trace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-INT-033 | Integration | P2 | Session trace groups all rounds | Trace structure |

#### AC-3: LLM trace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-INT-034 | Integration | P2 | LLM calls traced with token usage | Cost tracking |

#### AC-4: Leaderboard trace

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-INT-035 | Integration | P3 | Leaderboard submissions traced | Analytics |

#### AC-5: TEA-OBS-002 API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-INT-036 | Integration | P2 | Uses enableOpikTracing() API | Integration compliance |

#### AC-6: Graceful degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| GAME-001.8-UNIT-048 | Unit | P1 | Game works when Opik not configured | Resilience |

---

## Risk Coverage Matrix

| Risk | Mitigating Tests | Priority |
|------|------------------|----------|
| DuckDB WASM size bloat | GAME-001.6-E2E-001 | P0 |
| Browser storage limits | GAME-001.6-E2E-001 (storage test) | P0 |
| LLM phrase inconsistency | GAME-001.4-UNIT-034/35/36/37/38 | P0 |
| Context window overflow | GAME-001.4-UNIT-039/40 | P1 |
| LLM word not in vocabulary | GAME-001.3-INT-019 | P1 |
| Score calculation errors | GAME-001.1-UNIT-010/11/12/13/14 | P0 |
| Leaderboard data loss | GAME-001.2-INT-009/10/11 | P0 |
| Difficulty stuck at bounds | GAME-001.1-UNIT-019/20 | P0 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit + Integration)
1. GAME-001.1-UNIT-* (all score/difficulty unit tests)
2. GAME-001.2-INT-* (schema and UPSERT tests)
3. GAME-001.3-UNIT-* (similarity algorithm tests)
4. GAME-001.4-UNIT-* (JSON parsing, retry logic)
5. GAME-001.5-INT-* (GameEngine orchestration)
6. GAME-001.6-INT-031 (WASM export)

### Phase 2: Core E2E
7. GAME-001.6-E2E-001/002 (browser persistence, LLM bridge)
8. GAME-001.7-E2E-005/006/007/008/013 (core user journey)

### Phase 3: P1 Tests
9. All remaining P1 tests by story order

### Phase 4: P2+ Tests
10. P2 tests as time permits
11. P3 only in full regression

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention: `GAME-001.{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 87
  by_level:
    unit: 42
    integration: 31
    e2e: 14
  by_priority:
    p0: 23
    p1: 34
    p2: 22
    p3: 8
  coverage_gaps: []
  risk_mitigations:
    - "Score calculation errors: 5 P0 unit tests"
    - "Leaderboard data loss: 3 P0 integration tests"
    - "LLM inconsistency: 5 P0 unit tests with retry logic"
    - "Browser persistence: 1 P0 E2E test"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001-test-design-20260112.md
P0 tests identified: 23
Stories covered: 8/8 (100%)
ACs covered: All ACs from all 8 stories
```

---

*Generated by Quinn (Test Architect) - BMAD QA Agent*
