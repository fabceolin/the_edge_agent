# Test Design: Story TEA-GAME-001.9

**Date**: 2026-01-25
**Designer**: Quinn (Test Architect)
**Story**: Phrase Database Integration - Brownfield Addition

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 10 (42%) |
| Integration tests | 9 (38%) |
| E2E tests | 5 (21%) |
| Priority distribution | P0: 8, P1: 10, P2: 6 |

### Coverage Summary

All 12 Acceptance Criteria have test coverage. High-risk areas (bundle size, UI compatibility) have multi-level testing.

---

## Test Scenarios by Acceptance Criteria

### AC-1: DuckDB Schema - `phrases` table created

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-001 | Unit | P1 | Phrase struct serialization/deserialization | Pure data struct validation |
| 001.9-INT-001 | Integration | P0 | `phrases` table DDL executes without error | Schema creation critical |
| 001.9-INT-002 | Integration | P1 | Index on `difficulty` column exists | Query performance |

**Given-When-Then:**
```gherkin
Scenario: Phrase table schema creation
  Given a fresh DuckDB database
  When game engine initializes
  Then phrases table exists with columns (id, phrase, correct_word, distractors, difficulty, category)
  And idx_phrases_difficulty index exists
```

---

### AC-2: Phrase Loading from JSON

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-002 | Unit | P0 | Parse `game_phrases.json` structure | Core data loading |
| 001.9-UNIT-003 | Unit | P1 | PhrasesFile struct deserializes version field | Schema validation |
| 001.9-INT-003 | Integration | P0 | All 1039 phrases load into DuckDB | Data completeness |
| 001.9-INT-004 | Integration | P1 | Compile-time include works (WASM) | WASM compatibility |

**Given-When-Then:**
```gherkin
Scenario: Phrase JSON loading
  Given game_phrases.json with 1039 phrases
  When phrases are loaded via include_str!
  Then 1039 phrases are inserted into DuckDB
  And each phrase has valid id, phrase, correct_word, distractors, difficulty, category
```

---

### AC-3: Random Phrase Selection with Difficulty Filter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-004 | Unit | P0 | `get_random_phrase(0.4, 0.6, [])` returns phrase in range | Core game logic |
| 001.9-UNIT-005 | Unit | P0 | Exclusion list filters used phrases | Prevents repeats |
| 001.9-UNIT-006 | Unit | P1 | Empty result when all phrases excluded | Edge case |
| 001.9-UNIT-007 | Unit | P1 | Difficulty range expansion fallback | Exhaustion recovery |

**Given-When-Then:**
```gherkin
Scenario: Random phrase selection with exclusion
  Given 100 phrases with difficulty 0.4-0.6
  And exclude_ids contains ["phrase_001", "phrase_002"]
  When get_random_phrase(0.4, 0.6, exclude_ids) is called
  Then returned phrase difficulty is between 0.4 and 0.6
  And returned phrase.id is not in exclude_ids
```

---

### AC-4: Session Tracks Used Phrase IDs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-UNIT-008 | Unit | P1 | `used_phrase_ids` initializes empty | State init |
| 001.9-INT-005 | Integration | P0 | Playing 10 rounds adds 10 unique phrase IDs | Core tracking |
| 001.9-INT-006 | Integration | P2 | Session with 50+ rounds doesn't repeat phrases | Long session |

**Given-When-Then:**
```gherkin
Scenario: Session tracks used phrases
  Given a new game session
  When player completes 10 rounds
  Then session.used_phrase_ids contains 10 unique IDs
  And no phrase was shown twice
```

---

### AC-5: `game_generate_round()` Uses Phrase Database

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-INT-007 | Integration | P0 | Round generation uses DB phrase (not LLM) | Core refactor validation |
| 001.9-UNIT-009 | Unit | P1 | Choices array contains correct_word + 4 distractors | Game mechanics |
| 001.9-UNIT-010 | Unit | P1 | Choices are shuffled | Randomization |

**Given-When-Then:**
```gherkin
Scenario: Round uses database phrase
  Given session with difficulty 0.5
  When game_generate_round() is called
  Then returned phrase is from phrases table
  And phrase.difficulty is within 0.35-0.65
  And choices contains phrase.correct_word and phrase.distractors
```

---

### AC-6: LLM Called Only for Completion

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-INT-008 | Integration | P1 | LLM prompt is "Complete with ONE word: {phrase}" | Prompt format |
| 001.9-E2E-001 | E2E | P0 | Full round flow: DB phrase -> player answer -> LLM completion -> score | End-to-end validation |

**Given-When-Then:**
```gherkin
Scenario: LLM completion only
  Given a round with phrase "The ___ is bright."
  When player submits answer
  Then LLM is called with prompt "Complete with ONE word: The ___ is bright."
  And LLM returns single word response
```

---

### AC-7: Existing GameSession/GameRound/Scoring Unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-E2E-002 | E2E | P0 | Existing game.rs unit tests pass | Regression |
| 001.9-INT-009 | Integration | P1 | Score calculation uses same formula | Scoring parity |

**Given-When-Then:**
```gherkin
Scenario: Scoring unchanged
  Given player answers 5 correct at difficulty 0.5
  When score is calculated
  Then score matches expected formula: accuracy * avg_difficulty * answer_factor
```

---

### AC-8: Leaderboard/Stats Persistence Unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-E2E-003 | E2E | P1 | Leaderboard submission works after round | Persistence |

**Given-When-Then:**
```gherkin
Scenario: Leaderboard still works
  Given player completes 3 rounds
  When player submits to leaderboard
  Then entry appears in leaderboard with correct stats
```

---

### AC-9: WASM Bridge Exports Compatible

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.9-E2E-004 | E2E | P0 | game.js renderRound() works with new round format | UI compatibility |
| 001.9-E2E-005 | E2E | P0 | game.js submitAnswer() flow unchanged | UI compatibility |

**Given-When-Then:**
```gherkin
Scenario: game.js UI compatibility
  Given WASM module loaded in browser
  When game_generate_round() returns
  Then renderRound() displays phrase and 5 choices
  And submitAnswer() processes player choice correctly
```

---

### AC-10: Phrase Loading Tested with Sample Data

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| (Covered by 001.9-INT-003) | Integration | P0 | Sample data test | Explicit requirement |

---

### AC-11: Random Selection with Exclusion Tested

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| (Covered by 001.9-UNIT-004, 001.9-UNIT-005) | Unit | P0 | Exclusion test | Explicit requirement |

---

### AC-12: No Regression in Existing Functionality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| (Covered by 001.9-E2E-002) | E2E | P0 | Regression test | Explicit requirement |

---

## Risk Coverage

| Risk ID | Test Coverage | Verification |
|---------|---------------|--------------|
| TECH-001 (Bundle Size) | Manual measurement | Measure WASM size before/after |
| TECH-002 (JSON Format) | 001.9-E2E-004, 001.9-E2E-005 | game.js compatibility tests |
| DATA-001 (Phrase Exhaustion) | 001.9-UNIT-006, 001.9-UNIT-007 | Exhaustion + fallback tests |
| TECH-003 (Schema Migration) | 001.9-INT-001 | Fresh DB creation test |
| TECH-004 (State Growth) | 001.9-INT-006 | 50+ round session test |
| PERF-001 (Query Latency) | Manual benchmark | `get_random_phrase()` latency |

---

## Test Data Requirements

### Sample Phrases (Minimum Test Dataset)

```json
{
  "version": "1.0-test",
  "phrases": [
    {"id": "test_001", "phrase": "The ___ is bright.", "correct_word": "sun", "distractors": ["moon","star","light","dawn"], "difficulty": 0.1, "category": "nature"},
    {"id": "test_002", "phrase": "She felt ___ after the news.", "correct_word": "happy", "distractors": ["relieved","excited","grateful","proud"], "difficulty": 0.3, "category": "emotions"},
    {"id": "test_003", "phrase": "The ___ paradigm uses immutable data.", "correct_word": "functional", "distractors": ["procedural","modular","reactive","declarative"], "difficulty": 0.8, "category": "technology"}
  ]
}
```

### Environment Requirements

| Environment | Purpose | Requirements |
|-------------|---------|--------------|
| Unit Tests | Rust logic | `cargo test` in `rust/tea-wasm-llm/` |
| Integration Tests | DuckDB + WASM | `wasm-pack test --headless --chrome` |
| E2E Tests | Browser flow | Chrome/Firefox + `docs/extra/wasm-demo/` |

### Test Fixtures

1. **Fresh DuckDB**: Empty database for schema tests
2. **Populated DuckDB**: Database with 1039 phrases loaded
3. **Exhausted Session**: Session with all difficulty-range phrases used
4. **Long Session**: 50+ rounds for memory/state tests

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit)
1. 001.9-UNIT-002 - JSON parsing
2. 001.9-UNIT-004 - Phrase selection
3. 001.9-UNIT-005 - Exclusion logic

### Phase 2: Schema Validation (P0 Integration)
4. 001.9-INT-001 - Table creation
5. 001.9-INT-003 - Data loading
6. 001.9-INT-007 - Round generation

### Phase 3: E2E Critical Path (P0 E2E)
7. 001.9-E2E-001 - Full game flow
8. 001.9-E2E-002 - Regression tests
9. 001.9-E2E-004 - game.js compatibility
10. 001.9-E2E-005 - submitAnswer flow

### Phase 4: P1 Tests
11-18. All P1 tests in order

### Phase 5: P2 As Time Permits
19-24. Long session, edge cases

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 10
    integration: 9
    e2e: 5
  by_priority:
    p0: 8
    p1: 10
    p2: 6
  coverage_gaps: []
  risk_coverage:
    - risk: TECH-001
      test: manual_measurement
    - risk: TECH-002
      tests: [001.9-E2E-004, 001.9-E2E-005]
    - risk: DATA-001
      tests: [001.9-UNIT-006, 001.9-UNIT-007]
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-GAME-001.9-test-design-20260125.md`
P0 tests identified: 8
