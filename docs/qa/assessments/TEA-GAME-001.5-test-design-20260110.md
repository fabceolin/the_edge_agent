# Test Design: Story TEA-GAME-001.5

**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 42 |
| Unit tests | 18 (43%) |
| Integration tests | 16 (38%) |
| E2E tests | 8 (19%) |
| **Priority distribution** | P0: 16, P1: 14, P2: 8, P3: 4 |

### Strategy Rationale

This story is the **orchestration layer** that coordinates all game components. Testing requires:

1. **Unit tests** for pure logic: scoring calculations, difficulty adjustment, validation
2. **Integration tests** for component coordination: DB + Engine, LLM callbacks + Engine, Embeddings + Engine
3. **E2E tests** for critical user flows: complete game session from start to leaderboard

Given dependencies on Stories 1-4, integration testing is critical to ensure proper coordination.

---

## Test Scenarios by Acceptance Criteria

### AC-1: GameEngine Orchestrates Session, DB, LLM, and Embeddings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-001 | Unit | P0 | Validate `GameEngineConfig` construction with all required fields | Pure struct validation, no side effects |
| 001.5-UNIT-002 | Unit | P1 | Validate `GameEngineConfig` defaults for optional fields | Business logic for defaults |
| 001.5-INT-001 | Integration | P0 | `GameEngine::new()` initializes all dependencies correctly | Multi-component initialization, critical startup path |
| 001.5-INT-002 | Integration | P1 | `GameEngine::new()` fails gracefully with invalid DB path | Error handling at component boundary |
| 001.5-INT-003 | Integration | P1 | `GameEngine::new()` works with missing embeddings (lazy load) | Component initialization edge case |

**Given-When-Then Templates:**

```gherkin
Given a valid GameEngineConfig with db_path, max_history_rounds, and difficulty_window_size
When GameEngine::new(config) is called
Then GameDb, EmbeddingSearch, and PhraseGenerator are initialized
And session is None
And recent_answers queue is empty
```

---

### AC-2: start_session() Creates Session with Random Username

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-003 | Unit | P1 | Random username generation follows expected format | Pure function, string generation logic |
| 001.5-UNIT-004 | Unit | P2 | Random usernames are unique across multiple calls | Probability validation |
| 001.5-INT-004 | Integration | P0 | `start_session()` creates GameSession and persists to DB | Critical user journey start |
| 001.5-INT-005 | Integration | P0 | `start_session()` initializes PhraseGenerator with empty history | Component coordination |
| 001.5-INT-006 | Integration | P1 | `start_session()` returns session info with correct defaults | Data flow validation |
| 001.5-INT-007 | Integration | P2 | `start_session()` records Opik span when callback configured | Observability integration |
| 001.5-INT-008 | Integration | P1 | `start_session()` while session exists replaces old session | State management |

**Given-When-Then Templates:**

```gherkin
Given a GameEngine with no active session
When start_session() is called
Then a new GameSession is created with a random username
And the session has total_answers = 0, correct_answers = 0
And current_difficulty = 0.5 (default)
And the session is persisted to the database
And start_session() returns the session info

Given a GameEngine with Opik callback configured
When start_session() is called
Then an Opik span is recorded with session creation metadata
```

---

### AC-3: generate_round() Creates Round with LLM Phrase and Similar Words

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-005 | Unit | P0 | Choices list contains exactly 5 words | Critical game rule enforcement |
| 001.5-UNIT-006 | Unit | P0 | Correct word is always included in choices | Game integrity |
| 001.5-UNIT-007 | Unit | P1 | Choices are shuffled (statistical verification) | User experience - no predictable patterns |
| 001.5-INT-009 | Integration | P0 | `generate_round()` calls LLM callback and gets phrase + word | Critical path - LLM integration |
| 001.5-INT-010 | Integration | P0 | `generate_round()` finds 4 similar words using EmbeddingSearch | Component coordination |
| 001.5-INT-011 | Integration | P1 | `generate_round()` uses fallback when word not in vocabulary | Error recovery |
| 001.5-INT-012 | Integration | P1 | `generate_round()` adjusts similarity threshold based on difficulty | Difficulty scaling |
| 001.5-INT-013 | Integration | P2 | `generate_round()` records Opik span with round metadata | Observability |
| 001.5-E2E-001 | E2E | P0 | Full round generation flow with real LLM mock | Critical user journey |

**Given-When-Then Templates:**

```gherkin
Given a GameEngine with an active session
And a mock LLM callback that returns {"phrase": "A red fruit", "word": "apple"}
When generate_round() is called
Then the phrase_generator invokes the LLM callback
And EmbeddingSearch finds 4 words similar to "apple"
And a GameRound is returned with 5 shuffled choices including "apple"
And the correct_word field is "apple"

Given an active session and word not in embedding vocabulary
When generate_round() is called with LLM returning unknown word
Then the engine attempts fallback strategy
And returns a valid round OR a descriptive error
```

---

### AC-4: submit_answer() Records Answer and Adjusts Difficulty

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-008 | Unit | P0 | Validate answer is one of the 5 choices | Input validation, security |
| 001.5-UNIT-009 | Unit | P0 | Determine correctness by comparing to correct_word | Core game logic |
| 001.5-UNIT-010 | Unit | P0 | Score calculation formula is correct | Business-critical calculation |
| 001.5-UNIT-011 | Unit | P1 | Rolling accuracy window updates correctly | Difficulty algorithm |
| 001.5-UNIT-012 | Unit | P1 | Difficulty adjustment based on rolling window accuracy | Algorithm correctness |
| 001.5-INT-014 | Integration | P0 | `submit_answer()` updates session stats and persists to DB | Data integrity |
| 001.5-INT-015 | Integration | P0 | `submit_answer()` records answer in answer table | Audit trail |
| 001.5-INT-016 | Integration | P1 | `submit_answer()` updates word knowledge graph | Learning system |
| 001.5-INT-017 | Integration | P1 | `submit_answer()` records confusion when wrong answer | Learning system |
| 001.5-INT-018 | Integration | P2 | `submit_answer()` records Opik span | Observability |
| 001.5-E2E-002 | E2E | P0 | Multiple answers update difficulty progressively | User experience |

**Given-When-Then Templates:**

```gherkin
Given a GameRound with choices ["apple", "orange", "banana", "grape", "pear"]
And correct_word = "apple"
When submit_answer("apple", 1500) is called
Then round.is_correct is true
And session.total_answers increments by 1
And session.correct_answers increments by 1
And the answer is persisted to the database
And difficulty adjustment considers rolling window

Given a GameRound with choices ["apple", "orange", "banana", "grape", "pear"]
When submit_answer("mango", 1500) is called (not in choices)
Then GameError::InvalidChoice("mango") is returned

Given 5 consecutive wrong answers in the rolling window
When submit_answer() processes another wrong answer
Then current_difficulty decreases to make game easier
```

---

### AC-5: submit_to_leaderboard() Calculates Final Score and Updates Best

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-013 | Unit | P0 | Final score calculation formula | Business-critical |
| 001.5-UNIT-014 | Unit | P1 | Score includes accuracy, difficulty, and time components | Formula completeness |
| 001.5-INT-019 | Integration | P0 | `submit_to_leaderboard()` upserts only if better score | Data integrity, competitive fairness |
| 001.5-INT-020 | Integration | P1 | `submit_to_leaderboard()` marks session as submitted | State management |
| 001.5-INT-021 | Integration | P1 | `submit_to_leaderboard()` returns position on leaderboard | User feedback |
| 001.5-INT-022 | Integration | P2 | `submit_to_leaderboard()` records Opik span | Observability |
| 001.5-E2E-003 | E2E | P0 | Full game session submission to leaderboard | Critical user journey |

**Given-When-Then Templates:**

```gherkin
Given a completed session with 10 answers, 8 correct, avg_difficulty 0.7
When submit_to_leaderboard() is called
Then the final score is calculated using the defined formula
And if this score > previous best for username, leaderboard is updated
And session.submitted = true
And the user's leaderboard position is returned

Given a session that was already submitted
When submit_to_leaderboard() is called again
Then GameError::AlreadySubmitted is returned
```

---

### AC-6: get_leaderboard() Returns Top 10 Entries

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-INT-023 | Integration | P1 | `get_leaderboard()` returns top 10 entries ordered by score desc | Core feature |
| 001.5-INT-024 | Integration | P2 | `get_leaderboard()` handles fewer than 10 entries gracefully | Edge case |
| 001.5-UNIT-015 | Unit | P2 | LeaderboardEntry struct has all required fields | Data structure validation |
| 001.5-E2E-004 | E2E | P1 | Leaderboard updates after game submission | User visibility |

**Given-When-Then Templates:**

```gherkin
Given 15 entries in the leaderboard table
When get_leaderboard() is called
Then Vec<LeaderboardEntry> with exactly 10 entries is returned
And entries are sorted by score descending

Given 3 entries in the leaderboard table
When get_leaderboard() is called
Then Vec<LeaderboardEntry> with 3 entries is returned
And no error for fewer than 10
```

---

### AC-7: All Actions Record Opik Spans (if Configured)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-016 | Unit | P1 | Opik callback invocation does not block main flow | Performance, fire-and-forget |
| 001.5-INT-025 | Integration | P1 | All major operations create spans when Opik configured | Feature completeness |
| 001.5-INT-026 | Integration | P2 | No spans created when Opik not configured | Conditional behavior |
| 001.5-E2E-005 | E2E | P2 | Full game flow generates complete trace | Observability validation |

**Given-When-Then Templates:**

```gherkin
Given GameEngine with opik_callback = Some(callback)
When start_session(), generate_round(), submit_answer(), submit_to_leaderboard() are called
Then each operation invokes the Opik callback with appropriate metadata
And the main operation does not wait for callback completion

Given GameEngine with opik_callback = None
When any operation is called
Then no Opik-related code is executed
And operations complete normally
```

---

### AC-8: Error Handling for All Failure Modes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.5-UNIT-017 | Unit | P0 | GameError enum covers all documented error types | Error taxonomy completeness |
| 001.5-UNIT-018 | Unit | P1 | Error messages are descriptive and actionable | User experience |
| 001.5-INT-027 | Integration | P0 | LLM timeout returns appropriate GameError::LlmError | Critical failure mode |
| 001.5-INT-028 | Integration | P1 | Invalid LLM response (malformed JSON) handled gracefully | LLM reliability |
| 001.5-INT-029 | Integration | P1 | DB connection error propagates correctly | Infrastructure failure |
| 001.5-INT-030 | Integration | P1 | Missing embeddings fallback or clear error | Data availability |
| 001.5-E2E-006 | E2E | P1 | Game recovers from transient LLM failures | User experience |
| 001.5-E2E-007 | E2E | P2 | Error state does not corrupt session data | Data integrity |
| 001.5-E2E-008 | E2E | P3 | Concurrent sessions do not interfere | Isolation |

**Given-When-Then Templates:**

```gherkin
Given an LLM callback that takes > 30 seconds to respond
When generate_round() is called with timeout = 10s
Then GameError::LlmError with timeout message is returned
And session state is not corrupted

Given an LLM callback that returns invalid JSON
When generate_round() processes the response
Then GameError::LlmError with parse error is returned
And the operation can be retried

Given a GameEngine with no active session
When submit_answer() is called
Then GameError::NoSession is returned
```

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| LLM unavailability | 001.5-INT-027, 001.5-E2E-006 | Timeout handling, retry logic |
| Invalid LLM responses | 001.5-INT-028 | JSON validation, fallback strategies |
| Score manipulation | 001.5-UNIT-008, 001.5-INT-019 | Input validation, server-side calculation |
| Data corruption | 001.5-INT-014, 001.5-E2E-007 | Transaction integrity, atomic operations |
| Session state leakage | 001.5-E2E-008 | Isolation testing, concurrent execution |
| Embeddings unavailable | 001.5-INT-011, 001.5-INT-030 | Fallback strategy, graceful degradation |

---

## Recommended Execution Order

1. **P0 Unit tests** - Fast validation of core logic (5-10 seconds)
2. **P0 Integration tests** - Component coordination (30-60 seconds with in-memory DB)
3. **P0 E2E tests** - Critical user journeys (2-5 minutes with mock LLM)
4. **P1 tests in order** - Extended coverage
5. **P2+ tests** - As time permits in full regression

### Test Dependencies

```
Unit tests: No dependencies
Integration tests: Requires in-memory DuckDB, mock LLM callback
E2E tests: Requires full engine initialization, mock LLM
```

---

## Test Infrastructure Requirements

### Mocks and Test Doubles

| Component | Mock Strategy |
|-----------|--------------|
| LLM Callback | Async closure returning predefined responses |
| DuckDB | In-memory database (`:memory:`) |
| Opik Callback | Spy pattern to capture calls |
| Random Username | Seeded RNG for deterministic tests |

### Test Data Fixtures

```rust
// Example test fixtures
const TEST_PHRASE_RESPONSE: &str = r#"{"phrase": "A red fruit", "word": "apple"}"#;
const TEST_EMBEDDINGS: &[(&str, [f32; 384])] = &[
    ("apple", [0.1, 0.2, ...]),
    ("orange", [0.15, 0.22, ...]),
    // ... similar words for difficulty testing
];
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention: `001.5-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
- [x] Error handling thoroughly tested

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-GAME-001.5
  story_slug: game-round-orchestration
  date: 2026-01-10
  scenarios_total: 42
  by_level:
    unit: 18
    integration: 16
    e2e: 8
  by_priority:
    p0: 16
    p1: 14
    p2: 8
    p3: 4
  coverage_gaps: []
  key_risks:
    - LLM availability and response quality
    - Score calculation integrity
    - Session state management
  infrastructure_requirements:
    - In-memory DuckDB
    - Mock LLM callback
    - Seeded RNG for deterministic tests
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.5-test-design-20260110.md
P0 tests identified: 16
P1 tests identified: 14
P2+ tests identified: 12
```
