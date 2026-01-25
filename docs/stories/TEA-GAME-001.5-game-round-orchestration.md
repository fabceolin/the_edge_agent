# Story TEA-GAME-001.5: Game Round Orchestration

## Status

Done

## Story

**As a** developer,
**I want** a complete game round pipeline that coordinates all components,
**So that** I can generate questions and process answers end-to-end.

## Story Context

**Existing System Integration:**

- Integrates with: All previous game stories, Opik tracing (TEA-OBS-002)
- Technology: Rust, async/await, DuckDB, LLM callbacks
- Follows pattern: TEA engine orchestration patterns
- Touch points: New `rust/src/games/engine.rs`

**Dependencies:**

- Story 1 (TEA-GAME-001.1): `GameSession`, `GameRound`, scoring
- Story 2 (TEA-GAME-001.2): `GameDb` persistence
- Story 3 (TEA-GAME-001.3): `EmbeddingSearch` similarity
- Story 4 (TEA-GAME-001.4): `PhraseGenerator` LLM phrases

## Acceptance Criteria

1. **AC-1**: `GameEngine` struct orchestrates session, DB, LLM phrase generator, and embeddings
2. **AC-2**: `start_session()` creates session with random username, initializes phrase generator
3. **AC-3**: `generate_round()` calls LLM for phrase, finds 4 similar words, shuffles 5 choices
4. **AC-4**: `submit_answer(choice, time_ms)` records answer, updates stats, adjusts difficulty
5. **AC-5**: `submit_to_leaderboard()` calculates final score and updates if best
6. **AC-6**: `get_leaderboard()` returns top 10 entries
7. **AC-7**: All actions record Opik spans via callback (if configured)
8. **AC-8**: Error handling for all failure modes (LLM timeout, invalid response, DB error)

## Tasks / Subtasks

- [x] Create `rust/src/games/engine.rs` module (AC-1)
  - [x] Add `mod engine;` to `rust/src/games/mod.rs`
  - [x] Define `GameEngine` struct with all dependencies
  - [x] Define `GameEngineConfig` for configuration
  - [x] Implement `new(config)` constructor

- [x] Implement `start_session()` (AC-2)
  - [x] Generate random username
  - [x] Create `GameSession` with default values
  - [x] Initialize `PhraseGenerator`
  - [x] Insert session into database
  - [x] Record Opik span if enabled
  - [x] Return session info

- [x] Implement `generate_round()` (AC-3)
  - [x] Call phrase generator for new phrase + word
  - [x] Get embedding for correct word
  - [x] Find 4 similar words using difficulty threshold
  - [x] Handle fallback if word not in vocabulary
  - [x] Shuffle all 5 words
  - [x] Create `GameRound` struct
  - [x] Record Opik span if enabled
  - [x] Return round to caller

- [x] Implement `submit_answer()` (AC-4)
  - [x] Validate answer is one of the choices
  - [x] Determine correctness
  - [x] Update session stats (total, correct, sum_difficulty)
  - [x] Record answer in database
  - [x] Update word knowledge graph
  - [x] Update confusion graph if wrong
  - [x] Adjust difficulty based on rolling window
  - [x] Record Opik span if enabled
  - [x] Return result with correct answer revealed

- [x] Implement `submit_to_leaderboard()` (AC-5)
  - [x] Calculate final score using formula
  - [x] Submit to database (UPSERT best only)
  - [x] Mark session as submitted
  - [x] Record Opik span if enabled
  - [x] Return success/failure and position

- [x] Implement `get_leaderboard()` (AC-6)
  - [x] Query top N from database
  - [x] Return `Vec<LeaderboardEntry>`

- [x] Integrate Opik tracing (AC-7)
  - [x] Accept optional Opik callback in config
  - [x] Create spans for each operation
  - [x] Include relevant metadata in spans
  - [x] Fire-and-forget (don't block on tracing)

- [x] Implement error handling (AC-8)
  - [x] Define `GameError` enum with all error types
  - [x] Handle LLM timeout gracefully
  - [x] Handle invalid LLM response (retry or error)
  - [x] Handle DB errors
  - [x] Handle missing embeddings

- [x] Write integration tests
  - [x] Test full game flow from start to leaderboard
  - [x] Test error recovery scenarios
  - [x] Test concurrent sessions (multiple sessions test)

## Dev Notes

### GameEngine Structure

```rust
pub struct GameEngineConfig {
    pub db_path: String,
    pub max_history_rounds: usize,
    pub difficulty_window_size: usize,
    pub opik_callback: Option<OpikCallback>,
}

pub struct GameEngine {
    db: GameDb,
    embeddings: EmbeddingSearch,
    phrase_generator: PhraseGenerator,
    session: Option<GameSession>,
    recent_answers: VecDeque<bool>,  // For rolling accuracy
    config: GameEngineConfig,
}

impl GameEngine {
    pub fn new(config: GameEngineConfig) -> Result<Self> {
        let db = GameDb::new(&config.db_path)?;
        let embeddings = EmbeddingSearch::new(&db)?;
        let phrase_generator = PhraseGenerator::new(config.max_history_rounds);

        Ok(Self {
            db,
            embeddings,
            phrase_generator,
            session: None,
            recent_answers: VecDeque::with_capacity(config.difficulty_window_size),
            config,
        })
    }
}
```

### Round Generation Flow

```rust
pub async fn generate_round<F>(&mut self, llm_callback: F) -> Result<GameRound>
where
    F: Fn(&[Message]) -> Future<Output = Result<String>>,
{
    let session = self.session.as_ref().ok_or(GameError::NoSession)?;

    // 1. Generate phrase from LLM
    let phrase_result = self.phrase_generator.generate_phrase(llm_callback).await?;

    // 2. Find similar words based on difficulty
    let difficulty = session.current_difficulty;
    let similar_words = self.embeddings
        .find_similar_words_with_fallback(&phrase_result.word, 4, difficulty)?;

    // 3. Combine and shuffle
    let mut choices: Vec<String> = similar_words;
    choices.push(phrase_result.word.clone());
    choices.shuffle(&mut rand::thread_rng());

    // 4. Create round
    let round = GameRound {
        id: Uuid::new_v4().to_string(),
        phrase: phrase_result.phrase,
        choices,
        correct_word: phrase_result.word,
        selected_word: None,
        is_correct: None,
        response_time_ms: None,
    };

    // 5. Trace if enabled
    if let Some(ref opik) = self.config.opik_callback {
        opik.trace_round_generated(&round, difficulty);
    }

    Ok(round)
}
```

### Answer Submission Flow

```rust
pub fn submit_answer(
    &mut self,
    round: &mut GameRound,
    selected_word: String,
    response_time_ms: u32,
) -> Result<AnswerResult> {
    let session = self.session.as_mut().ok_or(GameError::NoSession)?;

    // Validate choice
    if !round.choices.contains(&selected_word) {
        return Err(GameError::InvalidChoice(selected_word));
    }

    // Determine correctness
    let is_correct = selected_word == round.correct_word;

    // Update round
    round.selected_word = Some(selected_word.clone());
    round.is_correct = Some(is_correct);
    round.response_time_ms = Some(response_time_ms);

    // Update session stats
    session.total_answers += 1;
    if is_correct {
        session.correct_answers += 1;
    }
    session.sum_difficulty += session.current_difficulty;

    // Record to DB
    self.db.record_answer(round)?;
    self.db.update_session(session)?;

    // Update knowledge graph
    self.db.update_word_knowledge(session.id, &round.correct_word, is_correct)?;
    if !is_correct {
        self.db.record_confusion(session.id, &round.correct_word, &selected_word)?;
    }

    // Adjust difficulty
    self.recent_answers.push_back(is_correct);
    if self.recent_answers.len() > self.config.difficulty_window_size {
        self.recent_answers.pop_front();
    }
    self.adjust_difficulty(session);

    // Trace if enabled
    if let Some(ref opik) = self.config.opik_callback {
        opik.trace_answer_submitted(round, session.current_difficulty);
    }

    Ok(AnswerResult {
        is_correct,
        correct_word: round.correct_word.clone(),
        current_score: calculate_score(session),
        current_difficulty: session.current_difficulty,
    })
}
```

### Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum GameError {
    #[error("No active session")]
    NoSession,

    #[error("Invalid choice: {0}")]
    InvalidChoice(String),

    #[error("LLM error: {0}")]
    LlmError(#[from] PhraseError),

    #[error("Database error: {0}")]
    DbError(#[from] duckdb::Error),

    #[error("Embedding not found for word: {0}")]
    EmbeddingNotFound(String),

    #[error("Session already submitted")]
    AlreadySubmitted,
}
```

### Relevant Source Tree

```
rust/
├── src/
│   └── games/
│       ├── mod.rs               # Add: mod engine; pub use
│       ├── mod.rs               # GameSession, GameRound
│       ├── db.rs                # GameDb
│       ├── embeddings.rs        # EmbeddingSearch
│       ├── phrase_generator.rs  # PhraseGenerator
│       └── engine.rs            # NEW: GameEngine
```

### Testing

- Test file location: `rust/src/games/engine.rs` (inline) or `rust/tests/test_game_engine.rs`
- Use in-memory DuckDB and mock LLM
- Test framework: Built-in Rust + tokio
- Run with: `cargo test --features game-duckdb`

## Definition of Done

- [x] `GameEngine` coordinates all components
- [x] Full game flow works from start to leaderboard
- [x] All error conditions handled gracefully
- [x] Opik tracing integrates correctly (when enabled)
- [x] Integration tests pass
- [x] No panics or crashes on edge cases

## File List

| File | Action | Description |
|------|--------|-------------|
| `rust/src/games/engine.rs` | Created | GameEngine implementation with orchestration, error handling, and tests (~1100 lines) |
| `rust/src/games/mod.rs` | Modified | Added `pub mod engine;` and re-exports for engine types |

---

## QA Notes

**Test Design Assessment:** 2026-01-10 | **Designer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 18 (43%) |
| **Integration tests** | 16 (38%) |
| **E2E tests** | 8 (19%) |
| **Priority distribution** | P0: 16, P1: 14, P2: 8, P3: 4 |

**Coverage by Acceptance Criteria:**

| AC | Unit | Integration | E2E | Total |
|----|------|-------------|-----|-------|
| AC-1 (GameEngine orchestration) | 2 | 3 | - | 5 |
| AC-2 (start_session) | 2 | 5 | - | 7 |
| AC-3 (generate_round) | 3 | 5 | 1 | 9 |
| AC-4 (submit_answer) | 5 | 5 | 1 | 11 |
| AC-5 (leaderboard submit) | 2 | 4 | 1 | 7 |
| AC-6 (get_leaderboard) | 1 | 2 | 1 | 4 |
| AC-7 (Opik tracing) | 1 | 2 | 1 | 4 |
| AC-8 (Error handling) | 2 | 4 | 3 | 9 |

### Risk Areas Identified

| Risk | Severity | Probability | Test Coverage |
|------|----------|-------------|---------------|
| **LLM unavailability/timeout** | High | Medium | INT-027, E2E-006 |
| **Invalid LLM responses** | Medium | Medium | INT-028 |
| **Score manipulation attempts** | High | Low | UNIT-008, INT-019 |
| **Data corruption on error** | High | Low | INT-014, E2E-007 |
| **Session state leakage** | Medium | Low | E2E-008 |
| **Embeddings not found** | Medium | Medium | INT-011, INT-030 |

**Key Risk Mitigations:**
- LLM failures: Timeout handling with graceful degradation
- Score integrity: Server-side calculation, input validation
- Data integrity: Transaction-based persistence, atomic operations
- State management: Session isolation, proper error recovery

### Recommended Test Scenarios

**P0 Critical (Must Pass Before Merge):**
1. `001.5-INT-001` - GameEngine::new() initializes all dependencies
2. `001.5-INT-004` - start_session() creates and persists GameSession
3. `001.5-INT-009` - generate_round() calls LLM and retrieves phrase
4. `001.5-INT-010` - generate_round() finds similar words via EmbeddingSearch
5. `001.5-UNIT-005` - Choices list contains exactly 5 words
6. `001.5-UNIT-006` - Correct word always included in choices
7. `001.5-UNIT-008` - Answer validation against allowed choices
8. `001.5-INT-014` - submit_answer() updates session stats and persists
9. `001.5-INT-019` - submit_to_leaderboard() upserts only if better score
10. `001.5-INT-027` - LLM timeout returns GameError::LlmError
11. `001.5-E2E-001` - Full round generation flow
12. `001.5-E2E-002` - Multiple answers update difficulty progressively
13. `001.5-E2E-003` - Full game session submission to leaderboard

**P1 Important (Should Pass):**
- Difficulty adjustment algorithm correctness
- Rolling window accuracy updates
- Error recovery and retry logic
- Opik span recording when configured

### Concerns and Blockers

**Concerns:**
1. **Dependency chain risk**: This story depends on Stories 1-4. Integration testing requires all dependencies to be stable.
2. **LLM response variability**: Mock LLM must simulate realistic edge cases (malformed JSON, timeouts, unexpected fields).
3. **Concurrent session isolation**: E2E-008 tests isolation but concurrent stress testing may reveal race conditions.
4. **Difficulty algorithm tuning**: Rolling window size and adjustment thresholds may need tuning post-implementation.

**Blockers:**
- None identified. All dependencies (Stories 1-4) appear well-defined.

**Test Infrastructure Requirements:**
- In-memory DuckDB (`:memory:`) for integration tests
- Mock LLM callback with configurable responses/delays
- Seeded RNG for deterministic username generation
- Spy pattern for Opik callback verification

**Estimated Test Execution Time:**
- P0 Unit: ~5-10 seconds
- P0 Integration: ~30-60 seconds
- P0 E2E: ~2-5 minutes
- Full suite: ~10-15 minutes

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Notes from test design assessment | Quinn (QA Agent) |
| 2026-01-23 | 1.0 | Implementation complete: GameEngine with all AC satisfied, 26 tests passing | James (Dev Agent) |

---

## QA Results

### Review Date: 2026-01-23

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall Grade: EXCELLENT**

The GameEngine implementation demonstrates high-quality Rust code with excellent architecture, comprehensive error handling, and thorough test coverage. The implementation correctly orchestrates all dependencies (GameDb, EmbeddingSearch, PhraseGenerator, Opik) in a clean, well-documented module.

**Strengths:**
1. **Clean Architecture**: GameEngine properly encapsulates all dependencies with clear separation of concerns
2. **Comprehensive Error Types**: `GameError` enum covers all failure modes (NoSession, AlreadySubmitted, InvalidChoice, NoActiveRound, LlmError, DbError, EmbeddingNotFound, ConfigError, InsufficientDistractors)
3. **Strong Type Safety**: Proper use of Rust's type system with clear Result types for all operations
4. **Excellent Documentation**: Module-level docs with examples, function-level docs explaining behavior
5. **Defensive Coding**: Validates inputs, checks state before operations, handles edge cases
6. **Test Coverage**: 31 passing tests covering all acceptance criteria

### Refactoring Performed

No refactoring was needed. The code follows Rust best practices and idiomatic patterns.

### Compliance Check

- Coding Standards: [✓] Follows Rust conventions with proper use of `thiserror`, `serde`, documentation
- Project Structure: [✓] Module structure aligns with `rust/src/games/` organization
- Testing Strategy: [✓] Unit tests inline in engine.rs, integration patterns for full flow tests
- All ACs Met: [✓] All 8 acceptance criteria verified and tested

### Requirements Traceability

| AC | Description | Implementation | Test Coverage |
|----|-------------|----------------|---------------|
| AC-1 | GameEngine orchestrates components | `GameEngine::new()` at engine.rs:254 | `test_game_engine_creation`, `test_game_engine_initial_state` |
| AC-2 | start_session() creates session | `start_session()` at engine.rs:345 | `test_start_session`, `test_start_session_creates_session`, `test_start_session_persists_to_db`, `test_start_session_generates_random_username` |
| AC-3 | generate_round() calls LLM, finds similar words | `generate_round()` at engine.rs:410 | `test_generate_round_returns_round`, `test_generate_round_stores_current_round`, `test_generate_round_shuffles_choices`, `test_generate_round_requires_session` |
| AC-4 | submit_answer() records, updates stats, adjusts difficulty | `submit_answer()` at engine.rs:524 | `test_submit_correct_answer`, `test_submit_incorrect_answer`, `test_submit_answer_clears_current_round`, `test_submit_answer_updates_session_stats`, `test_submit_answer_adjusts_difficulty`, `test_submit_answer_validates_choice`, `test_submit_answer_requires_active_round` |
| AC-5 | submit_to_leaderboard() calculates score, UPSERTs | `submit_to_leaderboard()` at engine.rs:633 | `test_submit_to_leaderboard_calculates_score`, `test_submit_to_leaderboard_prevents_double_submission`, `test_submit_to_leaderboard_with_no_answers` |
| AC-6 | get_leaderboard() returns top 10 | `get_leaderboard()` at engine.rs:720 | `test_get_leaderboard_empty`, `test_get_leaderboard_returns_entries`, `test_get_leaderboard_respects_limit` |
| AC-7 | Opik spans recorded when enabled | Throughout all operations with `is_game_opik_enabled()` checks | Code paths verified in `test_full_game_flow` |
| AC-8 | Error handling for all failure modes | `GameError` enum at engine.rs:63 | `test_llm_error_propagates`, `test_json_parse_error`, `test_game_error_display` |

### Improvements Checklist

[x] All acceptance criteria are fully implemented
[x] All 31 engine tests pass
[x] Error handling covers all specified failure modes
[x] Opik integration follows fire-and-forget pattern (non-blocking)
[x] Documentation is comprehensive with examples
[x] Code is well-structured with clear module boundaries

**Recommendations (Future Enhancement):**
- [ ] Consider adding integration tests with VSS extension enabled (currently skipped when VSS unavailable)
- [ ] Consider adding stress tests for concurrent session isolation (E2E-008 from test design)
- [ ] Consider parameterized tests for difficulty adjustment edge cases

### Security Review

**Status: PASS**

- No SQL injection vulnerabilities - uses parameterized queries via DuckDB's params![] macro
- Input validation prevents invalid choices in `submit_answer()`
- No unsafe code blocks used
- Session state properly isolated
- Opik callbacks are fire-and-forget, cannot leak sensitive data

### Performance Considerations

**Status: PASS**

- In-memory DuckDB option for fast testing (`":memory:"`)
- Rolling window for difficulty adjustment is bounded by `difficulty_window_size`
- Opik tracing is non-blocking (fire-and-forget)
- Embedding search uses fallback strategy with progressive range widening

### Test Coverage Analysis

| Test Category | Count | Status |
|---------------|-------|--------|
| AC-1 (GameEngine orchestration) | 4 | PASS |
| AC-2 (start_session) | 4 | PASS |
| AC-3 (generate_round) | 4 | PASS |
| AC-4 (submit_answer) | 7 | PASS |
| AC-5 (leaderboard submit) | 3 | PASS |
| AC-6 (get_leaderboard) | 3 | PASS |
| AC-7 (Opik tracing) | Integrated | PASS |
| AC-8 (Error handling) | 3 | PASS |
| Integration (Full flow) | 2 | PASS |
| Config/Display | 2 | PASS |
| **TOTAL** | **31** | **ALL PASS** |

### Files Modified During Review

None. The implementation met all quality standards.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-GAME-001.5-game-round-orchestration.yml
Risk profile: Low - well-tested, isolated module with comprehensive error handling
NFR assessment: Security PASS, Performance PASS, Reliability PASS, Maintainability PASS

### Recommended Status

**[✓ Ready for Done]**

The implementation is complete, well-tested (31/31 tests passing), and follows all best practices. All acceptance criteria are met with full traceability. The code quality is excellent with no issues requiring attention before merge.
