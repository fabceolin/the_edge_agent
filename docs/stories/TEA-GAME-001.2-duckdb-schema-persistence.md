# Story TEA-GAME-001.2: DuckDB Schema and Persistence Layer

## Status

Ready for Development

**QA Validation:** Passed (2026-01-10)
- All 9 acceptance criteria have test coverage
- 31 test scenarios defined (11 P0, 13 P1, 5 P2, 2 P3)
- No blockers identified
- Risk mitigations documented

## Story

**As a** developer,
**I want** DuckDB tables for game state, answers, words, and leaderboard,
**So that** game data persists and supports vector search and graph queries.

## Story Context

**Existing System Integration:**

- Integrates with: DuckDB Rust crate (already in Cargo.toml as optional)
- Technology: DuckDB with VSS and DuckPGQ extensions
- Follows pattern: Python DuckDB implementations in `python/src/the_edge_agent/memory/`
- Touch points: New `rust/src/games/db.rs` module

**Dependencies:**

- Story 1 (TEA-GAME-001.1): `GameSession` and `GameRound` structs

## Acceptance Criteria

1. **AC-1**: `words` table with `id`, `text`, `embedding FLOAT[384]`, `frequency`
2. **AC-2**: `game_sessions` table with session metadata and rolling stats
3. **AC-3**: `answers` table with full round details including JSON `choices` array and `phrase` text
4. **AC-4**: `leaderboard` table with UPSERT logic (best score only per username)
5. **AC-5**: `user_word_knowledge` graph edge table for learning analytics
6. **AC-6**: `user_confusions` graph edge table tracking wrong guesses
7. **AC-7**: VSS extension loaded and `array_cosine_similarity` functional
8. **AC-8**: DuckPGQ extension loaded for graph queries
9. **AC-9**: Integration tests for CRUD operations

## Tasks / Subtasks

- [ ] Create `rust/src/games/db.rs` module (AC-1 through AC-6)
  - [ ] Add `mod db;` to `rust/src/games/mod.rs`
  - [ ] Create `GameDb` struct wrapping DuckDB connection
  - [ ] Implement `new(path: &str)` constructor (":memory:" for tests)
  - [ ] Implement `init_schema()` to create all tables

- [ ] Implement schema DDL (AC-1 through AC-6)
  - [ ] `words` table with vector column
  - [ ] `game_sessions` table
  - [ ] `answers` table with JSON choices
  - [ ] `leaderboard` table
  - [ ] `user_word_knowledge` graph edge table
  - [ ] `user_confusions` graph edge table

- [ ] Load DuckDB extensions (AC-7, AC-8)
  - [ ] Install and load VSS extension
  - [ ] Install and load DuckPGQ extension
  - [ ] Handle extension load failures gracefully

- [ ] Implement session CRUD operations (AC-2)
  - [ ] `insert_session(session: &GameSession) -> Result<()>`
  - [ ] `update_session(session: &GameSession) -> Result<()>`
  - [ ] `get_session(id: &str) -> Result<Option<GameSession>>`

- [ ] Implement answer recording (AC-3, AC-5, AC-6)
  - [ ] `record_answer(round: &GameRound) -> Result<()>`
  - [ ] Update `user_word_knowledge` on answer
  - [ ] Update `user_confusions` on wrong answer

- [ ] Implement leaderboard operations (AC-4)
  - [ ] `submit_to_leaderboard(username, score, accuracy, total, avg_diff) -> Result<bool>`
  - [ ] UPSERT only if new score > existing score
  - [ ] `get_top_leaderboard(limit: usize) -> Result<Vec<LeaderboardEntry>>`

- [ ] Add feature flag and tests (AC-9)
  - [ ] Add `game-duckdb` feature to Cargo.toml
  - [ ] Write integration tests with in-memory DuckDB
  - [ ] Test CRUD operations
  - [ ] Test leaderboard UPSERT logic

## Dev Notes

### Schema DDL

```sql
-- Word embeddings for similarity search
CREATE TABLE IF NOT EXISTS words (
    id VARCHAR PRIMARY KEY,
    text VARCHAR NOT NULL UNIQUE,
    embedding FLOAT[384],
    frequency INTEGER DEFAULT 0
);

-- Game sessions
CREATE TABLE IF NOT EXISTS game_sessions (
    id VARCHAR PRIMARY KEY,
    username VARCHAR NOT NULL,
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    total_answers INTEGER DEFAULT 0,
    correct_answers INTEGER DEFAULT 0,
    sum_difficulty DOUBLE DEFAULT 0.0,
    current_difficulty DOUBLE DEFAULT 0.5,
    is_submitted BOOLEAN DEFAULT FALSE
);

-- Individual answers
CREATE TABLE IF NOT EXISTS answers (
    id VARCHAR PRIMARY KEY,
    session_id VARCHAR,
    phrase VARCHAR NOT NULL,
    choices JSON,
    correct_word VARCHAR,
    selected_word VARCHAR,
    is_correct BOOLEAN,
    response_time_ms INTEGER,
    difficulty DOUBLE,
    answered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Leaderboard (best score per username)
CREATE TABLE IF NOT EXISTS leaderboard (
    username VARCHAR PRIMARY KEY,
    score DOUBLE NOT NULL,
    accuracy DOUBLE NOT NULL,
    total_answers INTEGER NOT NULL,
    avg_difficulty DOUBLE NOT NULL,
    submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Graph: User word knowledge
CREATE TABLE IF NOT EXISTS user_word_knowledge (
    session_id VARCHAR,
    word_id VARCHAR,
    times_seen INTEGER DEFAULT 0,
    times_correct INTEGER DEFAULT 0,
    times_as_llm INTEGER DEFAULT 0,
    times_guessed INTEGER DEFAULT 0,
    last_seen TIMESTAMP,
    PRIMARY KEY (session_id, word_id)
);

-- Graph: User confusion patterns
CREATE TABLE IF NOT EXISTS user_confusions (
    session_id VARCHAR,
    correct_word_id VARCHAR,
    confused_word_id VARCHAR,
    times INTEGER DEFAULT 1,
    PRIMARY KEY (session_id, correct_word_id, confused_word_id)
);
```

### Leaderboard UPSERT Logic

```sql
INSERT INTO leaderboard (username, score, accuracy, total_answers, avg_difficulty)
VALUES (?, ?, ?, ?, ?)
ON CONFLICT (username) DO UPDATE SET
    score = EXCLUDED.score,
    accuracy = EXCLUDED.accuracy,
    total_answers = EXCLUDED.total_answers,
    avg_difficulty = EXCLUDED.avg_difficulty,
    submitted_at = CURRENT_TIMESTAMP
WHERE EXCLUDED.score > leaderboard.score;
```

### Extension Loading

```rust
fn load_extensions(conn: &Connection) -> Result<()> {
    // VSS for vector similarity
    conn.execute("INSTALL vss; LOAD vss;", [])?;

    // DuckPGQ for graph queries
    conn.execute("INSTALL duckpgq FROM community; LOAD duckpgq;", [])?;

    Ok(())
}
```

### Relevant Source Tree

```
rust/
├── src/
│   └── games/
│       ├── mod.rs       # Add: mod db;
│       └── db.rs        # NEW: GameDb, schema, CRUD
├── Cargo.toml           # Add: duckdb feature
```

### Testing

- Test file location: `rust/src/games/db.rs` (inline) or `rust/tests/test_games_db.rs`
- Use in-memory DuckDB: `GameDb::new(":memory:")`
- Test framework: Built-in Rust + duckdb crate
- Run with: `cargo test --features game-duckdb`

## Definition of Done

- [ ] All acceptance criteria met
- [ ] Schema creates successfully
- [ ] Extensions load without error
- [ ] CRUD operations work correctly
- [ ] Leaderboard UPSERT logic verified
- [ ] Integration tests pass

---

## QA Results

**Reviewed:** 2026-01-10
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 31 |
| Unit Tests | 10 (32%) |
| Integration Tests | 18 (58%) |
| E2E Tests | 3 (10%) |
| P0 (Critical) | 11 |
| P1 (High) | 13 |
| P2 (Medium) | 5 |
| P3 (Low) | 2 |

All 9 Acceptance Criteria have test coverage with no gaps identified.

### Risk Areas Identified

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Data loss on session crash | High | Medium | Integration tests for persistence durability |
| Leaderboard corruption via concurrent UPSERT | High | Low | P0 tests for UPSERT atomicity |
| VSS/DuckPGQ extension load failure | High | Medium | Integration tests for graceful degradation |
| Vector search inaccuracy | Medium | Low | Unit tests for cosine similarity edge cases |
| Graph query performance degradation | Medium | Medium | Integration tests with realistic data volumes |

### Recommended Test Scenarios (Priority Execution)

**Phase 1 - Fail-Fast (P0):**
1. GameSession struct validation (UNIT-002)
2. UPSERT SQL logic correctness (UNIT-005)
3. VSS extension loads successfully (INT-020)
4. Word with 384-dim embedding insert/retrieve (INT-001)
5. Session creation with defaults (INT-004)
6. Session stats atomic update (INT-005)
7. Answer recording with JSON choices (INT-008)
8. Leaderboard UPSERT paths: insert, update-higher, no-update-lower (INT-011 through INT-013)

**Phase 2 - Core Functionality (P1):**
- All P1 unit and integration tests
- Game-to-leaderboard E2E journey (E2E-001, E2E-002)

### Concerns

1. **Extension Availability:** DuckDB extensions (VSS, DuckPGQ) may not be available in all environments. Recommend feature flag `game-duckdb` with graceful fallback.

2. **Vector Dimension Validation:** Schema uses `FLOAT[384]` but no validation exists to reject embeddings of wrong dimension. Consider runtime check.

3. **JSON Choices Edge Cases:** Empty/null choices arrays should be tested (INT-010 covers this at P2).

4. **Graph Table Orphans:** No foreign key constraints defined. Session deletion could leave orphan records in `user_word_knowledge` and `user_confusions`.

### Blockers

None identified. Story is ready for implementation.

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-GAME-001.2-test-design-20260110.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Results section | Quinn (Test Architect) |
