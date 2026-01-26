# Epic TEA-GAME-001: Know Your Model - Interactive LLM Guessing Game

## Status

Ready for Development

**Status Updated:** 2026-01-12
**Updated By:** Bob (SM Agent)
**Validation:** All QA checklist criteria passed - test design complete with 87 scenarios covering 100% of stories and ACs

## Epic Goal

Create an interactive browser-based game called "Know Your Model" that teaches users to distinguish between LLM-generated text and semantically similar alternatives, with adaptive difficulty, DuckDB-backed persistence, Opik observability tracing, and a competitive leaderboard system.

## Epic Description

### Existing System Integration

- **Integrates with:** TEA WASM LLM demo (`docs/extra/wasm-demo/`), TEA-OBS-002 (Opik integration)
- **Technology:** Rust (native debugging) -> WASM (browser deployment), DuckDB (VSS + PGQ extensions)
- **Follows pattern:** YAML agent interview mode from `docs/articles/intelligent-interview-bot.md`
- **Touch points:**
  - `rust/tea-wasm-llm/src/lib.rs` - WASM LLM execution
  - `docs/extra/wasm-demo/app.js` - Demo application
  - `docs/extra/wasm-demo/index.html` - UI with tabs
  - DuckDB Rust crate for persistence

### Game Concept

Users are presented with an incomplete phrase and 5 word choices:
- **1 word** is the actual LLM completion (the correct answer)
- **4 words** are semantically similar alternatives (from embedding vector search)

The user must identify which word came from the LLM. Difficulty adapts based on user performance by adjusting the similarity threshold for distractor words.

### Key Features

1. **Dynamic Phrase Generation** - LLM generates both phrase AND completion, using context window to avoid repeats
2. **Embedding-Based Distractors** - DuckDB VSS finds similar words within difficulty-adjusted threshold
3. **Adaptive Difficulty** - Statistical analysis of rolling accuracy adjusts word similarity
4. **Weighted Scoring** - Formula: `(correct/total) * avg_difficulty * log2(total+1)/log2(50)`
5. **Graph Learning** - DuckPGQ tracks user knowledge/confusion patterns
6. **Leaderboard** - Best score per username (random generated)
7. **Opik Tracing** - All game rounds traced for analytics

### Architecture: Context Window for Phrase Memory

Instead of a database-backed phrase repository, the LLM generates phrases dynamically with context window memory:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    CONTEXT WINDOW PHRASE GENERATION                         │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  SYSTEM PROMPT:                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  You are a phrase generator for a word-guessing game.               │   │
│  │  Generate a sentence with ONE blank (marked as ___) and provide     │   │
│  │  the word that fills the blank.                                     │   │
│  │                                                                      │   │
│  │  Rules:                                                              │   │
│  │  1. The blank word must be a common English word                    │   │
│  │  2. The sentence should be natural and grammatically correct        │   │
│  │  3. DO NOT repeat phrases from the conversation history             │   │
│  │  4. Vary topics: nature, science, emotions, daily life, etc.        │   │
│  │                                                                      │   │
│  │  Respond in JSON: {"phrase": "The ___ rises.", "word": "sun"}       │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  CONVERSATION HISTORY (Context Window):                                     │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  Round 1: {"phrase": "The ___ rises early.", "word": "sun"}         │   │
│  │  Round 2: {"phrase": "A ___ flew overhead.", "word": "bird"}        │   │
│  │  Round 3: {"phrase": "She felt ___ inside.", "word": "happy"}       │   │
│  │  ...                                                                 │   │
│  │  (LLM sees history and avoids repetition naturally)                 │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
│  NEW ROUND REQUEST:                                                         │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │  User: "Generate a new phrase for round 4."                         │   │
│  │  LLM: {"phrase": "The ___ was delicious.", "word": "cake"}          │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Stories

### Story 1: Rust Game Engine Core

**As a** developer,
**I want** a Rust game engine with state management, scoring, and difficulty adjustment,
**So that** I can test and debug game logic before WASM compilation.

**Acceptance Criteria:**

1. **AC-1**: `GameSession` struct with `id`, `username`, `total_answers`, `correct_answers`, `current_difficulty`, `sum_difficulty`
2. **AC-2**: `GameRound` struct with `phrase`, `choices` (5 words), `correct_word`, `selected_word`, `is_correct`, `response_time_ms`
3. **AC-3**: `generate_username()` function returns random `{Adjective}{Animal}{Number}` pattern
4. **AC-4**: `calculate_score(session)` implements weighted formula with answer_factor
5. **AC-5**: `adjust_difficulty(session, window_size)` uses rolling accuracy to modify difficulty (+/-0.05)
6. **AC-6**: Difficulty bounded to `[0.1, 0.95]` range
7. **AC-7**: Unit tests for all score/difficulty calculations

**Tasks:**

- [ ] Create `rust/src/games/mod.rs` module
- [ ] Implement `GameSession` and `GameRound` structs
- [ ] Implement `generate_username()` with word lists
- [ ] Implement `calculate_score()` with weighted formula
- [ ] Implement `adjust_difficulty()` with rolling window
- [ ] Write comprehensive unit tests

---

### Story 2: DuckDB Schema and Persistence Layer

**As a** developer,
**I want** DuckDB tables for game state, answers, words, and leaderboard,
**So that** game data persists and supports vector search and graph queries.

**Acceptance Criteria:**

1. **AC-1**: `words` table with `id`, `text`, `embedding FLOAT[384]`, `frequency`
2. **AC-2**: `game_sessions` table with session metadata and rolling stats
3. **AC-3**: `answers` table with full round details including JSON `choices` array and `phrase` text
4. **AC-4**: `leaderboard` table with UPSERT logic (best score only per username)
5. **AC-5**: `user_word_knowledge` graph edge table for learning analytics
6. **AC-6**: `user_confusions` graph edge table tracking wrong guesses
7. **AC-7**: VSS extension loaded and `array_cosine_similarity` functional
8. **AC-8**: DuckPGQ extension loaded for graph queries
9. **AC-9**: Integration tests for CRUD operations

**Tasks:**

- [ ] Create `rust/src/games/db.rs` module
- [ ] Implement schema creation DDL (no phrases table needed)
- [ ] Implement `insert_session()`, `update_session()`, `get_session()`
- [ ] Implement `record_answer()` with knowledge graph updates
- [ ] Implement `submit_to_leaderboard()` with UPSERT logic
- [ ] Implement `get_top_leaderboard(limit)` query
- [ ] Add DuckDB feature flag to Cargo.toml
- [ ] Write integration tests with in-memory DuckDB

---

### Story 3: Word Embedding and Similarity Search

**As a** developer,
**I want** to find semantically similar words based on difficulty threshold,
**So that** the game generates appropriate distractor words.

**Acceptance Criteria:**

1. **AC-1**: Pre-computed word embeddings loaded into DuckDB `words` table (common vocabulary ~10k words)
2. **AC-2**: `find_similar_words(word, n, min_sim, max_sim)` returns words within similarity range
3. **AC-3**: Difficulty maps to similarity range: Easy (0.1-0.4), Medium (0.4-0.7), Hard (0.7-0.95)
4. **AC-4**: Similar words exclude the target word itself
5. **AC-5**: Random selection among qualifying words (not just top-N)
6. **AC-6**: Fallback behavior when insufficient words in range (widen range progressively)
7. **AC-7**: Handle case where LLM-generated word is not in vocabulary (generate embedding on-the-fly or skip round)

**Tasks:**

- [ ] Source word embeddings (all-MiniLM-L6-v2 or similar, 384 dims)
- [ ] Create embedding loader script (Parquet format)
- [ ] Implement `find_similar_words()` using DuckDB VSS
- [ ] Implement difficulty-to-similarity mapping
- [ ] Add fallback logic for edge cases
- [ ] Unit tests for similarity search

---

### Story 4: LLM Phrase Generation with Context Memory

**As a** developer,
**I want** the LLM to generate phrases dynamically using context window memory,
**So that** phrases are varied and never repeat within a session.

**Acceptance Criteria:**

1. **AC-1**: `PhraseGenerator` struct maintains conversation history for context window
2. **AC-2**: System prompt instructs LLM to generate phrase + word in JSON format
3. **AC-3**: Conversation history includes all previously generated phrases (for repetition avoidance)
4. **AC-4**: `generate_phrase()` returns `{phrase: String, word: String}` parsed from LLM response
5. **AC-5**: JSON parsing with error handling for malformed responses
6. **AC-6**: Retry logic (up to 3 attempts) if LLM returns invalid format
7. **AC-7**: Context window pruning strategy if history exceeds token limit (keep last N rounds)
8. **AC-8**: Word extraction handles edge cases (punctuation, casing normalization)

**Tasks:**

- [ ] Create `rust/src/games/phrase_generator.rs` module
- [ ] Implement `PhraseGenerator` with conversation history
- [ ] Design system prompt for phrase generation
- [ ] Implement JSON response parsing with `serde_json`
- [ ] Implement retry logic for invalid responses
- [ ] Implement context window management (pruning old rounds)
- [ ] Unit tests with mock LLM responses
- [ ] Integration tests with real LLM callback

---

### Story 5: Game Round Orchestration

**As a** developer,
**I want** a complete game round pipeline that coordinates all components,
**So that** I can generate questions and process answers end-to-end.

**Acceptance Criteria:**

1. **AC-1**: `GameEngine` struct orchestrates session, DB, LLM phrase generator, and embeddings
2. **AC-2**: `start_session()` creates session with random username, initializes phrase generator
3. **AC-3**: `generate_round()` calls LLM for phrase, finds 4 similar words, shuffles 5 choices
4. **AC-4**: `submit_answer(choice, time_ms)` records answer, updates stats, adjusts difficulty
5. **AC-5**: `submit_to_leaderboard()` calculates final score and updates if best
6. **AC-6**: `get_leaderboard()` returns top 10 entries
7. **AC-7**: All actions record Opik spans via callback
8. **AC-8**: Error handling for all failure modes (LLM timeout, invalid response, DB error)

**Tasks:**

- [ ] Create `GameEngine` struct with dependencies
- [ ] Implement `start_session()` flow with phrase generator init
- [ ] Implement `generate_round()` pipeline (LLM -> embeddings -> shuffle)
- [ ] Implement `submit_answer()` with all side effects
- [ ] Implement `submit_to_leaderboard()` with score calculation
- [ ] Wire Opik tracing hooks (TEA-OBS-002 integration)
- [ ] End-to-end integration tests

---

### Story 6: WASM Port and JavaScript Bridge

**As a** developer,
**I want** the game engine compiled to WASM with JavaScript bindings,
**So that** it can run in the browser alongside the existing demo.

**Acceptance Criteria:**

1. **AC-1**: `wasm-bindgen` exports for `start_session`, `generate_round`, `submit_answer`, `submit_to_leaderboard`, `get_leaderboard`
2. **AC-2**: IndexedDB or OPFS storage for DuckDB persistence in browser
3. **AC-3**: LLM callback bridge (reuse existing `tea-wasm-llm` pattern)
4. **AC-4**: Opik callback bridge from TEA-OBS-002
5. **AC-5**: TypeScript type definitions for all exports
6. **AC-6**: Error handling returns structured JSON errors
7. **AC-7**: Playwright tests for WASM functions

**Tasks:**

- [ ] Create `rust/tea-wasm-game/` crate (or add to `tea-wasm-llm`)
- [ ] Add `wasm-bindgen` exports for game API
- [ ] Implement browser storage adapter for DuckDB
- [ ] Wire LLM callback through existing bridge
- [ ] Wire Opik callback through TEA-OBS-002 bridge
- [ ] Generate TypeScript declarations
- [ ] Playwright browser tests

---

### Story 7: Browser UI Implementation

**As a** user,
**I want** a game tab in the WASM demo with interactive UI,
**So that** I can play "Know Your Model" in my browser.

**Acceptance Criteria:**

1. **AC-1**: New "Game" tab in `index.html` alongside YAML workflow tab
2. **AC-2**: Welcome screen with random username display and "Start Game" button
3. **AC-3**: Game screen shows phrase with blank, 5 word buttons, current stats
4. **AC-4**: Visual feedback on correct/incorrect answer (green/red flash)
5. **AC-5**: Difficulty indicator (progress bar or gauge)
6. **AC-6**: "Give Up & Submit" button always visible
7. **AC-7**: Leaderboard screen shows top 10 with user's position highlighted
8. **AC-8**: "Play Again" button from leaderboard screen
9. **AC-9**: Loading states during LLM calls
10. **AC-10**: Responsive design matching existing demo style

**Tasks:**

- [ ] Add tab navigation to `index.html`
- [ ] Create welcome screen HTML/CSS
- [ ] Create game screen HTML/CSS
- [ ] Create leaderboard screen HTML/CSS
- [ ] Implement `game.js` module with state machine
- [ ] Wire all button handlers to WASM API
- [ ] Add loading spinners during async operations
- [ ] Test on mobile viewport

---

### Story 8: Opik Integration and Analytics

**As a** developer,
**I want** all game actions traced to Opik,
**So that** I can analyze user behavior and model performance.

**Acceptance Criteria:**

1. **AC-1**: Each game round creates an Opik trace with: `phrase`, `choices`, `correct_word`, `selected_word`, `is_correct`, `response_time_ms`, `difficulty`
2. **AC-2**: Session-level trace groups all rounds
3. **AC-3**: LLM phrase generation calls traced with token usage
4. **AC-4**: Leaderboard submissions traced
5. **AC-5**: Uses TEA-OBS-002 `enableOpikTracing()` API
6. **AC-6**: Graceful degradation if Opik not configured
7. **AC-7**: Integration test with mock Opik handler

**Tasks:**

- [ ] Define Opik span schema for game events
- [ ] Add trace calls in `GameEngine` methods
- [ ] Integrate with TEA-OBS-002 `set_opik_handler()`
- [ ] Add project_name configuration in YAML/settings
- [ ] Test tracing in both Rust and WASM

---

### Story 9: Phrase Database Integration (Brownfield)

**As a** game developer,
**I want** the game engine to load and use pre-created phrases from `data/game_phrases.json`,
**So that** players can play with curated phrases and the LLM only needs to complete (not generate) phrases.

**Acceptance Criteria:**

1. **AC-1**: `phrases` table created in DuckDB with columns: `id`, `phrase`, `correct_word`, `distractors` (JSON), `difficulty`, `category`
2. **AC-2**: Phrases loaded from embedded JSON (compile-time include) on startup
3. **AC-3**: `get_random_phrase(min_difficulty, max_difficulty, exclude_ids)` returns phrase matching difficulty range
4. **AC-4**: Session tracks `used_phrase_ids` to avoid repeats within a game
5. **AC-5**: `game_generate_round()` uses phrase database instead of LLM phrase generation
6. **AC-6**: LLM is called only for simple completion: "Complete with ONE word: {phrase}"

**Tasks:**

- [ ] Add `phrases` table DDL to `db.rs`
- [ ] Define `Phrase` and `PhrasesFile` structs
- [ ] Implement `include_str!` for WASM build
- [ ] Load phrases into DuckDB on initialization
- [ ] Implement `get_random_phrase(min, max, exclude)`
- [ ] Add `used_phrase_ids` to `GameSession`
- [ ] Modify `game_generate_round()` to use phrase database
- [ ] Verify `game.js` UI still works
- [ ] Tests for phrase loading and selection

**File:** `docs/stories/TEA-GAME-001.9-phrase-database-integration.md`

---

## Technical Design Notes

### Score Formula

```
score = (correct / total) * avg_difficulty * answer_factor

where:
  avg_difficulty = sum_difficulty / total
  answer_factor = log2(total + 1) / log2(50)  // caps at 1.0 for 50+ answers
```

### Difficulty Adjustment Algorithm

```rust
fn adjust_difficulty(session: &mut GameSession, window_size: usize) {
    let recent = session.recent_answers(window_size);
    let accuracy = recent.iter().filter(|a| a.is_correct).count() as f64 / window_size as f64;

    if accuracy > 0.8 {
        session.current_difficulty = (session.current_difficulty + 0.05).min(0.95);
    } else if accuracy < 0.4 {
        session.current_difficulty = (session.current_difficulty - 0.05).max(0.1);
    }
}
```

### Similarity-to-Difficulty Mapping

| Difficulty | Min Similarity | Max Similarity | Description |
|------------|---------------|----------------|-------------|
| 0.1 (Easy) | 0.10 | 0.40 | Very different words |
| 0.5 (Medium) | 0.40 | 0.70 | Moderately similar |
| 0.9 (Hard) | 0.70 | 0.95 | Near-synonyms |

Interpolation: `min_sim = 0.1 + 0.6 * difficulty`, `max_sim = 0.4 + 0.55 * difficulty`

### LLM Phrase Generation Prompt

```
SYSTEM:
You are a phrase generator for a word-guessing game.
Generate a sentence with ONE blank (marked as ___) and provide the word that fills the blank.

Rules:
1. The blank word must be a common English word (noun, verb, or adjective)
2. The sentence should be natural and grammatically correct
3. DO NOT repeat phrases from the conversation history
4. Vary topics: nature, science, emotions, daily life, food, weather, etc.
5. Keep sentences short (5-12 words)

Respond ONLY with JSON: {"phrase": "The ___ rises early.", "word": "sun"}

USER: Generate a new phrase.
```

### Context Window Management

```rust
struct PhraseGenerator {
    conversation_history: Vec<Message>,
    max_history_rounds: usize,  // e.g., 20 rounds before pruning
}

impl PhraseGenerator {
    fn prune_history(&mut self) {
        if self.conversation_history.len() > self.max_history_rounds * 2 {
            // Keep system prompt + last N rounds
            let keep = self.max_history_rounds * 2;
            let drain_count = self.conversation_history.len() - keep - 1;
            self.conversation_history.drain(1..=drain_count);
        }
    }
}
```

### Random Username Pattern

```rust
const ADJECTIVES: &[&str] = &["Swift", "Brave", "Clever", "Quick", "Wise", "Bold", "Sharp", "Keen", "Nimble", "Bright"];
const ANIMALS: &[&str] = &["Fox", "Owl", "Wolf", "Raven", "Tiger", "Eagle", "Hawk", "Bear", "Deer", "Lion"];

fn generate_username() -> String {
    format!("{}{}{:02}",
        ADJECTIVES[rand() % 10],
        ANIMALS[rand() % 10],
        rand() % 100
    )
}
```

---

## DuckDB Schema (Simplified - No Phrases Table)

```sql
-- Word embeddings for similarity search
CREATE TABLE words (
    id VARCHAR PRIMARY KEY,
    text VARCHAR NOT NULL UNIQUE,
    embedding FLOAT[384],
    frequency INTEGER DEFAULT 0
);

-- Game sessions
CREATE TABLE game_sessions (
    id VARCHAR PRIMARY KEY,
    username VARCHAR NOT NULL,
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    total_answers INTEGER DEFAULT 0,
    correct_answers INTEGER DEFAULT 0,
    sum_difficulty FLOAT DEFAULT 0.0,
    current_difficulty FLOAT DEFAULT 0.5,
    is_submitted BOOLEAN DEFAULT FALSE
);

-- Individual answers (includes phrase text for history)
CREATE TABLE answers (
    id VARCHAR PRIMARY KEY,
    session_id VARCHAR REFERENCES game_sessions(id),
    phrase VARCHAR NOT NULL,  -- Store generated phrase
    choices JSON,
    correct_word VARCHAR,
    selected_word VARCHAR,
    is_correct BOOLEAN,
    response_time_ms INTEGER,
    difficulty FLOAT,
    answered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Leaderboard (best score per username)
CREATE TABLE leaderboard (
    username VARCHAR PRIMARY KEY,
    score FLOAT NOT NULL,
    accuracy FLOAT NOT NULL,
    total_answers INTEGER NOT NULL,
    avg_difficulty FLOAT NOT NULL,
    submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Graph: User word knowledge
CREATE TABLE user_word_knowledge (
    session_id VARCHAR,
    word_id VARCHAR REFERENCES words(id),
    times_seen INTEGER DEFAULT 0,
    times_correct INTEGER DEFAULT 0,
    times_as_llm INTEGER DEFAULT 0,
    times_guessed INTEGER DEFAULT 0,
    last_seen TIMESTAMP,
    PRIMARY KEY (session_id, word_id)
);

-- Graph: User confusion patterns
CREATE TABLE user_confusions (
    session_id VARCHAR,
    correct_word_id VARCHAR REFERENCES words(id),
    confused_word_id VARCHAR REFERENCES words(id),
    times INTEGER DEFAULT 1,
    PRIMARY KEY (session_id, correct_word_id, confused_word_id)
);
```

---

## Dependencies

- **TEA-OBS-002** (Opik WASM integration) - Required for tracing
- **DuckDB Rust crate** with VSS and PGQ extensions
- **Word embeddings dataset** (all-MiniLM-L6-v2 or similar)

## Compatibility Requirements

- [x] Existing WASM demo functionality unchanged
- [x] Tab navigation doesn't break current workflow
- [x] LLM callback bridge remains compatible
- [x] No breaking changes to `tea-wasm-llm` API

## Risk Mitigation

| Risk | Mitigation | Rollback |
|------|------------|----------|
| DuckDB WASM size bloat | Use separate bundle, lazy load | Remove game tab |
| Browser storage limits | Warn user, offer reset | Clear game data |
| LLM phrase generation inconsistency | Robust JSON parsing, retry logic | Show error, regenerate |
| Context window overflow | Prune old rounds, keep last N | Reset generator |
| LLM word not in vocabulary | Widen similarity search, or regenerate | Skip round |
| Performance on slow devices | Web Worker for DuckDB, loading states | Graceful degradation |

## Definition of Done

- [ ] All 9 stories completed with acceptance criteria met
- [ ] Game playable in browser with full feature set
- [ ] Leaderboard persists across sessions
- [ ] Opik traces visible in dashboard
- [ ] Existing demo functionality unaffected
- [ ] Playwright E2E tests pass
- [ ] Documentation updated

---

## QA Notes

**Assessment Date:** 2026-01-12
**Test Architect:** Quinn (QA Agent)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Scenarios** | 87 |
| **Unit Tests** | 42 (48%) |
| **Integration Tests** | 31 (36%) |
| **E2E Tests** | 14 (16%) |
| **P0 (Critical)** | 23 |
| **P1 (High)** | 34 |
| **Stories Covered** | 8/8 (100%) |
| **AC Coverage** | All ACs mapped |

### Risk Areas Identified

| Risk | Severity | Mitigation Tests |
|------|----------|------------------|
| Score calculation errors | High | 5 P0 unit tests (GAME-001.1-UNIT-010-014) |
| Leaderboard data loss | High | 3 P0 integration tests (GAME-001.2-INT-009-011) |
| LLM phrase inconsistency | High | 5 P0 unit tests with retry logic |
| Browser storage limits | High | 1 P0 E2E test (GAME-001.6-E2E-001) |
| Context window overflow | Medium | 2 P1 unit tests (GAME-001.4-UNIT-039/040) |
| OOV word handling | Medium | 1 P1 integration test (GAME-001.3-INT-019) |
| Difficulty bounds clamping | Medium | 2 P0 unit tests (GAME-001.1-UNIT-019/020) |

### Recommended Test Scenarios (Priority Order)

**Phase 1 - Fail Fast (P0 Unit + Integration):**
1. All score/difficulty unit tests (Story 1)
2. DuckDB schema and UPSERT tests (Story 2)
3. Similarity algorithm tests (Story 3)
4. JSON parsing and retry logic tests (Story 4)
5. GameEngine orchestration tests (Story 5)
6. WASM export verification (Story 6)

**Phase 2 - Core E2E:**
1. Browser persistence with IndexedDB/OPFS
2. LLM callback bridge validation
3. Core user journey: Welcome → Game → Leaderboard

**Phase 3 - P1 Regression:**
- Rolling window difficulty adjustment
- Visual feedback (green/red flash)
- Give Up flow and Play Again loop

### Concerns and Recommendations

1. **WASM Bundle Size** - DuckDB WASM may add significant bloat. Recommend lazy loading and bundle size monitoring in CI.

2. **Flaky E2E Risk** - LLM-dependent tests may be flaky. Use mock LLM responses for deterministic unit tests; real LLM only for integration smoke tests.

3. **Graph Extension (DuckPGQ)** - Marked P2 since analytics features are secondary. Consider deferring to post-MVP if timeline is tight.

4. **Mobile Viewport** - P3 priority for responsive design test. Recommend manual exploratory testing on actual devices.

5. **Opik Dependency** - TEA-OBS-002 must be complete before Story 8 integration tests can run.

### Test Design Document

Full test matrix available at: `docs/qa/assessments/TEA-GAME-001-test-design-20260112.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial epic draft | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Removed phrases database, added LLM context window generation | Sarah (PO Agent) |
| 2026-01-12 | 0.3 | Added QA Notes with test coverage analysis | Quinn (QA Agent) |
| 2026-01-25 | 0.4 | **DESIGN PIVOT**: Re-introduced pre-defined phrase database (1000 phrases). Game now compares player guess vs LLM's actual completion. Distractors are human-curated, not LLM/embedding-generated. See Story 4 v2.0 for details. | Sarah (PO Agent) |
| 2026-01-25 | 0.5 | Added Story 9 (brownfield): Phrase Database Integration - implements loading of pre-generated 1039 phrases from `data/game_phrases.json`. | Sarah (PO Agent) |
