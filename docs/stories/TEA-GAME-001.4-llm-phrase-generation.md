# Story TEA-GAME-001.4: Pre-defined Phrase Database with LLM Comparison

## Status

Ready for Development (Revised)

## Story

**As a** player,
**I want** to guess what word the LLM would choose for a fill-in-the-blank phrase,
**So that** I can learn to predict LLM behavior and compete on the leaderboard.

## Story Context

**Design Change (2026-01-25):**

The original design had the LLM generate both the phrase AND the correct word, with distractors found via embedding similarity. This was problematic because:
1. The game wasn't truly "guess the LLM" - it was a vocabulary game
2. LLM-generated distractors were often nonsensical
3. Browser-based LLM inference was too slow for real-time phrase generation

**New Design:**
- Pre-defined phrase database (1000 phrases) with human-curated distractors
- Player sees phrase + 5 choices, picks what they think the LLM would say
- LLM generates its answer for the phrase (one word completion)
- Player scores if their guess matches the LLM's choice

**Existing System Integration:**

- Integrates with: TEA LLM callback pattern, DuckDB persistence
- Technology: Rust, serde_json, DuckDB
- Touch points:
  - `rust/tea-wasm-llm/src/game.rs` - Game engine
  - `docs/extra/wasm-demo/app.js` - LLM handler

**Dependencies:**

- Story 1 (TEA-GAME-001.1): `GameSession` struct
- Story 2 (TEA-GAME-001.2): DuckDB schema (add `phrases` table)

## Acceptance Criteria

1. **AC-1**: `phrases` table in DuckDB with `id`, `phrase`, `correct_word`, `distractors` (JSON array), `difficulty`, `category`
2. **AC-2**: 1000 pre-defined phrases loaded from JSON/Parquet file at startup
3. **AC-3**: `get_random_phrase(difficulty_range)` returns phrase matching current difficulty
4. **AC-4**: `get_llm_completion(phrase)` calls LLM to generate its word choice for the blank
5. **AC-5**: LLM prompt is simple: "Complete: {phrase}" with strict single-word response
6. **AC-6**: Scoring compares player's choice with LLM's actual response (not pre-defined word)
7. **AC-7**: Phrase selection avoids repeats within session (track `used_phrase_ids`)
8. **AC-8**: Fallback: If LLM returns invalid response, use pre-defined `correct_word`

## Tasks / Subtasks

- [ ] Update DuckDB schema (AC-1)
  - [ ] Add `phrases` table with columns: `id`, `phrase`, `correct_word`, `distractors`, `difficulty`, `category`
  - [ ] Add index on `difficulty` for range queries
  - [ ] Add `used_phrases` tracking in session

- [ ] Create phrase dataset (AC-2)
  - [ ] Generate 1000 phrases using LLM prompt (see Phrase Generation Prompt below)
  - [ ] Validate JSON format and deduplicate
  - [ ] Balance difficulty distribution (~40% easy, ~35% medium, ~25% hard)
  - [ ] Export as `data/game_phrases.json` or Parquet

- [ ] Implement phrase selection (AC-3, AC-7)
  - [ ] `get_random_phrase(min_difficulty, max_difficulty, exclude_ids)`
  - [ ] Track used phrases in `GameSession.used_phrase_ids`
  - [ ] Reset tracking when all phrases at difficulty level used

- [ ] Implement LLM completion (AC-4, AC-5, AC-8)
  - [ ] Simple prompt: "Complete this sentence with ONE word: {phrase}"
  - [ ] Parse single-word response, normalize (lowercase, strip punctuation)
  - [ ] Timeout handling (use pre-defined word as fallback)
  - [ ] Retry once on invalid response

- [ ] Update scoring logic (AC-6)
  - [ ] Compare player choice with LLM response (not pre-defined word)
  - [ ] Log both LLM response and pre-defined word for analytics

- [ ] Write tests
  - [ ] Phrase loading from JSON
  - [ ] Random selection with exclusion
  - [ ] LLM completion parsing
  - [ ] Scoring comparison logic

## Dev Notes

### Updated DuckDB Schema

```sql
-- Pre-defined phrases for the game
CREATE TABLE phrases (
    id VARCHAR PRIMARY KEY,
    phrase VARCHAR NOT NULL,           -- "The ___ shines brightly."
    correct_word VARCHAR NOT NULL,     -- "sun" (human-curated expected answer)
    distractors JSON NOT NULL,         -- ["moon", "star", "light", "lamp"]
    difficulty FLOAT NOT NULL,         -- 0.0-1.0 (easy to hard)
    category VARCHAR,                  -- "nature", "emotions", etc.
    times_used INTEGER DEFAULT 0,
    avg_llm_match_rate FLOAT DEFAULT 0.5  -- How often LLM picks correct_word
);

CREATE INDEX idx_phrases_difficulty ON phrases(difficulty);

-- Track used phrases per session
ALTER TABLE game_sessions ADD COLUMN used_phrase_ids JSON DEFAULT '[]';
```

### Game Flow (Revised)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         NEW GAME FLOW                                        │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. SELECT PHRASE FROM DATABASE                                             │
│     ┌─────────────────────────────────────────────────────────────────────┐ │
│     │  SELECT * FROM phrases                                              │ │
│     │  WHERE difficulty BETWEEN 0.3 AND 0.5                               │ │
│     │  AND id NOT IN (session.used_phrase_ids)                            │ │
│     │  ORDER BY RANDOM() LIMIT 1                                          │ │
│     └─────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  2. SHOW TO PLAYER                                                          │
│     ┌─────────────────────────────────────────────────────────────────────┐ │
│     │  Phrase: "The ___ shines brightly."                                 │ │
│     │  Choices: [sun] [moon] [star] [light] [lamp]                        │ │
│     │           (shuffled: correct_word + 4 distractors)                  │ │
│     └─────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  3. PLAYER SELECTS ANSWER                                                   │
│     ┌─────────────────────────────────────────────────────────────────────┐ │
│     │  Player picks: "star"                                               │ │
│     └─────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  4. LLM GENERATES ITS ANSWER                                                │
│     ┌─────────────────────────────────────────────────────────────────────┐ │
│     │  Prompt: "Complete with ONE word: The ___ shines brightly."         │ │
│     │  LLM response: "sun"                                                │ │
│     └─────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
│  5. COMPARE AND SCORE                                                       │
│     ┌─────────────────────────────────────────────────────────────────────┐ │
│     │  Player: "star" vs LLM: "sun"                                       │ │
│     │  Result: ❌ INCORRECT                                                │ │
│     │  (Player learns what the LLM actually chose)                        │ │
│     └─────────────────────────────────────────────────────────────────────┘ │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### LLM Completion Prompt

```rust
const LLM_COMPLETION_PROMPT: &str = "Complete this sentence with exactly ONE word.
Respond with ONLY the word, nothing else.

Sentence: {phrase}

Word:";
```

### Phrase Generation Prompt (for creating dataset)

Use this prompt to generate phrase batches (run 20x for 1000 phrases):

```
Generate 50 fill-in-the-blank phrases for a word-guessing game.

Output JSON array:
[
  {
    "id": "phrase_001",
    "phrase": "The ___ rises in the east.",
    "correct_word": "sun",
    "distractors": ["moon", "star", "light", "day"],
    "difficulty": 0.2,
    "category": "nature"
  }
]

Requirements:
1. Phrase has ONE blank marked as ___
2. correct_word is the most natural/expected answer
3. 4 distractors: plausible but less fitting words
4. difficulty: 0.0-0.3 (easy), 0.3-0.6 (medium), 0.6-1.0 (hard)
5. Categories: nature, emotions, daily_life, food, weather, science, animals, actions, objects
6. Balance: ~40% easy, ~35% medium, ~25% hard
7. Single common English words only
8. Phrases 4-10 words long
```

### Difficulty Mapping

| Difficulty | Range | Description | Example |
|------------|-------|-------------|---------|
| Easy | 0.0 - 0.3 | Obvious answer, clear context | "The ___ is yellow" → sun |
| Medium | 0.3 - 0.6 | Multiple plausible, one best | "She felt ___ today" → happy |
| Hard | 0.6 - 1.0 | Ambiguous, many valid options | "The ___ changed" → situation |

### Data File Format

`data/game_phrases.json`:
```json
{
  "version": "1.0",
  "generated_at": "2026-01-25",
  "phrases": [
    {
      "id": "phrase_001",
      "phrase": "The ___ rises in the east.",
      "correct_word": "sun",
      "distractors": ["moon", "star", "light", "day"],
      "difficulty": 0.2,
      "category": "nature"
    }
  ]
}
```

## Definition of Done

- [ ] `phrases` table created in DuckDB schema
- [ ] 1000 phrases generated and validated
- [ ] Phrase selection returns random unused phrase
- [ ] LLM completion generates single-word response
- [ ] Scoring compares player choice with LLM response
- [ ] All tests pass
- [ ] Documentation updated

## File List

| File | Status | Description |
|------|--------|-------------|
| `rust/tea-wasm-llm/src/game.rs` | Modified | Update round generation to use phrase DB |
| `data/game_phrases.json` | New | 1000 pre-defined phrases |
| `scripts/generate_phrases.py` | New | Script to generate phrases using LLM |
| `docs/extra/wasm-demo/app.js` | Modified | Update LLM handler for simple completion |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-25 | 2.0 | **MAJOR REVISION**: Changed from LLM-generated phrases to pre-defined phrase database. Game now compares player guess with LLM's actual response. | Sarah (PO Agent) |
| 2026-01-22 | 1.0 | Implementation complete - 59 tests passing | Claude Opus 4.5 (Dev Agent) |
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
