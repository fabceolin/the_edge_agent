# Story TEA-GAME-001.3: Word Embedding and Similarity Search

## Status

Ready for Development

**Validation Date:** 2026-01-10
**QA Assessment:** All criteria passed (7/7 AC coverage, 28 test scenarios defined)

## Story

**As a** developer,
**I want** to find semantically similar words based on difficulty threshold,
**So that** the game generates appropriate distractor words.

## Story Context

**Existing System Integration:**

- Integrates with: DuckDB VSS extension, Python embedding patterns
- Technology: DuckDB `array_cosine_similarity`, pre-computed embeddings
- Follows pattern: `python/src/the_edge_agent/memory/vector/duckdb.py`
- Touch points: `rust/src/games/embeddings.rs`, word Parquet file

**Dependencies:**

- Story 2 (TEA-GAME-001.2): DuckDB schema with `words` table

## Acceptance Criteria

1. **AC-1**: Pre-computed word embeddings loaded into DuckDB `words` table (common vocabulary ~10k words)
2. **AC-2**: `find_similar_words(word, n, min_sim, max_sim)` returns words within similarity range
3. **AC-3**: Difficulty maps to similarity range: Easy (0.1-0.4), Medium (0.4-0.7), Hard (0.7-0.95)
4. **AC-4**: Similar words exclude the target word itself
5. **AC-5**: Random selection among qualifying words (not just top-N)
6. **AC-6**: Fallback behavior when insufficient words in range (widen range progressively)
7. **AC-7**: Handle case where LLM-generated word is not in vocabulary (return empty or generate embedding)

## Tasks / Subtasks

- [ ] Source word embeddings (AC-1)
  - [ ] Download or generate embeddings for common English vocabulary
  - [ ] Use all-MiniLM-L6-v2 (384 dimensions) or similar
  - [ ] Export to Parquet format with columns: `id`, `text`, `embedding`
  - [ ] Target ~10,000 common words

- [ ] Create embedding loader (AC-1)
  - [ ] Script to load Parquet into DuckDB `words` table
  - [ ] Handle duplicate words gracefully
  - [ ] Log loading progress and statistics

- [ ] Create `rust/src/games/embeddings.rs` module (AC-2 through AC-7)
  - [ ] Add `mod embeddings;` to `rust/src/games/mod.rs`
  - [ ] Create `EmbeddingSearch` struct with DuckDB connection

- [ ] Implement `find_similar_words()` (AC-2, AC-4, AC-5)
  - [ ] Query word embedding from `words` table
  - [ ] Use `array_cosine_similarity` for similarity calculation
  - [ ] Filter by `min_sim <= similarity <= max_sim`
  - [ ] Exclude the target word itself
  - [ ] Use `ORDER BY RANDOM()` for random selection
  - [ ] Return `Vec<String>` of similar words

- [ ] Implement difficulty mapping (AC-3)
  - [ ] `difficulty_to_similarity_range(difficulty: f64) -> (f64, f64)`
  - [ ] Easy (0.1): returns (0.10, 0.40)
  - [ ] Medium (0.5): returns (0.40, 0.70)
  - [ ] Hard (0.9): returns (0.70, 0.95)
  - [ ] Linear interpolation for values between

- [ ] Implement fallback logic (AC-6)
  - [ ] If fewer than N words found, widen range by 0.1 on each side
  - [ ] Retry up to 3 times
  - [ ] Log warning if falling back

- [ ] Handle missing words (AC-7)
  - [ ] `get_word_embedding(word: &str) -> Option<Vec<f64>>`
  - [ ] Return `None` if word not in vocabulary
  - [ ] Caller decides to skip round or use alternative

- [ ] Write tests
  - [ ] Test similarity search with known embeddings
  - [ ] Test difficulty mapping edge cases
  - [ ] Test fallback behavior
  - [ ] Test missing word handling

## Dev Notes

### Similarity Search Query

```sql
SELECT text, array_cosine_similarity(embedding, ?) AS similarity
FROM words
WHERE text != ?
  AND array_cosine_similarity(embedding, ?) BETWEEN ? AND ?
ORDER BY RANDOM()
LIMIT ?
```

### Difficulty-to-Similarity Mapping

```rust
pub fn difficulty_to_similarity_range(difficulty: f64) -> (f64, f64) {
    // Linear interpolation
    // difficulty 0.1 -> (0.10, 0.40)  -- easy: different words
    // difficulty 0.9 -> (0.70, 0.95)  -- hard: near-synonyms

    let min_sim = 0.1 + 0.6 * difficulty;  // 0.1 -> 0.16, 0.9 -> 0.64
    let max_sim = 0.4 + 0.55 * difficulty; // 0.1 -> 0.455, 0.9 -> 0.895

    (min_sim.clamp(0.1, 0.9), max_sim.clamp(0.2, 0.95))
}
```

### Fallback Strategy

```rust
pub fn find_similar_words_with_fallback(
    &self,
    word: &str,
    count: usize,
    difficulty: f64,
) -> Result<Vec<String>> {
    let (mut min_sim, mut max_sim) = difficulty_to_similarity_range(difficulty);

    for attempt in 0..3 {
        let words = self.find_similar_words(word, count, min_sim, max_sim)?;
        if words.len() >= count {
            return Ok(words);
        }

        // Widen range
        min_sim = (min_sim - 0.1).max(0.05);
        max_sim = (max_sim + 0.1).min(0.99);

        log::warn!("Attempt {}: widening range to ({}, {})", attempt + 1, min_sim, max_sim);
    }

    // Return what we have
    self.find_similar_words(word, count, 0.05, 0.99)
}
```

### Word Embedding Source Options

1. **GloVe 6B** - Pre-trained, widely available
2. **all-MiniLM-L6-v2** - Sentence-transformers, 384 dims
3. **FastText** - Good for morphological similarity

Recommended: **all-MiniLM-L6-v2** for balance of quality and size.

### Parquet Schema

```
words.parquet:
  - id: string (UUID)
  - text: string (lowercase word)
  - embedding: list<float32>[384]
  - frequency: int64 (optional, for filtering)
```

### Relevant Source Tree

```
rust/
├── src/
│   └── games/
│       ├── mod.rs           # Add: mod embeddings;
│       ├── db.rs            # GameDb with words table
│       └── embeddings.rs    # NEW: EmbeddingSearch
├── data/
│   └── words.parquet        # NEW: Pre-computed embeddings
```

### Testing

- Test file location: `rust/src/games/embeddings.rs` (inline tests)
- Requires test embeddings (small subset for unit tests)
- Test framework: Built-in Rust tests
- Run with: `cargo test --features game-duckdb`

## Definition of Done

- [ ] Word embeddings Parquet file created (10k+ words)
- [ ] Embeddings loaded into DuckDB successfully
- [ ] `find_similar_words()` returns correct results
- [ ] Difficulty mapping produces expected ranges
- [ ] Fallback logic handles edge cases
- [ ] Tests pass with real embeddings

---

## QA Notes

**Reviewer:** Quinn (Test Architect)
**Date:** 2026-01-10
**Assessment Reference:** `docs/qa/assessments/TEA-GAME-001.3-test-design-20260110.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 28 |
| Unit tests | 14 (50%) |
| Integration tests | 11 (39%) |
| E2E tests | 3 (11%) |
| AC coverage | 7/7 (100%) |

**Priority Distribution:** P0: 10 | P1: 12 | P2: 6

### Risk Areas Identified

| Risk | Probability | Impact | Coverage |
|------|-------------|--------|----------|
| Wrong similarity range for difficulty | Medium | High | 6 unit tests (UNIT-006 to UNIT-011) |
| Target word returned as distractor | Low | **Critical** | UNIT-012, INT-006 |
| Empty results crash game | Medium | High | UNIT-005, UNIT-014, INT-008 |
| Performance degradation with 10k words | Medium | Medium | INT-005 (< 100ms requirement) |
| Embedding dimension mismatch | Low | **Critical** | INT-002 |
| Non-random selection reduces variety | Low | Medium | INT-007, UNIT-013 |

### Recommended Test Scenarios (Critical Path)

**Must-pass P0 tests before merge:**

1. **TEA-GAME-001.3-UNIT-006/007/008** — Difficulty mapping for Easy/Medium/Hard
2. **TEA-GAME-001.3-UNIT-012** — Target word exclusion (prevents trivial answers)
3. **TEA-GAME-001.3-UNIT-014** — Fallback range widening algorithm
4. **TEA-GAME-001.3-INT-001** — Parquet to DuckDB load integrity
5. **TEA-GAME-001.3-INT-004** — `array_cosine_similarity` query correctness
6. **TEA-GAME-001.3-INT-006** — SQL-level target exclusion

### Concerns / Blockers

1. **Test data dependency**: Unit tests require mock embeddings with known similarity values. Recommend creating a test fixture with ~5 words having pre-calculated cosine similarities.

2. **Performance baseline**: The 100ms performance requirement (INT-005) needs a baseline measurement once embeddings are loaded. Consider adding a benchmark test.

3. **Fallback observability**: AC-6 fallback logging (INT-009) is P2 but important for production debugging. Ensure `log::warn!` calls are testable.

4. **No coverage gaps identified**: All 7 acceptance criteria have dedicated test scenarios. Strategy is appropriately shifted-left with majority unit tests for algorithmic logic.

### Test Readiness Checklist

- [ ] Mock embedding generator implemented
- [ ] Test vocabulary Parquet (~100 words for integration tests)
- [ ] DuckDB test instance factory available
- [ ] `--features game-duckdb` flag documented in CI

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.2 | Added QA Notes section | Quinn (Test Architect) |
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
