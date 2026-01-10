# Test Design: Story TEA-GAME-001.3

**Story:** Word Embedding and Similarity Search
**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 14 (50%)
- **Integration tests:** 11 (39%)
- **E2E tests:** 3 (11%)
- **Priority distribution:** P0: 10, P1: 12, P2: 6

### Strategy Rationale

This story implements core game mechanics for word similarity search. The majority of tests are at unit level because:
1. Difficulty-to-similarity mapping is pure algorithmic logic
2. Fallback logic is deterministic with clear boundaries
3. Edge case handling can be isolated

Integration tests focus on DuckDB VSS (Vector Similarity Search) operations since database interaction is central to the functionality. E2E tests validate the complete word selection flow for game rounds.

---

## Test Scenarios by Acceptance Criteria

### AC-1: Pre-computed word embeddings loaded into DuckDB `words` table

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-INT-001 | Integration | P0 | Verify Parquet file loads into DuckDB `words` table with correct schema (id, text, embedding) | Data integrity critical for all downstream operations |
| TEA-GAME-001.3-INT-002 | Integration | P1 | Verify embedding dimensions match expected size (384 for all-MiniLM-L6-v2) | Wrong dimensions cause runtime failures |
| TEA-GAME-001.3-INT-003 | Integration | P1 | Verify ~10k words loaded successfully with no duplicates | Vocabulary completeness affects game variety |
| TEA-GAME-001.3-UNIT-001 | Unit | P2 | Validate Parquet schema conforms to expected structure | Pre-flight validation before load |

---

### AC-2: `find_similar_words(word, n, min_sim, max_sim)` returns words within similarity range

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-UNIT-002 | Unit | P0 | Test similarity range filtering with known embeddings (mock data) | Core algorithm correctness |
| TEA-GAME-001.3-UNIT-003 | Unit | P0 | Test returns exactly N words when available | API contract verification |
| TEA-GAME-001.3-UNIT-004 | Unit | P1 | Test returns fewer than N words when insufficient matches | Boundary behavior |
| TEA-GAME-001.3-UNIT-005 | Unit | P1 | Test empty result when no words in similarity range | Empty set handling |
| TEA-GAME-001.3-INT-004 | Integration | P0 | Verify `array_cosine_similarity` query returns correct results against real DuckDB | Database function integration |
| TEA-GAME-001.3-INT-005 | Integration | P1 | Test performance with full 10k vocabulary (< 100ms) | Performance requirement for game responsiveness |

---

### AC-3: Difficulty maps to similarity range (Easy/Medium/Hard)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-UNIT-006 | Unit | P0 | Test difficulty 0.1 (Easy) maps to (0.10, 0.40) range | Core difficulty mechanic |
| TEA-GAME-001.3-UNIT-007 | Unit | P0 | Test difficulty 0.5 (Medium) maps to (0.40, 0.70) range | Core difficulty mechanic |
| TEA-GAME-001.3-UNIT-008 | Unit | P0 | Test difficulty 0.9 (Hard) maps to (0.70, 0.95) range | Core difficulty mechanic |
| TEA-GAME-001.3-UNIT-009 | Unit | P1 | Test linear interpolation for intermediate values (0.3, 0.7) | Smooth difficulty scaling |
| TEA-GAME-001.3-UNIT-010 | Unit | P1 | Test boundary clamping (difficulty < 0.1, > 0.9) | Input validation |
| TEA-GAME-001.3-UNIT-011 | Unit | P2 | Test that min_sim is always < max_sim across all inputs | Invariant guarantee |

---

### AC-4: Similar words exclude the target word itself

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-UNIT-012 | Unit | P0 | Test target word excluded from results when in vocabulary | Prevents trivial game answers |
| TEA-GAME-001.3-INT-006 | Integration | P0 | Verify SQL `WHERE text != ?` clause excludes target in real query | Database-level exclusion |

---

### AC-5: Random selection among qualifying words (not just top-N)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-INT-007 | Integration | P1 | Verify `ORDER BY RANDOM()` produces varied results across multiple calls | Game variety requirement |
| TEA-GAME-001.3-UNIT-013 | Unit | P2 | Test that selection is non-deterministic (statistical test over 100 iterations) | Randomness verification |

---

### AC-6: Fallback behavior when insufficient words in range

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-UNIT-014 | Unit | P0 | Test range widening by 0.1 on each retry (up to 3 attempts) | Fallback algorithm correctness |
| TEA-GAME-001.3-INT-008 | Integration | P1 | Verify fallback produces results when initial range too narrow | Real-world fallback validation |
| TEA-GAME-001.3-UNIT-015 | Unit | P1 | Test range bounds clamped to (0.05, 0.99) on final fallback | Prevents invalid similarity values |
| TEA-GAME-001.3-INT-009 | Integration | P2 | Verify warning logged on fallback attempts | Observability requirement |

---

### AC-7: Handle case where LLM-generated word is not in vocabulary

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-UNIT-016 | Unit | P0 | Test `get_word_embedding()` returns `None` for unknown word | Graceful degradation |
| TEA-GAME-001.3-UNIT-017 | Unit | P1 | Test `get_word_embedding()` returns correct embedding for known word | Happy path |
| TEA-GAME-001.3-INT-010 | Integration | P1 | Verify missing word lookup against real DuckDB returns None efficiently | Performance on miss |
| TEA-GAME-001.3-E2E-001 | E2E | P1 | Test game round skips gracefully when LLM word not in vocabulary | User experience on edge case |

---

## End-to-End Test Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.3-E2E-001 | E2E | P1 | Complete word similarity search flow: load embeddings -> query similar -> return distractors | Full pipeline validation |
| TEA-GAME-001.3-E2E-002 | E2E | P1 | Game round with Easy difficulty produces semantically distant distractors | Difficulty mechanic works end-to-end |
| TEA-GAME-001.3-E2E-003 | E2E | P2 | Game round with Hard difficulty produces near-synonym distractors | Challenge scaling validation |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Wrong similarity range for difficulty | Medium | High | TEA-GAME-001.3-UNIT-006 through UNIT-011 |
| Target word returned as distractor | Low | Critical | TEA-GAME-001.3-UNIT-012, INT-006 |
| Empty results crash game | Medium | High | TEA-GAME-001.3-UNIT-005, UNIT-014, INT-008 |
| Performance degradation with 10k words | Medium | Medium | TEA-GAME-001.3-INT-005 |
| Embedding dimension mismatch | Low | Critical | TEA-GAME-001.3-INT-002 |
| Non-random selection reduces variety | Low | Medium | TEA-GAME-001.3-INT-007, UNIT-013 |

---

## Test Data Requirements

### Unit Tests
- Mock embedding vectors (3-5 test words with known similarity values)
- Test cases with boundary similarity values (0.0, 0.5, 1.0)

### Integration Tests
- Small test vocabulary (~100 words) for fast iteration
- Full vocabulary (10k words) for performance tests
- Known word pairs with pre-calculated similarities

### E2E Tests
- Real word embeddings loaded in test DuckDB instance
- Predefined target words with expected difficulty ranges

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on algorithm bugs)
   - Difficulty mapping: UNIT-006, UNIT-007, UNIT-008
   - Core search: UNIT-002, UNIT-003, UNIT-012, UNIT-014
   - Missing word: UNIT-016

2. **P0 Integration tests** (validate database operations)
   - Data load: INT-001
   - Similarity query: INT-004, INT-006

3. **P1 Unit tests** (secondary paths)
   - Edge cases and interpolation

4. **P1 Integration tests** (performance, fallback)
   - INT-005, INT-007, INT-008

5. **P1 E2E tests** (user journeys)
   - E2E-001, E2E-002

6. **P2+ tests** (as time permits)
   - Schema validation, logging, hard mode

---

## Implementation Notes

### Test File Locations (Rust)
```
rust/src/games/embeddings.rs  # Inline #[cfg(test)] module for unit tests
rust/tests/embeddings_integration.rs  # Integration tests
rust/tests/e2e/game_embeddings.rs  # E2E tests (if separate)
```

### Test Utilities Needed
- Mock embedding generator (deterministic vectors for unit tests)
- Test fixture loader for Parquet
- DuckDB test instance factory

### Feature Flags
Tests require `--features game-duckdb` to enable DuckDB integration.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shifted left where possible)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (game mechanics = high)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Edge cases covered for numeric boundaries
- [x] Error conditions tested at appropriate levels

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 14
    integration: 11
    e2e: 3
  by_priority:
    p0: 10
    p1: 12
    p2: 6
  coverage_gaps: []
  critical_tests:
    - TEA-GAME-001.3-UNIT-006  # Easy difficulty mapping
    - TEA-GAME-001.3-UNIT-012  # Target exclusion
    - TEA-GAME-001.3-INT-001   # Embedding load
    - TEA-GAME-001.3-INT-004   # Similarity query
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.3-test-design-20260110.md
P0 tests identified: 10
Story: TEA-GAME-001.3
AC coverage: 7/7 (100%)
```
