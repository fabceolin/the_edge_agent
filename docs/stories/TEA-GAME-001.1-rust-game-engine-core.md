# Story TEA-GAME-001.1: Rust Game Engine Core

## Status

Ready for Development

## Story

**As a** developer,
**I want** a Rust game engine with state management, scoring, and difficulty adjustment,
**So that** I can test and debug game logic before WASM compilation.

## Story Context

**Existing System Integration:**

- Integrates with: TEA Rust crate (`rust/src/`)
- Technology: Rust, serde, rand
- Follows pattern: Existing module structure in `rust/src/actions/`
- Touch points: New `rust/src/games/` module

**Dependencies:**

- None (foundational story)

## Acceptance Criteria

1. **AC-1**: `GameSession` struct with `id`, `username`, `total_answers`, `correct_answers`, `current_difficulty`, `sum_difficulty`
2. **AC-2**: `GameRound` struct with `phrase`, `choices` (5 words), `correct_word`, `selected_word`, `is_correct`, `response_time_ms`
3. **AC-3**: `generate_username()` function returns random `{Adjective}{Animal}{Number}` pattern (e.g., "SwiftFox42")
4. **AC-4**: `calculate_score(session)` implements weighted formula: `(correct/total) * avg_difficulty * answer_factor`
5. **AC-5**: `adjust_difficulty(session, window_size)` uses rolling accuracy to modify difficulty (+/-0.05)
6. **AC-6**: Difficulty bounded to `[0.1, 0.95]` range
7. **AC-7**: Unit tests for all score/difficulty calculations with edge cases

## Tasks / Subtasks

- [ ] Create `rust/src/games/mod.rs` module (AC-1, AC-2)
  - [ ] Add `mod games;` to `rust/src/lib.rs`
  - [ ] Define `GameSession` struct with serde derives
  - [ ] Define `GameRound` struct with serde derives
  - [ ] Implement `Default` traits

- [ ] Implement `generate_username()` (AC-3)
  - [ ] Define `ADJECTIVES` and `ANIMALS` const arrays (10 each)
  - [ ] Use `rand` crate for random selection
  - [ ] Format as `{Adj}{Animal}{00-99}`

- [ ] Implement `calculate_score()` (AC-4)
  - [ ] Calculate `accuracy = correct / total`
  - [ ] Calculate `avg_difficulty = sum_difficulty / total`
  - [ ] Calculate `answer_factor = log2(total + 1) / log2(50)`
  - [ ] Return `accuracy * avg_difficulty * answer_factor`
  - [ ] Handle edge case: `total == 0` returns 0.0

- [ ] Implement `adjust_difficulty()` (AC-5, AC-6)
  - [ ] Add `recent_answers: Vec<bool>` to track last N answers
  - [ ] Calculate rolling accuracy over window
  - [ ] If accuracy > 0.8: increase difficulty by 0.05
  - [ ] If accuracy < 0.4: decrease difficulty by 0.05
  - [ ] Clamp result to `[0.1, 0.95]`

- [ ] Write unit tests (AC-7)
  - [ ] Test `generate_username()` format validation
  - [ ] Test `calculate_score()` with various inputs
  - [ ] Test `adjust_difficulty()` boundary conditions
  - [ ] Test difficulty clamping at min/max

## Dev Notes

### Score Formula Details

```rust
pub fn calculate_score(session: &GameSession) -> f64 {
    if session.total_answers == 0 {
        return 0.0;
    }

    let accuracy = session.correct_answers as f64 / session.total_answers as f64;
    let avg_difficulty = session.sum_difficulty / session.total_answers as f64;
    let answer_factor = (session.total_answers as f64 + 1.0).log2() / 50_f64.log2();
    let answer_factor = answer_factor.min(1.0); // Cap at 1.0

    accuracy * avg_difficulty * answer_factor
}
```

### Username Word Lists

```rust
const ADJECTIVES: &[&str] = &[
    "Swift", "Brave", "Clever", "Quick", "Wise",
    "Bold", "Sharp", "Keen", "Nimble", "Bright"
];

const ANIMALS: &[&str] = &[
    "Fox", "Owl", "Wolf", "Raven", "Tiger",
    "Eagle", "Hawk", "Bear", "Deer", "Lion"
];
```

### Relevant Source Tree

```
rust/
├── src/
│   ├── lib.rs           # Add: mod games;
│   └── games/
│       └── mod.rs       # NEW: GameSession, GameRound, scoring
├── Cargo.toml           # rand already included
```

### Testing

- Test file location: `rust/src/games/mod.rs` (inline tests) or `rust/tests/test_games.rs`
- Use `#[cfg(test)]` module with `#[test]` functions
- Test framework: Built-in Rust test framework
- Run with: `cargo test --features game`

## Definition of Done

- [ ] All acceptance criteria met
- [ ] Unit tests pass (`cargo test`)
- [ ] Code compiles without warnings
- [ ] Module properly exported from `lib.rs`
- [ ] No clippy warnings (`cargo clippy`)

---

## QA Notes

**Reviewer:** Quinn (Test Architect)
**Date:** 2026-01-10
**Test Design Reference:** `docs/qa/assessments/TEA-GAME-001.1-test-design-20260110.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 18 (75%) |
| Integration tests | 4 (17%) |
| E2E tests | 2 (8%) |
| P0 (Critical) | 8 |
| P1 (Core) | 10 |
| P2 (Secondary) | 6 |

**Coverage Assessment:** All 7 acceptance criteria have explicit test coverage. Heavy unit test focus is appropriate for pure mathematical/algorithmic logic with no external dependencies.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Division by zero in score calculation | Medium | Critical | UNIT-010 guards `total_answers == 0` |
| Difficulty bounds violated | Medium | Medium | UNIT-020, UNIT-021 verify clamping |
| Score calculation overflow | Low | High | UNIT-011, UNIT-015 verify bounds |
| Floating point precision errors | Low | Medium | UNIT-015 known-value verification |
| Incorrect difficulty adjustment rate | Medium | Medium | UNIT-016, UNIT-017, UNIT-018 |
| Username collision | Low | Low | UNIT-009 statistical distribution |

### Recommended Test Scenarios

**Phase 1 - Critical Path (P0):**
1. `calculate_score()` with zero total answers (division guard)
2. Difficulty lower bound clamping at 0.1
3. Difficulty upper bound clamping at 0.95
4. Difficulty increase on >80% accuracy
5. Difficulty decrease on <40% accuracy
6. Username format regex validation
7. Perfect score calculation
8. Zero score calculation

**Phase 2 - Core Functionality (P1):**
- Struct default initialization tests
- Serde serialization roundtrips
- Rolling window accuracy calculation
- Answer factor cap at 1.0

**Phase 3 - Secondary (P2):**
- Username randomness distribution
- WASM FFI type compatibility
- Full game session lifecycle E2E

### Concerns and Recommendations

1. **Property-based testing recommended** for username generation using `proptest` or `quickcheck` to ensure format validity across many seeds.

2. **Consider `recent_answers: Vec<bool>`** field addition to `GameSession` for rolling window tracking (mentioned in tasks but not in struct definition in AC-1).

3. **Estimated test implementation:** 200-300 lines of Rust test code.

4. **No blockers identified** - story is well-defined with clear acceptance criteria and deterministic behavior.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Notes section | Quinn (Test Architect) |
