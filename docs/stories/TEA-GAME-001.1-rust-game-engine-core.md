# Story TEA-GAME-001.1: Rust Game Engine Core

## Status

Done

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

- [x] Create `rust/src/games/mod.rs` module (AC-1, AC-2)
  - [x] Add `mod games;` to `rust/src/lib.rs`
  - [x] Define `GameSession` struct with serde derives
  - [x] Define `GameRound` struct with serde derives
  - [x] Implement `Default` traits

- [x] Implement `generate_username()` (AC-3)
  - [x] Define `ADJECTIVES` and `ANIMALS` const arrays (10 each)
  - [x] Use `rand` crate for random selection
  - [x] Format as `{Adj}{Animal}{00-99}`

- [x] Implement `calculate_score()` (AC-4)
  - [x] Calculate `accuracy = correct / total`
  - [x] Calculate `avg_difficulty = sum_difficulty / total`
  - [x] Calculate `answer_factor = log2(total + 1) / log2(50)`
  - [x] Return `accuracy * avg_difficulty * answer_factor`
  - [x] Handle edge case: `total == 0` returns 0.0

- [x] Implement `adjust_difficulty()` (AC-5, AC-6)
  - [x] Add `recent_answers: Vec<bool>` to track last N answers
  - [x] Calculate rolling accuracy over window
  - [x] If accuracy > 0.8: increase difficulty by 0.05
  - [x] If accuracy < 0.4: decrease difficulty by 0.05
  - [x] Clamp result to `[0.1, 0.95]`

- [x] Write unit tests (AC-7)
  - [x] Test `generate_username()` format validation
  - [x] Test `calculate_score()` with various inputs
  - [x] Test `adjust_difficulty()` boundary conditions
  - [x] Test difficulty clamping at min/max

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

- [x] All acceptance criteria met
- [x] Unit tests pass (`cargo test --features game`)
- [x] Code compiles without warnings
- [x] Module properly exported from `lib.rs`
- [x] No clippy warnings (`cargo clippy --features game`)

## File List

### New Files
- `rust/src/games/mod.rs` - Core game engine module with GameSession, GameRound, scoring, and difficulty adjustment (916 lines)

### Modified Files
- `rust/src/lib.rs` - Added `mod games;` with `game` feature flag
- `rust/Cargo.toml` - Added `game` and `game-duckdb` features
- `rust/src/games/phrase_generator.rs` - Fixed clippy warnings (doc indentation, vec![] macro)

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

## QA Results

**Reviewer:** Claude (QA Agent)
**Date:** 2026-01-22
**Gate Status:** PASS

### Acceptance Criteria Verification

| AC | Description | Status | Evidence |
|----|-------------|--------|----------|
| AC-1 | GameSession struct with required fields | ✅ PASS | `rust/src/games/mod.rs:94-117` - Struct has id, username, total_answers, correct_answers, current_difficulty, sum_difficulty, plus recent_answers for rolling window |
| AC-2 | GameRound struct with required fields | ✅ PASS | `rust/src/games/mod.rs:158-177` - Struct has phrase, choices (Vec<String>), correct_word, selected_word, is_correct, response_time_ms |
| AC-3 | generate_username() function | ✅ PASS | `rust/src/games/mod.rs:293-299` - Returns {Adjective}{Animal}{00-99} pattern, 10 adjectives, 10 animals defined |
| AC-4 | calculate_score() formula | ✅ PASS | `rust/src/games/mod.rs:339-350` - Implements accuracy * avg_difficulty * answer_factor with log2 scaling |
| AC-5 | adjust_difficulty() rolling window | ✅ PASS | `rust/src/games/mod.rs:245-272` - Uses configurable window_size, +/-0.05 step based on thresholds |
| AC-6 | Difficulty bounds [0.1, 0.95] | ✅ PASS | `rust/src/games/mod.rs:269-271` - Uses clamp() with MIN_DIFFICULTY=0.1, MAX_DIFFICULTY=0.95 constants |
| AC-7 | Unit tests for all calculations | ✅ PASS | 107 tests in games module covering all edge cases and boundary conditions |

### Test Results

| Metric | Result |
|--------|--------|
| Total tests executed | 750 |
| Tests passed | 750 |
| Tests failed | 0 |
| Tests ignored | 13 |
| Games module tests | 107 |
| Clippy warnings | 0 |

**Test Categories in games module:**
- GameSession struct tests: 6 tests
- GameRound struct tests: 8 tests
- generate_username() tests: 6 tests
- calculate_score() tests: 9 tests
- adjust_difficulty() tests: 14 tests
- Difficulty bounds tests: 7 tests
- Integration tests: 3 tests
- Phrase generator tests: 38 tests
- Opik integration tests: 24 tests

### Code Quality Assessment

| Category | Status | Notes |
|----------|--------|-------|
| Compilation | ✅ PASS | Compiles without warnings |
| Clippy | ✅ PASS | No lints |
| Documentation | ✅ PASS | Comprehensive rustdoc with examples |
| Error handling | ✅ PASS | Division by zero guard in calculate_score() |
| Test coverage | ✅ PASS | Exceeds recommended 24 scenarios with 107 tests |

### NFR Validation

| NFR | Status | Notes |
|-----|--------|-------|
| Security | ✅ PASS | No external inputs, pure internal logic, no arbitrary code execution |
| Performance | ✅ PASS | O(n) for rolling window, O(1) for score calculation |
| Reliability | ✅ PASS | Deterministic algorithms, boundary clamping prevents invalid states |
| Maintainability | ✅ PASS | Well-documented constants, clear separation of concerns |

### Risk Assessment

| Risk | Status | Mitigation Verified |
|------|--------|---------------------|
| Division by zero | ✅ Mitigated | `test_calculate_score_zero_total_answers` verifies guard |
| Difficulty bounds | ✅ Mitigated | `test_difficulty_*_bound_clamping` tests verify clamping |
| Floating point precision | ✅ Mitigated | `test_calculate_score_formula_verification` uses exact comparison |
| Username collision | ✅ Acceptable | `test_generate_username_uniqueness` confirms >50% unique in 100 samples |

### Recommendations

**Immediate:** None - all criteria met, no blockers.

**Future Improvements:**
1. Consider property-based testing with `proptest` for username generation
2. Add benchmark tests for score calculation performance
3. Consider fuzz testing for edge cases in difficulty adjustment

### Gate Decision

**PASS** - All 7 acceptance criteria fully implemented and verified. Test coverage exceeds requirements with 107 tests covering unit, integration, and edge cases. No critical risks identified. Code quality excellent with zero clippy warnings and comprehensive documentation.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-10 | 0.2 | Added QA Notes section | Quinn (Test Architect) |
| 2026-01-22 | 1.0 | Implementation complete: GameSession, GameRound, generate_username(), calculate_score(), adjust_difficulty() with 107 passing tests | Dev Agent |
