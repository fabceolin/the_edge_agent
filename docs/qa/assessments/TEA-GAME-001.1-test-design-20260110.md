# Test Design: Story TEA-GAME-001.1

**Title:** Rust Game Engine Core
**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 24 |
| Unit tests | 18 (75%) |
| Integration tests | 4 (17%) |
| E2E tests | 2 (8%) |

### Priority Distribution

| Priority | Count | Percentage |
|----------|-------|------------|
| P0 | 8 | 33% |
| P1 | 10 | 42% |
| P2 | 6 | 25% |

### Test Level Justification

This story is **foundational game engine logic** with:
- Pure mathematical functions (scoring, difficulty)
- Deterministic algorithms (username generation)
- No external dependencies (DB, network, file system)

**Conclusion:** Heavy unit test coverage is appropriate. Integration tests validate module exports and struct serialization. E2E tests validate the complete game flow when integrated with higher-level components.

---

## Test Scenarios by Acceptance Criteria

### AC-1: GameSession Struct

**Requirement:** `GameSession` struct with `id`, `username`, `total_answers`, `correct_answers`, `current_difficulty`, `sum_difficulty`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-001 | Unit | P1 | GameSession default initialization | Verify Default trait produces valid initial state |
| TEA-GAME-001.1-UNIT-002 | Unit | P1 | GameSession field types and bounds | Ensure correct data types and no overflow potential |
| TEA-GAME-001.1-INT-001 | Integration | P1 | GameSession serde serialization | Verify JSON serialization/deserialization roundtrip |
| TEA-GAME-001.1-INT-002 | Integration | P2 | GameSession WASM-compatible types | Validate types work across FFI boundary |

---

### AC-2: GameRound Struct

**Requirement:** `GameRound` struct with `phrase`, `choices` (5 words), `correct_word`, `selected_word`, `is_correct`, `response_time_ms`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-003 | Unit | P1 | GameRound default initialization | Verify Default trait produces valid initial state |
| TEA-GAME-001.1-UNIT-004 | Unit | P1 | GameRound choices array size | Ensure choices array always has exactly 5 elements |
| TEA-GAME-001.1-INT-003 | Integration | P1 | GameRound serde serialization | Verify JSON serialization/deserialization roundtrip |

---

### AC-3: generate_username() Function

**Requirement:** Returns random `{Adjective}{Animal}{Number}` pattern (e.g., "SwiftFox42")

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-005 | Unit | P0 | Username format validation | Regex match for `^[A-Z][a-z]+[A-Z][a-z]+\d{2}$` |
| TEA-GAME-001.1-UNIT-006 | Unit | P1 | Username adjective from valid list | Extracted adjective exists in ADJECTIVES array |
| TEA-GAME-001.1-UNIT-007 | Unit | P1 | Username animal from valid list | Extracted animal exists in ANIMALS array |
| TEA-GAME-001.1-UNIT-008 | Unit | P1 | Username number range 00-99 | Number suffix is 2 digits, range 0-99 |
| TEA-GAME-001.1-UNIT-009 | Unit | P2 | Username randomness distribution | Multiple calls produce different usernames (statistical) |

---

### AC-4: calculate_score() Function

**Requirement:** Weighted formula: `(correct/total) * avg_difficulty * answer_factor`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-010 | Unit | P0 | Score zero total answers | Returns 0.0 when total_answers == 0 (edge case) |
| TEA-GAME-001.1-UNIT-011 | Unit | P0 | Score perfect accuracy | 100% correct answers yields expected result |
| TEA-GAME-001.1-UNIT-012 | Unit | P0 | Score zero accuracy | 0% correct answers yields 0.0 |
| TEA-GAME-001.1-UNIT-013 | Unit | P1 | Score answer_factor cap at 1.0 | answer_factor never exceeds 1.0 at 50+ answers |
| TEA-GAME-001.1-UNIT-014 | Unit | P1 | Score difficulty weighting | Higher avg_difficulty increases score proportionally |
| TEA-GAME-001.1-UNIT-015 | Unit | P2 | Score known value verification | Specific inputs produce expected calculated output |

**Test Data for UNIT-015:**
```
Input: total=10, correct=8, sum_difficulty=5.0
Expected: accuracy=0.8, avg_diff=0.5, answer_factor=log2(11)/log2(50)=0.612
Result: 0.8 * 0.5 * 0.612 = 0.245 (approx)
```

---

### AC-5: adjust_difficulty() Function

**Requirement:** Uses rolling accuracy over window to modify difficulty (+/-0.05)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-016 | Unit | P0 | Difficulty increase on high accuracy | >80% rolling accuracy increases by 0.05 |
| TEA-GAME-001.1-UNIT-017 | Unit | P0 | Difficulty decrease on low accuracy | <40% rolling accuracy decreases by 0.05 |
| TEA-GAME-001.1-UNIT-018 | Unit | P1 | Difficulty stable in mid-range | 40-80% accuracy maintains current difficulty |
| TEA-GAME-001.1-UNIT-019 | Unit | P1 | Rolling window respects size | Only considers last N answers for calculation |

---

### AC-6: Difficulty Bounds

**Requirement:** Difficulty bounded to `[0.1, 0.95]` range

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-UNIT-020 | Unit | P0 | Difficulty lower bound clamping | Difficulty never goes below 0.1 |
| TEA-GAME-001.1-UNIT-021 | Unit | P0 | Difficulty upper bound clamping | Difficulty never exceeds 0.95 |
| TEA-GAME-001.1-UNIT-022 | Unit | P2 | Boundary exact values allowed | 0.1 and 0.95 are valid difficulty values |

---

### AC-7: Unit Tests Coverage (Meta-verification)

**Requirement:** Unit tests for all score/difficulty calculations with edge cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-GAME-001.1-INT-004 | Integration | P2 | Module export verification | All public functions exported from games module |
| TEA-GAME-001.1-E2E-001 | E2E | P2 | Full game session lifecycle | Create session, play rounds, verify score/difficulty |
| TEA-GAME-001.1-E2E-002 | E2E | P2 | Difficulty adaptation over time | 20+ rounds show appropriate difficulty curve |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Score calculation overflow | Low | High | UNIT-010, UNIT-011, UNIT-015 |
| Difficulty bounds violated | Medium | Medium | UNIT-020, UNIT-021, UNIT-022 |
| Username collision | Low | Low | UNIT-009 (statistical) |
| Division by zero | Medium | Critical | UNIT-010 |
| Floating point precision errors | Low | Medium | UNIT-015 |
| Incorrect difficulty adjustment rate | Medium | Medium | UNIT-016, UNIT-017, UNIT-018 |

---

## Recommended Execution Order

### Phase 1: Critical Path (P0) - Run First
1. TEA-GAME-001.1-UNIT-010 - Score zero division guard
2. TEA-GAME-001.1-UNIT-020 - Difficulty lower bound
3. TEA-GAME-001.1-UNIT-021 - Difficulty upper bound
4. TEA-GAME-001.1-UNIT-016 - Difficulty increase logic
5. TEA-GAME-001.1-UNIT-017 - Difficulty decrease logic
6. TEA-GAME-001.1-UNIT-005 - Username format
7. TEA-GAME-001.1-UNIT-011 - Perfect score
8. TEA-GAME-001.1-UNIT-012 - Zero score

### Phase 2: Core Functionality (P1) - Run Second
9. TEA-GAME-001.1-UNIT-001 - GameSession default
10. TEA-GAME-001.1-UNIT-002 - GameSession types
11. TEA-GAME-001.1-UNIT-003 - GameRound default
12. TEA-GAME-001.1-UNIT-004 - GameRound choices
13. TEA-GAME-001.1-UNIT-006 - Username adjective
14. TEA-GAME-001.1-UNIT-007 - Username animal
15. TEA-GAME-001.1-UNIT-008 - Username number
16. TEA-GAME-001.1-UNIT-013 - Answer factor cap
17. TEA-GAME-001.1-UNIT-014 - Difficulty weighting
18. TEA-GAME-001.1-UNIT-018 - Difficulty stable range
19. TEA-GAME-001.1-UNIT-019 - Rolling window
20. TEA-GAME-001.1-INT-001 - GameSession serde
21. TEA-GAME-001.1-INT-003 - GameRound serde

### Phase 3: Secondary (P2) - Run if Time Permits
22. TEA-GAME-001.1-UNIT-009 - Username randomness
23. TEA-GAME-001.1-UNIT-015 - Known value verification
24. TEA-GAME-001.1-UNIT-022 - Boundary exact values
25. TEA-GAME-001.1-INT-002 - WASM compatibility
26. TEA-GAME-001.1-INT-004 - Module exports
27. TEA-GAME-001.1-E2E-001 - Full lifecycle
28. TEA-GAME-001.1-E2E-002 - Difficulty curve

---

## Test Implementation Notes

### Rust Test Framework Usage

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_score_zero_total() {
        // TEA-GAME-001.1-UNIT-010
        let session = GameSession {
            total_answers: 0,
            ..Default::default()
        };
        assert_eq!(calculate_score(&session), 0.0);
    }

    #[test]
    fn test_difficulty_lower_bound() {
        // TEA-GAME-001.1-UNIT-020
        let mut session = GameSession {
            current_difficulty: 0.15,
            ..Default::default()
        };
        // Simulate many wrong answers
        for _ in 0..10 {
            session.recent_answers.push(false);
        }
        adjust_difficulty(&mut session, 10);
        assert!(session.current_difficulty >= 0.1);
    }
}
```

### Property-Based Testing Recommendation

For AC-3 (username generation), consider using `proptest` or `quickcheck`:

```rust
#[cfg(test)]
mod proptests {
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn username_always_valid_format(seed in any::<u64>()) {
            let username = generate_username_with_seed(seed);
            let re = regex::Regex::new(r"^[A-Z][a-z]+[A-Z][a-z]+\d{2}$").unwrap();
            prop_assert!(re.is_match(&username));
        }
    }
}
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 18
    integration: 4
    e2e: 2
  by_priority:
    p0: 8
    p1: 10
    p2: 6
  coverage_gaps: []
  key_risks_mitigated:
    - division_by_zero
    - difficulty_bounds
    - score_calculation
  test_framework: rust_builtin
  estimated_test_loc: 200-300
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit-heavy for pure logic)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (score/difficulty correctness = P0)
- [x] Test IDs follow naming convention `TEA-GAME-001.1-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Edge cases identified for all calculations
- [x] Boundary conditions explicitly tested

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.1-test-design-20260110.md
P0 tests identified: 8
Total scenarios: 24
Coverage: All 7 acceptance criteria mapped
```
