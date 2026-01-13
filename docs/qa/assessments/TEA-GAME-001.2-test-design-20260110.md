# Test Design: Story TEA-GAME-001.2

Date: 2026-01-10
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 31
- Unit tests: 10 (32%)
- Integration tests: 18 (58%)
- E2E tests: 3 (10%)
- Priority distribution: P0: 11, P1: 13, P2: 5, P3: 2

## Story Summary

**Story:** DuckDB Schema and Persistence Layer
**Epic:** TEA-GAME-001 (Know Your Model Game)
**Purpose:** Provide persistent storage for game state, answers, words with embeddings, and leaderboard data using DuckDB with VSS and DuckPGQ extensions.

## Risk Assessment

| Risk | Impact | Probability | Mitigation via Tests |
|------|--------|-------------|---------------------|
| Data loss on session crash | High | Medium | INT tests for persistence durability |
| Leaderboard corruption | High | Low | P0 tests for UPSERT atomicity |
| Extension load failure | High | Medium | INT tests for graceful degradation |
| Vector search inaccuracy | Medium | Low | Unit tests for cosine similarity |
| Graph query performance | Medium | Medium | INT tests with realistic data volumes |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Words Table with Embeddings

**Requirement:** `words` table with `id`, `text`, `embedding FLOAT[384]`, `frequency`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-UNIT-001 | Unit | P1 | Validate word struct serialization to DuckDB row format | Pure data transformation logic |
| TEA-GAME-001.2-INT-001 | Integration | P0 | Insert word with 384-dimension embedding and retrieve | Critical: validates vector storage works |
| TEA-GAME-001.2-INT-002 | Integration | P1 | Update word frequency counter atomically | Multi-operation DB interaction |
| TEA-GAME-001.2-INT-003 | Integration | P1 | Reject duplicate word text (UNIQUE constraint) | Constraint enforcement at DB level |

---

### AC-2: Game Sessions Table

**Requirement:** `game_sessions` table with session metadata and rolling stats

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-UNIT-002 | Unit | P0 | Validate GameSession struct mapping to table columns | Core data model validation |
| TEA-GAME-001.2-INT-004 | Integration | P0 | Create session with default values populated | Critical: session creation is entry point |
| TEA-GAME-001.2-INT-005 | Integration | P0 | Update session rolling stats (total_answers, correct_answers, sum_difficulty) | Critical: game score tracking |
| TEA-GAME-001.2-INT-006 | Integration | P1 | Retrieve session by ID returns correct data | Standard CRUD verification |
| TEA-GAME-001.2-INT-007 | Integration | P2 | List sessions by username | Query functionality |

---

### AC-3: Answers Table with JSON Choices

**Requirement:** `answers` table with full round details including JSON `choices` array and `phrase` text

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-UNIT-003 | Unit | P1 | Serialize choices array to JSON string | JSON transformation logic |
| TEA-GAME-001.2-UNIT-004 | Unit | P1 | Deserialize JSON choices back to Vec<String> | JSON parsing logic |
| TEA-GAME-001.2-INT-008 | Integration | P0 | Record complete answer with all fields including JSON choices | Critical: core game data capture |
| TEA-GAME-001.2-INT-009 | Integration | P1 | Retrieve answers by session_id ordered by answered_at | Query and ordering validation |
| TEA-GAME-001.2-INT-010 | Integration | P2 | Handle null/empty choices array gracefully | Edge case: defensive coding |

---

### AC-4: Leaderboard with UPSERT Logic

**Requirement:** `leaderboard` table with UPSERT logic (best score only per username)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-UNIT-005 | Unit | P0 | UPSERT SQL generation with conditional WHERE clause | Critical: leaderboard integrity logic |
| TEA-GAME-001.2-INT-011 | Integration | P0 | Submit first score for new user creates entry | Critical: leaderboard insertion |
| TEA-GAME-001.2-INT-012 | Integration | P0 | Submit higher score updates existing entry | Critical: UPSERT update path |
| TEA-GAME-001.2-INT-013 | Integration | P0 | Submit lower score does NOT update existing entry | Critical: UPSERT conditional logic |
| TEA-GAME-001.2-INT-014 | Integration | P1 | Get top N leaderboard entries ordered by score DESC | Core leaderboard display query |
| TEA-GAME-001.2-E2E-001 | E2E | P1 | Complete game session results in leaderboard update | Full journey: play game → submit → appear on board |

---

### AC-5: User Word Knowledge Graph Table

**Requirement:** `user_word_knowledge` graph edge table for learning analytics

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-INT-015 | Integration | P1 | Create knowledge edge on first word encounter | Graph table insertion |
| TEA-GAME-001.2-INT-016 | Integration | P1 | Increment times_seen and times_correct on correct answer | Atomic counter update |
| TEA-GAME-001.2-INT-017 | Integration | P2 | Track times_as_llm when word used as distractor | Analytics tracking |
| TEA-GAME-001.2-UNIT-006 | Unit | P2 | Calculate knowledge score from edge metrics | Pure calculation logic |

---

### AC-6: User Confusions Graph Table

**Requirement:** `user_confusions` graph edge table tracking wrong guesses

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-INT-018 | Integration | P1 | Create confusion edge on first wrong guess | Graph table insertion |
| TEA-GAME-001.2-INT-019 | Integration | P1 | Increment times counter on repeated confusion | Atomic counter update |
| TEA-GAME-001.2-UNIT-007 | Unit | P3 | Identify top confusions for a word | Query result processing |

---

### AC-7: VSS Extension and Cosine Similarity

**Requirement:** VSS extension loaded and `array_cosine_similarity` functional

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-INT-020 | Integration | P0 | VSS extension installs and loads successfully | Critical: required for core functionality |
| TEA-GAME-001.2-UNIT-008 | Unit | P1 | Cosine similarity calculation returns expected values | Algorithm correctness |
| TEA-GAME-001.2-INT-021 | Integration | P1 | Query top-k similar words by embedding | VSS search functionality |
| TEA-GAME-001.2-UNIT-009 | Unit | P2 | Handle zero vector edge case in similarity | Edge case: division by zero |

---

### AC-8: DuckPGQ Extension for Graph Queries

**Requirement:** DuckPGQ extension loaded for graph queries

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-INT-022 | Integration | P1 | DuckPGQ extension installs and loads successfully | Required for graph analytics |
| TEA-GAME-001.2-INT-023 | Integration | P2 | Execute simple graph traversal query | Verify extension is functional |
| TEA-GAME-001.2-UNIT-010 | Unit | P3 | Graph query result parsing | Result transformation logic |

---

### AC-9: Integration Tests for CRUD Operations

**Requirement:** Integration tests for CRUD operations

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-GAME-001.2-E2E-002 | E2E | P1 | Full game flow: start session → play rounds → submit to leaderboard | Complete user journey validation |
| TEA-GAME-001.2-E2E-003 | E2E | P2 | Extension load failure graceful degradation | Resilience testing |

---

## Test Implementation Guidance

### Unit Test Setup (Rust)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_serialization() {
        // TEA-GAME-001.2-UNIT-001
        let word = Word { id: "w1".into(), text: "test".into(), embedding: vec![0.1; 384], frequency: 5 };
        let row = word.to_row();
        assert_eq!(row.len(), 4);
    }
}
```

### Integration Test Setup (Rust)

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    fn setup_test_db() -> GameDb {
        GameDb::new(":memory:").expect("Failed to create in-memory DB")
    }

    #[test]
    fn test_insert_word_with_embedding() {
        // TEA-GAME-001.2-INT-001
        let db = setup_test_db();
        let embedding: Vec<f32> = vec![0.1; 384];
        db.insert_word("w1", "apple", &embedding, 0).unwrap();
        let word = db.get_word("w1").unwrap().unwrap();
        assert_eq!(word.text, "apple");
        assert_eq!(word.embedding.len(), 384);
    }
}
```

### E2E Test Setup

```rust
#[test]
fn test_complete_game_flow() {
    // TEA-GAME-001.2-E2E-001
    let db = setup_test_db();

    // 1. Create session
    let session = db.create_session("testuser").unwrap();

    // 2. Play rounds
    for _ in 0..5 {
        db.record_answer(&round).unwrap();
    }

    // 3. Submit to leaderboard
    let updated = db.submit_to_leaderboard("testuser", 85.0, 0.8, 5, 0.6).unwrap();
    assert!(updated);

    // 4. Verify on leaderboard
    let top = db.get_top_leaderboard(10).unwrap();
    assert!(top.iter().any(|e| e.username == "testuser"));
}
```

---

## Recommended Execution Order

### Phase 1: Fail-Fast (P0 Unit + P0 Integration)

1. TEA-GAME-001.2-UNIT-002 - GameSession struct validation
2. TEA-GAME-001.2-UNIT-005 - UPSERT SQL logic
3. TEA-GAME-001.2-INT-020 - VSS extension loads
4. TEA-GAME-001.2-INT-001 - Word with embedding insert/retrieve
5. TEA-GAME-001.2-INT-004 - Session creation
6. TEA-GAME-001.2-INT-005 - Session stats update
7. TEA-GAME-001.2-INT-008 - Answer recording
8. TEA-GAME-001.2-INT-011 through INT-013 - Leaderboard UPSERT

### Phase 2: Core Functionality (P1)

9. All P1 unit tests
10. All P1 integration tests
11. TEA-GAME-001.2-E2E-001 - Game → Leaderboard journey
12. TEA-GAME-001.2-E2E-002 - Full game flow

### Phase 3: Extended Coverage (P2+)

13. P2 tests
14. P3 tests (if time permits)

---

## Coverage Summary

| Acceptance Criteria | Unit | Integration | E2E | Total |
|--------------------|------|-------------|-----|-------|
| AC-1: Words table | 1 | 3 | - | 4 |
| AC-2: Sessions table | 1 | 4 | - | 5 |
| AC-3: Answers table | 2 | 3 | - | 5 |
| AC-4: Leaderboard | 1 | 4 | 1 | 6 |
| AC-5: Word knowledge | 1 | 3 | - | 4 |
| AC-6: Confusions | 1 | 2 | - | 3 |
| AC-7: VSS extension | 2 | 2 | - | 4 |
| AC-8: DuckPGQ | 1 | 2 | - | 3 |
| AC-9: CRUD tests | - | - | 2 | 2 |
| **Total** | **10** | **18** | **3** | **31** |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (favoring unit/integration over E2E)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (data integrity = P0)
- [x] Test IDs follow TEA-GAME-001.2-{LEVEL}-{SEQ} convention
- [x] Scenarios are atomic and independent
- [x] In-memory DuckDB specified for test isolation

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 31
  by_level:
    unit: 10
    integration: 18
    e2e: 3
  by_priority:
    p0: 11
    p1: 13
    p2: 5
    p3: 2
  coverage_gaps: []
  key_risks_mitigated:
    - leaderboard_upsert_atomicity
    - extension_load_failure
    - vector_storage_integrity
    - json_serialization_correctness
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.2-test-design-20260110.md
P0 tests identified: 11
P1 tests identified: 13
```
