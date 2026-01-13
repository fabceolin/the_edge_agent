# Test Design: Story TEA-GAME-001.4

**Story Title:** LLM Phrase Generation with Context Memory
**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 28 | 100% |
| **Unit tests** | 18 | 64% |
| **Integration tests** | 8 | 29% |
| **E2E tests** | 2 | 7% |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 10 | Critical - JSON parsing, retry logic, core generation |
| P1 | 12 | High - Context management, normalization, error handling |
| P2 | 6 | Medium - Edge cases, boundary conditions |

---

## Test Scenarios by Acceptance Criteria

### AC-1: PhraseGenerator struct maintains conversation history for context window

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-001 | Unit | P0 | `PhraseGenerator::new()` initializes with system prompt | Struct construction is foundational; pure logic |
| TEA-GAME-001.4-UNIT-002 | Unit | P0 | History contains system message as first entry | Core invariant; pure state verification |
| TEA-GAME-001.4-UNIT-003 | Unit | P1 | `max_history_rounds` field is correctly set | Configuration validation; pure logic |
| TEA-GAME-001.4-UNIT-004 | Unit | P1 | History is mutable and allows message additions | State management; isolated behavior |

---

### AC-2: System prompt instructs LLM to generate phrase + word in JSON format

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-005 | Unit | P0 | `PHRASE_SYSTEM_PROMPT` constant contains JSON format instruction | Critical prompt correctness; string validation |
| TEA-GAME-001.4-UNIT-006 | Unit | P1 | System prompt includes repetition avoidance rule | Game logic requirement; string validation |
| TEA-GAME-001.4-UNIT-007 | Unit | P1 | System prompt specifies sentence length constraint (5-12 words) | Quality control; string validation |
| TEA-GAME-001.4-UNIT-008 | Unit | P2 | System prompt includes all 6 generation rules | Completeness verification; string validation |

---

### AC-3: Conversation history includes all previously generated phrases

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-009 | Unit | P0 | User message added before LLM call | Conversation flow; state mutation |
| TEA-GAME-001.4-UNIT-010 | Unit | P0 | Assistant response recorded after successful generation | History tracking; state mutation |
| TEA-GAME-001.4-INT-001 | Integration | P1 | History contains correct sequence: system → user → assistant → user → assistant | Multi-step flow verification |
| TEA-GAME-001.4-INT-002 | Integration | P1 | LLM receives full conversation history for context | Callback receives correct data |

---

### AC-4: `generate_phrase()` returns `PhraseResult { phrase: String, word: String }`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-011 | Unit | P0 | Valid JSON response parsed to `PhraseResult` | Core functionality; pure parsing |
| TEA-GAME-001.4-UNIT-012 | Unit | P0 | `phrase` field extracted correctly | Field mapping; pure parsing |
| TEA-GAME-001.4-UNIT-013 | Unit | P0 | `word` field extracted and normalized | Field mapping + transformation |
| TEA-GAME-001.4-INT-003 | Integration | P1 | `generate_phrase()` returns `Ok(PhraseResult)` on success | Full function flow with mock LLM |

---

### AC-5: JSON parsing with error handling for malformed responses

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-014 | Unit | P0 | Malformed JSON triggers parse error | Error path; pure parsing |
| TEA-GAME-001.4-UNIT-015 | Unit | P1 | Missing `phrase` field returns error | Schema validation; pure parsing |
| TEA-GAME-001.4-UNIT-016 | Unit | P1 | Missing `word` field returns error | Schema validation; pure parsing |
| TEA-GAME-001.4-UNIT-017 | Unit | P1 | Empty string response returns error | Edge case; pure parsing |
| TEA-GAME-001.4-UNIT-018 | Unit | P2 | JSON with extra fields still parses successfully | Serde flexibility; pure parsing |

---

### AC-6: Retry logic (up to 3 attempts) if LLM returns invalid format

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-INT-004 | Integration | P0 | First attempt fails, second succeeds → returns success | Retry behavior; stateful flow |
| TEA-GAME-001.4-INT-005 | Integration | P0 | All 3 attempts fail → returns `PhraseError::ParseFailed` | Error escalation; stateful flow |
| TEA-GAME-001.4-INT-006 | Integration | P1 | LLM callback invoked exactly 3 times on repeated failure | Call count verification |
| TEA-GAME-001.4-INT-007 | Integration | P2 | Retry reuses same conversation context (no duplicate user messages) | State consistency during retry |

---

### AC-7: Context window pruning strategy if history exceeds token limit

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-019 | Unit | P1 | `prune_history()` keeps system prompt | Core invariant; pure logic |
| TEA-GAME-001.4-UNIT-020 | Unit | P1 | `prune_history()` keeps last N rounds (user+assistant pairs) | Pruning algorithm; pure logic |
| TEA-GAME-001.4-UNIT-021 | Unit | P1 | History below threshold not pruned | Boundary condition; pure logic |
| TEA-GAME-001.4-UNIT-022 | Unit | P2 | History exactly at threshold not pruned | Boundary condition; pure logic |
| TEA-GAME-001.4-INT-008 | Integration | P1 | Pruning occurs after each successful generation | Timing verification in flow |

---

### AC-8: Word extraction handles edge cases (punctuation, casing normalization)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-UNIT-023 | Unit | P0 | `normalize_word("SUN")` → `"sun"` | Case normalization; pure function |
| TEA-GAME-001.4-UNIT-024 | Unit | P1 | `normalize_word("sun.")` → `"sun"` | Punctuation removal; pure function |
| TEA-GAME-001.4-UNIT-025 | Unit | P1 | `normalize_word("  sun  ")` → `"sun"` | Whitespace trimming; pure function |
| TEA-GAME-001.4-UNIT-026 | Unit | P1 | `normalize_word("bright sun")` → `"bright"` | Multi-word handling (take first); pure function |
| TEA-GAME-001.4-UNIT-027 | Unit | P2 | `normalize_word("")` → `""` | Empty input edge case; pure function |
| TEA-GAME-001.4-UNIT-028 | Unit | P2 | `normalize_word("123")` → `""` | Numeric-only input; pure function |

---

## E2E Test Scenarios (Critical Path Validation)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| TEA-GAME-001.4-E2E-001 | E2E | P1 | Generate 5 consecutive phrases without repetition | Game session behavior; real LLM integration |
| TEA-GAME-001.4-E2E-002 | E2E | P2 | Generate phrases until context pruning triggers | Long-running session behavior |

**Note:** E2E tests require actual LLM integration (WASM browser environment or native Rust with LLM callback). These validate the complete flow but are more expensive to run.

---

## Risk Coverage Matrix

| Risk | Mitigated By | Tests |
|------|--------------|-------|
| LLM returns invalid JSON | Retry logic + error handling | TEA-GAME-001.4-INT-004, INT-005, UNIT-014 |
| Context window overflow | Pruning strategy | TEA-GAME-001.4-UNIT-019 through UNIT-022, INT-008 |
| Word normalization inconsistency | Comprehensive edge case tests | TEA-GAME-001.4-UNIT-023 through UNIT-028 |
| History state corruption | State mutation tests | TEA-GAME-001.4-UNIT-009, UNIT-010, INT-001 |
| Repeated phrases in session | Context history verification | TEA-GAME-001.4-INT-002, E2E-001 |

---

## Test Data Requirements

### Mock LLM Responses

```rust
// Valid response
r#"{"phrase": "The ___ rises early.", "word": "sun"}"#

// Valid with extra fields (should still parse)
r#"{"phrase": "The ___ barks.", "word": "dog", "extra": "ignored"}"#

// Invalid - malformed JSON
"This is not JSON at all"

// Invalid - missing phrase
r#"{"word": "cat"}"#

// Invalid - missing word
r#"{"phrase": "The ___ meows."}"#

// Valid - word needs normalization
r#"{"phrase": "The ___ shines.", "word": "SUN."}"#
```

### Test Fixtures

- `PhraseGenerator` with `max_history_rounds = 3`
- Pre-populated history for pruning tests
- Sequence of valid/invalid responses for retry tests

---

## Recommended Execution Order

1. **P0 Unit tests** - Fail fast on core logic issues
   - JSON parsing (UNIT-011 through UNIT-014)
   - History initialization (UNIT-001, UNIT-002)
   - Word normalization core (UNIT-023)

2. **P0 Integration tests** - Validate component interaction
   - Retry logic (INT-004, INT-005)
   - Full generation flow (INT-003)

3. **P1 Unit tests** - Secondary logic validation
   - Schema validation (UNIT-015 through UNIT-017)
   - Pruning algorithm (UNIT-019 through UNIT-021)
   - Normalization edge cases (UNIT-024 through UNIT-026)

4. **P1 Integration tests** - Flow verification
   - History sequence (INT-001, INT-002)
   - Retry call counting (INT-006)
   - Pruning timing (INT-008)

5. **P1 E2E tests** - Session behavior
   - Consecutive phrase generation (E2E-001)

6. **P2 tests** - Edge cases and boundary conditions (as time permits)

---

## Test Implementation Notes

### Test Location
- Unit tests: `rust/src/games/phrase_generator.rs` (inline `#[cfg(test)]` module)
- Integration tests: `rust/tests/phrase_generator_integration.rs`
- E2E tests: Requires WASM test harness or native LLM callback

### Test Framework
- Rust built-in test framework
- `tokio` for async test support
- Mock callbacks for unit/integration isolation

### Running Tests
```bash
# Unit + Integration tests
cargo test --features game phrase_generator

# With verbose output
cargo test --features game phrase_generator -- --nocapture
```

---

## Coverage Gaps

- [ ] None identified - all 8 ACs have test coverage

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for pure logic, integration for flows)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (JSON parsing is P0)
- [x] Test IDs follow naming convention (TEA-GAME-001.4-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 18
    integration: 8
    e2e: 2
  by_priority:
    p0: 10
    p1: 12
    p2: 6
  coverage_gaps: []
  risk_mitigations:
    - risk: "LLM returns invalid JSON"
      tests: ["INT-004", "INT-005", "UNIT-014"]
    - risk: "Context window overflow"
      tests: ["UNIT-019", "UNIT-020", "UNIT-021", "UNIT-022", "INT-008"]
    - risk: "Word normalization inconsistency"
      tests: ["UNIT-023", "UNIT-024", "UNIT-025", "UNIT-026", "UNIT-027", "UNIT-028"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-GAME-001.4-test-design-20260110.md
P0 tests identified: 10
Total scenarios: 28
Story coverage: 8/8 ACs (100%)
```
