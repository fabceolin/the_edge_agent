# Story TEA-GAME-001.4: LLM Phrase Generation with Context Memory

## Status

Ready for Development

## Story

**As a** developer,
**I want** the LLM to generate phrases dynamically using context window memory,
**So that** phrases are varied and never repeat within a session.

## Story Context

**Existing System Integration:**

- Integrates with: TEA LLM callback pattern, existing `llm.call` action
- Technology: Rust, serde_json, LLM callback bridge
- Follows pattern: `rust/tea-wasm-llm/src/llm.rs` callback pattern
- Touch points: New `rust/src/games/phrase_generator.rs`

**Dependencies:**

- Story 1 (TEA-GAME-001.1): `GameSession` struct

## Acceptance Criteria

1. **AC-1**: `PhraseGenerator` struct maintains conversation history for context window
2. **AC-2**: System prompt instructs LLM to generate phrase + word in JSON format
3. **AC-3**: Conversation history includes all previously generated phrases (for repetition avoidance)
4. **AC-4**: `generate_phrase()` returns `PhraseResult { phrase: String, word: String }` parsed from LLM response
5. **AC-5**: JSON parsing with error handling for malformed responses
6. **AC-6**: Retry logic (up to 3 attempts) if LLM returns invalid format
7. **AC-7**: Context window pruning strategy if history exceeds token limit (keep last N rounds)
8. **AC-8**: Word extraction handles edge cases (punctuation, casing normalization)

## Tasks / Subtasks

- [ ] Create `rust/src/games/phrase_generator.rs` module (AC-1)
  - [ ] Add `mod phrase_generator;` to `rust/src/games/mod.rs`
  - [ ] Define `PhraseGenerator` struct with conversation history
  - [ ] Define `Message` struct for conversation entries
  - [ ] Define `PhraseResult` struct for output

- [ ] Implement system prompt (AC-2)
  - [ ] Create `PHRASE_SYSTEM_PROMPT` constant
  - [ ] Include all generation rules
  - [ ] Specify JSON output format

- [ ] Implement conversation history management (AC-3, AC-7)
  - [ ] Store messages as `Vec<Message>`
  - [ ] Add assistant response after each generation
  - [ ] Implement `prune_history(max_rounds: usize)`
  - [ ] Keep system prompt + last N rounds

- [ ] Implement `generate_phrase()` (AC-4, AC-5, AC-6)
  - [ ] Build messages array with history
  - [ ] Call LLM callback with messages
  - [ ] Parse JSON response with `serde_json`
  - [ ] Retry on parse failure (up to 3 times)
  - [ ] Return `Result<PhraseResult, PhraseError>`

- [ ] Implement word normalization (AC-8)
  - [ ] `normalize_word(word: &str) -> String`
  - [ ] Remove punctuation
  - [ ] Convert to lowercase
  - [ ] Trim whitespace
  - [ ] Handle multi-word responses (take first word)

- [ ] Write tests
  - [ ] Test JSON parsing with valid responses
  - [ ] Test JSON parsing with malformed responses
  - [ ] Test retry logic
  - [ ] Test context pruning
  - [ ] Test word normalization edge cases

## Dev Notes

### System Prompt

```rust
const PHRASE_SYSTEM_PROMPT: &str = r#"
You are a phrase generator for a word-guessing game.
Generate a sentence with ONE blank (marked as ___) and provide the word that fills the blank.

Rules:
1. The blank word must be a common English word (noun, verb, or adjective)
2. The sentence should be natural and grammatically correct
3. DO NOT repeat phrases from the conversation history
4. Vary topics: nature, science, emotions, daily life, food, weather, animals, etc.
5. Keep sentences short (5-12 words)
6. The word should be a single, common word (no proper nouns, no rare words)

Respond ONLY with JSON in this exact format:
{"phrase": "The ___ rises early.", "word": "sun"}

Do not include any other text, explanations, or formatting.
"#;
```

### PhraseGenerator Structure

```rust
#[derive(Debug, Clone)]
pub struct Message {
    pub role: String,  // "system", "user", "assistant"
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhraseResult {
    pub phrase: String,
    pub word: String,
}

pub struct PhraseGenerator {
    history: Vec<Message>,
    max_history_rounds: usize,
}

impl PhraseGenerator {
    pub fn new(max_history_rounds: usize) -> Self {
        let mut history = Vec::new();
        history.push(Message {
            role: "system".to_string(),
            content: PHRASE_SYSTEM_PROMPT.to_string(),
        });
        Self { history, max_history_rounds }
    }

    pub async fn generate_phrase<F>(&mut self, llm_callback: F) -> Result<PhraseResult>
    where
        F: Fn(&[Message]) -> Future<Output = Result<String>>,
    {
        // Add user request
        self.history.push(Message {
            role: "user".to_string(),
            content: "Generate a new phrase.".to_string(),
        });

        // Try up to 3 times
        for attempt in 0..3 {
            let response = llm_callback(&self.history).await?;

            match serde_json::from_str::<PhraseResult>(&response) {
                Ok(mut result) => {
                    result.word = normalize_word(&result.word);

                    // Record successful response
                    self.history.push(Message {
                        role: "assistant".to_string(),
                        content: response,
                    });

                    self.prune_history();
                    return Ok(result);
                }
                Err(e) => {
                    log::warn!("Attempt {}: JSON parse error: {}", attempt + 1, e);
                    if attempt == 2 {
                        return Err(PhraseError::ParseFailed(e.to_string()));
                    }
                }
            }
        }

        unreachable!()
    }

    fn prune_history(&mut self) {
        // Keep system prompt (index 0) + last N*2 messages (user+assistant pairs)
        let max_messages = 1 + (self.max_history_rounds * 2);
        if self.history.len() > max_messages {
            let remove_count = self.history.len() - max_messages;
            self.history.drain(1..=remove_count);
        }
    }
}
```

### Word Normalization

```rust
pub fn normalize_word(word: &str) -> String {
    let word = word.trim();

    // Remove punctuation
    let word: String = word
        .chars()
        .filter(|c| c.is_alphabetic())
        .collect();

    // Take first word if multiple
    let word = word.split_whitespace().next().unwrap_or("");

    // Lowercase
    word.to_lowercase()
}
```

### Error Types

```rust
#[derive(Debug, thiserror::Error)]
pub enum PhraseError {
    #[error("LLM callback failed: {0}")]
    LlmFailed(String),

    #[error("JSON parse failed after 3 attempts: {0}")]
    ParseFailed(String),

    #[error("Empty response from LLM")]
    EmptyResponse,
}
```

### Relevant Source Tree

```
rust/
├── src/
│   └── games/
│       ├── mod.rs               # Add: mod phrase_generator;
│       └── phrase_generator.rs  # NEW: PhraseGenerator
```

### Testing

- Test file location: `rust/src/games/phrase_generator.rs` (inline tests)
- Use mock LLM callback for unit tests
- Test framework: Built-in Rust tests + tokio for async
- Run with: `cargo test --features game`

### Mock LLM for Testing

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn mock_llm_success(_messages: &[Message]) -> String {
        r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string()
    }

    fn mock_llm_invalid(_messages: &[Message]) -> String {
        "This is not JSON".to_string()
    }

    #[test]
    fn test_parse_valid_response() {
        // ...
    }
}
```

## Definition of Done

- [ ] `PhraseGenerator` creates and manages conversation history
- [ ] System prompt produces consistent JSON output
- [ ] JSON parsing handles valid and invalid responses
- [ ] Retry logic works correctly
- [ ] Context pruning keeps history bounded
- [ ] Word normalization handles edge cases
- [ ] All tests pass

---

## QA Results

### Test Coverage Summary

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 28 | 100% |
| **Unit tests** | 18 | 64% |
| **Integration tests** | 8 | 29% |
| **E2E tests** | 2 | 7% |
| **AC coverage** | 8/8 | 100% |

**Priority Distribution:**
- P0 (Critical): 10 scenarios - JSON parsing, retry logic, core generation
- P1 (High): 12 scenarios - Context management, normalization, error handling
- P2 (Medium): 6 scenarios - Edge cases, boundary conditions

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **LLM returns invalid JSON** | High | Retry logic (3 attempts) + structured error handling (INT-004, INT-005, UNIT-014) |
| **Context window overflow** | Medium | Pruning strategy keeps system prompt + last N rounds (UNIT-019–022, INT-008) |
| **Word normalization inconsistency** | Medium | Comprehensive edge case coverage for punctuation, casing, whitespace (UNIT-023–028) |
| **History state corruption during retry** | Medium | State mutation tests verify no duplicate user messages on retry (INT-007) |
| **Repeated phrases in session** | Low | Context history passed to LLM includes all prior phrases (INT-002, E2E-001) |

### Recommended Test Scenarios

**Critical Path (Must Implement First):**
1. `TEA-GAME-001.4-UNIT-011`: Valid JSON response parsed to `PhraseResult`
2. `TEA-GAME-001.4-UNIT-014`: Malformed JSON triggers parse error
3. `TEA-GAME-001.4-INT-004`: First attempt fails, second succeeds → returns success
4. `TEA-GAME-001.4-INT-005`: All 3 attempts fail → returns `PhraseError::ParseFailed`
5. `TEA-GAME-001.4-UNIT-023`: Case normalization (`"SUN"` → `"sun"`)

**Integration Flow:**
1. `TEA-GAME-001.4-INT-001`: History sequence: system → user → assistant → user → assistant
2. `TEA-GAME-001.4-INT-003`: `generate_phrase()` returns `Ok(PhraseResult)` on success
3. `TEA-GAME-001.4-INT-008`: Pruning occurs after each successful generation

**E2E Validation:**
1. `TEA-GAME-001.4-E2E-001`: Generate 5 consecutive phrases without repetition
2. `TEA-GAME-001.4-E2E-002`: Generate phrases until context pruning triggers

### Concerns / Observations

1. **No blockers identified** - Story is well-structured with clear implementation guidance
2. **Mock LLM callbacks required** - Unit/integration tests need proper mock setup for callback isolation
3. **E2E tests require LLM integration** - WASM browser environment or native Rust callback needed
4. **Async test support** - `tokio` test runtime required for `generate_phrase()` tests
5. **Feature flag dependency** - Tests run with `--features game` flag

### Test Implementation Guidance

```bash
# Run phrase generator tests
cargo test --features game phrase_generator

# Run with verbose output for debugging
cargo test --features game phrase_generator -- --nocapture
```

**Test Locations:**
- Unit tests: `rust/src/games/phrase_generator.rs` (inline `#[cfg(test)]` module)
- Integration tests: `rust/tests/phrase_generator_integration.rs`

---

**Review Date:** 2026-01-10
**Reviewer:** Quinn (Test Architect)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.2 | QA Results added with test design analysis | Quinn (QA Agent) |
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
