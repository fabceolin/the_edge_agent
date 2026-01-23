# Story TEA-GAME-001.4: LLM Phrase Generation with Context Memory

## Status

Done

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

- [x] Create `rust/src/games/phrase_generator.rs` module (AC-1)
  - [x] Add `mod phrase_generator;` to `rust/src/games/mod.rs`
  - [x] Define `PhraseGenerator` struct with conversation history
  - [x] Define `Message` struct for conversation entries
  - [x] Define `PhraseResult` struct for output

- [x] Implement system prompt (AC-2)
  - [x] Create `PHRASE_SYSTEM_PROMPT` constant
  - [x] Include all generation rules
  - [x] Specify JSON output format

- [x] Implement conversation history management (AC-3, AC-7)
  - [x] Store messages as `Vec<Message>`
  - [x] Add assistant response after each generation
  - [x] Implement `prune_history(max_rounds: usize)`
  - [x] Keep system prompt + last N rounds

- [x] Implement `generate_phrase()` (AC-4, AC-5, AC-6)
  - [x] Build messages array with history
  - [x] Call LLM callback with messages
  - [x] Parse JSON response with `serde_json`
  - [x] Retry on parse failure (up to 3 times)
  - [x] Return `Result<PhraseResult, PhraseError>`

- [x] Implement word normalization (AC-8)
  - [x] `normalize_word(word: &str) -> String`
  - [x] Remove punctuation
  - [x] Convert to lowercase
  - [x] Trim whitespace
  - [x] Handle multi-word responses (take first word)

- [x] Write tests
  - [x] Test JSON parsing with valid responses
  - [x] Test JSON parsing with malformed responses
  - [x] Test retry logic
  - [x] Test context pruning
  - [x] Test word normalization edge cases

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

- [x] `PhraseGenerator` creates and manages conversation history
- [x] System prompt produces consistent JSON output
- [x] JSON parsing handles valid and invalid responses
- [x] Retry logic works correctly
- [x] Context pruning keeps history bounded
- [x] Word normalization handles edge cases
- [x] All tests pass

## File List

| File | Status | Description |
|------|--------|-------------|
| `rust/src/games/phrase_generator.rs` | Modified | PhraseGenerator module with conversation history, system prompt, retry logic, and word normalization |
| `rust/src/games/mod.rs` | Modified | Added `mod phrase_generator;` export |
| `rust/tests/test_phrase_generator.rs` | New | Integration tests for phrase generator (20 tests) |

## Test Results

**Unit Tests:** 39 passed (inline in `phrase_generator.rs`)
**Integration Tests:** 20 passed (`tests/test_phrase_generator.rs`)
**Total:** 59 tests passing

Run with: `cargo test --features game phrase_generator`

---

## QA Results

### Final Review Summary

**Gate Status:** ✅ **PASS**

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| **Unit tests** | ≥20 | 39 | ✅ |
| **Integration tests** | ≥8 | 20 | ✅ |
| **AC coverage** | 8/8 | 8/8 | ✅ |
| **All tests passing** | Yes | Yes | ✅ |

### Risk Assessment

**Risk Level: LOW** - Well-defined scope with comprehensive test coverage.

| Risk | Severity | Mitigation | Verified |
|------|----------|------------|----------|
| **LLM returns invalid JSON** | High | Retry logic (3 attempts) + structured error handling | ✅ `test_int_004`, `test_int_005` |
| **Context window overflow** | Medium | Pruning strategy keeps system prompt + last N rounds | ✅ `test_prune_history_*`, `test_int_008` |
| **Word normalization inconsistency** | Medium | Comprehensive edge case coverage | ✅ 10 normalization tests |
| **History state corruption during retry** | Medium | State mutation tests verify no duplicate user messages | ✅ `test_int_007` |
| **Repeated phrases in session** | Low | Context history passed to LLM includes all prior phrases | ✅ `test_int_002`, `test_e2e_001` |

### Requirements Traceability

| AC | Description | Implementation | Test Coverage |
|----|-------------|----------------|---------------|
| AC-1 | `PhraseGenerator` maintains conversation history | `phrase_generator.rs:116-136` | `test_phrase_generator_creation`, `test_history_includes_previous_generations` |
| AC-2 | System prompt instructs LLM to generate phrase + word in JSON | `phrase_generator.rs:35-51` | `test_system_prompt_contains_rules`, `test_system_prompt_specifies_json_format` |
| AC-3 | Conversation history includes all previously generated phrases | `phrase_generator.rs:205-206` | `test_int_002`, `test_history_sequence_after_multiple_generations` |
| AC-4 | `generate_phrase()` returns `PhraseResult` | `phrase_generator.rs:167-233` | `test_generate_phrase_returns_phrase_result`, `test_int_003` |
| AC-5 | JSON parsing with error handling | `phrase_generator.rs:200-227` | `test_parse_*` tests (6 tests) |
| AC-6 | Retry logic (up to 3 attempts) | `phrase_generator.rs:178-228` | `test_retry_*` tests (4 tests), `test_int_004`, `test_int_005` |
| AC-7 | Context window pruning strategy | `phrase_generator.rs:239-248` | `test_prune_history_*` tests (4 tests), `test_int_008`, `test_e2e_002` |
| AC-8 | Word extraction handles edge cases | `phrase_generator.rs:279-290` | `test_normalize_word_*` tests (9 tests) |

### Code Quality Assessment

| Aspect | Rating | Notes |
|--------|--------|-------|
| **Documentation** | ✅ Excellent | Module-level docs, function docs with examples, inline comments for each AC |
| **Error handling** | ✅ Excellent | `PhraseError` enum with `thiserror`, proper Result propagation |
| **Type safety** | ✅ Excellent | Strong typing with `PhraseResult`, `Message`, serde derive macros |
| **Test isolation** | ✅ Excellent | Mock callbacks for all tests, no external dependencies |
| **Code organization** | ✅ Good | Clear separation: structs, constants, impl blocks, tests |

### Test Architecture Assessment

**Unit Tests (39 passing):** `rust/src/games/phrase_generator.rs`
- AC-1: 4 tests (struct creation, default, history management)
- AC-2: 2 tests (system prompt content)
- AC-3/AC-7: 5 tests (history, pruning)
- AC-4: 2 tests (generate_phrase return)
- AC-5: 5 tests (JSON parsing)
- AC-6: 7 tests (retry logic)
- AC-8: 9 tests (word normalization)
- Struct tests: 5 tests (Message, PhraseResult serde)

**Integration Tests (20 passing):** `rust/tests/test_phrase_generator.rs`
- INT-001 to INT-008: Core integration scenarios
- E2E-001, E2E-002: End-to-end validation
- Normalization integration: 3 tests
- Error handling: 2 tests
- Struct serde: 4 tests

### NFR Validation

| NFR | Requirement | Status |
|-----|-------------|--------|
| **Performance** | Pruning limits history to bounded size | ✅ Verified in `test_e2e_002` |
| **Reliability** | Retry logic handles transient failures | ✅ 3-attempt retry verified |
| **Security** | No arbitrary code execution | ✅ Uses serde_json for parsing |
| **Maintainability** | Comprehensive test coverage | ✅ 59 tests total |

### Standards Compliance

- [x] Rust 2021 edition
- [x] `thiserror` for error types
- [x] `serde` for serialization
- [x] Feature flag (`game`) for conditional compilation
- [x] Doc comments with `///` and `//!`
- [x] `#[cfg(test)]` for inline tests

### Verified Test Execution

```
$ cargo test --features game phrase_generator
running 39 tests
test result: ok. 39 passed; 0 failed

$ cargo test --features game --test test_phrase_generator
running 20 tests
test result: ok. 20 passed; 0 failed
```

**Total: 59 tests passing**

### Recommendations

1. **Future Enhancement:** Consider adding async support for `generate_phrase()` when WASM LLM integration is needed
2. **Monitoring:** Add Opik spans for phrase generation metrics (addressed in TEA-GAME-001.8)
3. **Documentation:** System prompt could be externalized to YAML for easier iteration

---

**Review Date:** 2026-01-22
**Reviewer:** Claude Opus 4.5 (QA Agent)
**Gate Status:** PASS

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-22 | 1.0 | Implementation complete - 59 tests passing, all ACs met | Claude Opus 4.5 (Dev Agent) |
| 2026-01-10 | 0.2 | QA Results added with test design analysis | Quinn (QA Agent) |
| 2026-01-10 | 0.1 | Initial story creation | Sarah (PO Agent) |
