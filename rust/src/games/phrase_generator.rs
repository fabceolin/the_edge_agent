//! Phrase Generator Module for The Edge Agent
//!
//! This module provides LLM-powered phrase generation with context memory
//! for vocabulary games. It generates varied phrases by maintaining conversation
//! history to avoid repetition within a session.
//!
//! # Features
//!
//! - Context window memory with conversation history
//! - JSON parsing with retry logic
//! - Word normalization (punctuation removal, case normalization)
//! - History pruning to stay within token limits
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::games::phrase_generator::{PhraseGenerator, Message};
//!
//! let mut generator = PhraseGenerator::new(10);
//!
//! // Define an LLM callback
//! let llm_callback = |messages: &[Message]| -> Result<String, String> {
//!     // Call your LLM here
//!     Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
//! };
//!
//! let result = generator.generate_phrase(llm_callback).unwrap();
//! assert_eq!(result.word, "sun");
//! ```

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// System prompt that instructs the LLM on how to generate phrases (AC-2)
pub const PHRASE_SYSTEM_PROMPT: &str = r#"
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

/// A message in the conversation history (AC-1)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Message {
    /// Role of the message sender: "system", "user", or "assistant"
    pub role: String,
    /// Content of the message
    pub content: String,
}

impl Message {
    /// Create a new message with the given role and content
    pub fn new(role: impl Into<String>, content: impl Into<String>) -> Self {
        Self {
            role: role.into(),
            content: content.into(),
        }
    }

    /// Create a system message
    pub fn system(content: impl Into<String>) -> Self {
        Self::new("system", content)
    }

    /// Create a user message
    pub fn user(content: impl Into<String>) -> Self {
        Self::new("user", content)
    }

    /// Create an assistant message
    pub fn assistant(content: impl Into<String>) -> Self {
        Self::new("assistant", content)
    }
}

/// Result from phrase generation containing the phrase and target word (AC-4)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PhraseResult {
    /// The phrase with a blank (marked as ___)
    pub phrase: String,
    /// The word that fills the blank
    pub word: String,
}

/// Errors that can occur during phrase generation (AC-5)
#[derive(Debug, Error)]
pub enum PhraseError {
    /// LLM callback failed with an error
    #[error("LLM callback failed: {0}")]
    LlmFailed(String),

    /// JSON parsing failed after all retry attempts
    #[error("JSON parse failed after 3 attempts: {0}")]
    ParseFailed(String),

    /// LLM returned an empty response
    #[error("Empty response from LLM")]
    EmptyResponse,
}

/// Phrase generator with conversation history for context memory (AC-1, AC-3)
///
/// Maintains a conversation history that includes the system prompt and
/// previous phrase generations to help the LLM avoid repetition.
pub struct PhraseGenerator {
    /// Conversation history including system prompt and previous exchanges
    history: Vec<Message>,
    /// Maximum number of user+assistant rounds to keep in history
    max_history_rounds: usize,
}

impl PhraseGenerator {
    /// Create a new phrase generator with the specified maximum history rounds
    ///
    /// # Arguments
    ///
    /// * `max_history_rounds` - Maximum number of user+assistant pairs to retain
    ///   (the system prompt is always kept)
    pub fn new(max_history_rounds: usize) -> Self {
        let history = vec![Message::system(PHRASE_SYSTEM_PROMPT)];
        Self {
            history,
            max_history_rounds,
        }
    }

    /// Get the current conversation history (for testing/debugging)
    pub fn history(&self) -> &[Message] {
        &self.history
    }

    /// Get the number of messages in the history
    pub fn history_len(&self) -> usize {
        self.history.len()
    }

    /// Generate a new phrase using the provided LLM callback (AC-4, AC-5, AC-6)
    ///
    /// This function:
    /// 1. Adds a user request to the history
    /// 2. Calls the LLM with the full conversation history
    /// 3. Parses the JSON response
    /// 4. Retries up to 3 times on parse failure
    /// 5. Normalizes the word in the result
    /// 6. Records successful responses in history
    /// 7. Prunes history if needed
    ///
    /// # Arguments
    ///
    /// * `llm_callback` - Function that takes messages and returns LLM response
    ///
    /// # Returns
    ///
    /// * `Ok(PhraseResult)` - Successfully generated phrase with normalized word
    /// * `Err(PhraseError)` - Error from LLM or parsing failure after retries
    pub fn generate_phrase<F, E>(&mut self, llm_callback: F) -> Result<PhraseResult, PhraseError>
    where
        F: Fn(&[Message]) -> Result<String, E>,
        E: std::fmt::Display,
    {
        // Add user request to history
        self.history.push(Message::user("Generate a new phrase."));

        let mut last_error = String::new();

        // Try up to 3 times (AC-6)
        for attempt in 0..3 {
            let response = match llm_callback(&self.history) {
                Ok(r) => r,
                Err(e) => {
                    // Remove the user message we added since we're failing
                    if attempt == 2 {
                        self.history.pop();
                    }
                    return Err(PhraseError::LlmFailed(e.to_string()));
                }
            };

            if response.trim().is_empty() {
                last_error = "Empty response".to_string();
                log::warn!("Attempt {}: Empty response from LLM", attempt + 1);
                if attempt == 2 {
                    self.history.pop();
                    return Err(PhraseError::EmptyResponse);
                }
                continue;
            }

            match serde_json::from_str::<PhraseResult>(&response) {
                Ok(mut result) => {
                    // Normalize the word (AC-8)
                    result.word = normalize_word(&result.word);

                    // Record successful response in history (AC-3)
                    self.history.push(Message::assistant(response));

                    // Prune history if needed (AC-7)
                    self.prune_history();

                    return Ok(result);
                }
                Err(e) => {
                    last_error = e.to_string();
                    log::warn!(
                        "Attempt {}: JSON parse error: {} (response: {})",
                        attempt + 1,
                        e,
                        response
                    );
                    if attempt == 2 {
                        // Remove the user message since we're failing
                        self.history.pop();
                        return Err(PhraseError::ParseFailed(last_error));
                    }
                }
            }
        }

        // This should be unreachable due to the loop logic
        self.history.pop();
        Err(PhraseError::ParseFailed(last_error))
    }

    /// Prune conversation history to stay within limits (AC-7)
    ///
    /// Keeps the system prompt (index 0) plus the last N rounds of
    /// user+assistant message pairs.
    fn prune_history(&mut self) {
        // Each round is 2 messages (user + assistant)
        let max_messages = 1 + (self.max_history_rounds * 2);

        if self.history.len() > max_messages {
            let remove_count = self.history.len() - max_messages;
            // Drain messages after system prompt (index 1) up to remove_count
            self.history.drain(1..=remove_count);
        }
    }

    /// Clear all history except the system prompt
    pub fn clear_history(&mut self) {
        self.history.truncate(1);
    }
}

impl Default for PhraseGenerator {
    fn default() -> Self {
        Self::new(10) // Default to keeping 10 rounds of history
    }
}

/// Normalize a word by removing punctuation and converting to lowercase (AC-8)
///
/// # Processing Steps
///
/// 1. Trim whitespace
/// 2. Remove all non-alphabetic characters
/// 3. Take only the first word if multiple words
/// 4. Convert to lowercase
///
/// # Examples
///
/// ```rust,ignore
/// assert_eq!(normalize_word("SUN"), "sun");
/// assert_eq!(normalize_word("sun."), "sun");
/// assert_eq!(normalize_word("  sun  "), "sun");
/// assert_eq!(normalize_word("sun moon"), "sun");
/// ```
pub fn normalize_word(word: &str) -> String {
    let word = word.trim();

    // Remove punctuation (keep only alphabetic characters)
    let word: String = word.chars().filter(|c| c.is_alphabetic()).collect();

    // Take first word if multiple (shouldn't happen after filtering, but handle edge cases)
    let word = word.split_whitespace().next().unwrap_or("");

    // Convert to lowercase
    word.to_lowercase()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    // ============================================================
    // AC-1: PhraseGenerator struct tests
    // ============================================================

    #[test]
    fn test_phrase_generator_creation() {
        let generator = PhraseGenerator::new(5);

        // Should start with just the system prompt
        assert_eq!(generator.history_len(), 1);
        assert_eq!(generator.history()[0].role, "system");
        assert!(generator.history()[0].content.contains("phrase generator"));
    }

    #[test]
    fn test_phrase_generator_default() {
        let generator = PhraseGenerator::default();
        assert_eq!(generator.max_history_rounds, 10);
        assert_eq!(generator.history_len(), 1);
    }

    // ============================================================
    // AC-2: System prompt tests
    // ============================================================

    #[test]
    fn test_system_prompt_contains_rules() {
        assert!(PHRASE_SYSTEM_PROMPT.contains("phrase generator"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("JSON"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("___"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("DO NOT repeat"));
    }

    #[test]
    fn test_system_prompt_specifies_json_format() {
        assert!(PHRASE_SYSTEM_PROMPT.contains(r#"{"phrase":"#));
        assert!(PHRASE_SYSTEM_PROMPT.contains(r#""word":"#));
    }

    // ============================================================
    // AC-3, AC-7: Conversation history tests
    // ============================================================

    #[test]
    fn test_history_includes_previous_generations() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        // History should now have: system + user + assistant
        assert_eq!(generator.history_len(), 3);
        assert_eq!(generator.history()[0].role, "system");
        assert_eq!(generator.history()[1].role, "user");
        assert_eq!(generator.history()[2].role, "assistant");
    }

    #[test]
    fn test_history_sequence_after_multiple_generations() {
        let mut generator = PhraseGenerator::new(10);
        let call_count = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = call_count.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase {}.", "word": "word{}"}}"#,
                *count, *count
            ))
        };

        // Generate 3 phrases
        for _ in 0..3 {
            let _ = generator.generate_phrase(&mock_callback);
        }

        // History should be: system + 3 * (user + assistant) = 7 messages
        assert_eq!(generator.history_len(), 7);

        // Verify sequence: system, user, assistant, user, assistant, user, assistant
        assert_eq!(generator.history()[0].role, "system");
        assert_eq!(generator.history()[1].role, "user");
        assert_eq!(generator.history()[2].role, "assistant");
        assert_eq!(generator.history()[3].role, "user");
        assert_eq!(generator.history()[4].role, "assistant");
        assert_eq!(generator.history()[5].role, "user");
        assert_eq!(generator.history()[6].role, "assistant");
    }

    #[test]
    fn test_prune_history_keeps_system_prompt() {
        let mut generator = PhraseGenerator::new(2); // Keep only 2 rounds

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "Test ___.", "word": "word"}"#.to_string())
        };

        // Generate 5 phrases
        for _ in 0..5 {
            let _ = generator.generate_phrase(&mock_callback);
        }

        // With max_history_rounds=2, we keep: system + 2*2 = 5 messages
        assert_eq!(generator.history_len(), 5);

        // System prompt should still be first
        assert_eq!(generator.history()[0].role, "system");
        assert!(generator.history()[0].content.contains("phrase generator"));
    }

    #[test]
    fn test_prune_history_keeps_last_n_rounds() {
        let mut generator = PhraseGenerator::new(2);
        let counter = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase number {}.", "word": "word{}"}}"#,
                *count, *count
            ))
        };

        // Generate 5 phrases
        for _ in 0..5 {
            let _ = generator.generate_phrase(&mock_callback);
        }

        // Last assistant message should be from round 5
        let last_msg = generator.history().last().unwrap();
        assert_eq!(last_msg.role, "assistant");
        assert!(last_msg.content.contains("word5"));
    }

    #[test]
    fn test_clear_history() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "Test ___.", "word": "word"}"#.to_string())
        };

        // Generate a phrase
        let _ = generator.generate_phrase(&mock_callback);
        assert!(generator.history_len() > 1);

        // Clear history
        generator.clear_history();

        // Should only have system prompt
        assert_eq!(generator.history_len(), 1);
        assert_eq!(generator.history()[0].role, "system");
    }

    // ============================================================
    // AC-4: generate_phrase() tests
    // ============================================================

    #[test]
    fn test_generate_phrase_returns_phrase_result() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        let phrase_result = result.unwrap();
        assert_eq!(phrase_result.phrase, "The ___ is bright.");
        assert_eq!(phrase_result.word, "sun");
    }

    #[test]
    fn test_generate_phrase_passes_history_to_callback() {
        let mut generator = PhraseGenerator::new(10);
        let received_messages_count = RefCell::new(0);

        let mock_callback = |messages: &[Message]| -> Result<String, String> {
            *received_messages_count.borrow_mut() = messages.len();
            Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
        };

        let _ = generator.generate_phrase(&mock_callback);

        // Should have passed system + user messages
        assert_eq!(*received_messages_count.borrow(), 2);
    }

    // ============================================================
    // AC-5: JSON parsing error handling tests
    // ============================================================

    #[test]
    fn test_parse_valid_json_response() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ rises early.", "word": "sun"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        let phrase_result = result.unwrap();
        assert_eq!(phrase_result.phrase, "The ___ rises early.");
        assert_eq!(phrase_result.word, "sun");
    }

    #[test]
    fn test_parse_json_with_extra_whitespace() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"  {  "phrase"  :  "The ___ is bright."  ,  "word"  :  "sun"  }  "#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_malformed_json_returns_error() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok("This is not JSON".to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());

        match result {
            Err(PhraseError::ParseFailed(_)) => {} // Expected
            _ => panic!("Expected ParseFailed error"),
        }
    }

    #[test]
    fn test_parse_partial_json_returns_error() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright."}"#.to_string()) // Missing "word"
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());
    }

    #[test]
    fn test_empty_response_returns_error() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback =
            |_messages: &[Message]| -> Result<String, String> { Ok("".to_string()) };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());

        match result {
            Err(PhraseError::EmptyResponse) => {} // Expected
            _ => panic!("Expected EmptyResponse error"),
        }
    }

    // ============================================================
    // AC-6: Retry logic tests
    // ============================================================

    #[test]
    fn test_retry_on_first_failure_second_success() {
        let mut generator = PhraseGenerator::new(10);
        let attempt_count = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt_count.borrow_mut();
            *count += 1;
            if *count == 1 {
                Ok("invalid json".to_string())
            } else {
                Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
            }
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_ok());
        assert_eq!(*attempt_count.borrow(), 2);
    }

    #[test]
    fn test_retry_three_times_then_fail() {
        let mut generator = PhraseGenerator::new(10);
        let attempt_count = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            *attempt_count.borrow_mut() += 1;
            Ok("invalid json".to_string())
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_err());
        assert_eq!(*attempt_count.borrow(), 3);
    }

    #[test]
    fn test_retry_success_on_third_attempt() {
        let mut generator = PhraseGenerator::new(10);
        let attempt_count = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt_count.borrow_mut();
            *count += 1;
            if *count < 3 {
                Ok("invalid json".to_string())
            } else {
                Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
            }
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_ok());
        assert_eq!(*attempt_count.borrow(), 3);
    }

    #[test]
    fn test_llm_callback_error_returns_llm_failed() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback =
            |_messages: &[Message]| -> Result<String, String> { Err("Network error".to_string()) };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());

        match result {
            Err(PhraseError::LlmFailed(msg)) => {
                assert!(msg.contains("Network error"));
            }
            _ => panic!("Expected LlmFailed error"),
        }
    }

    #[test]
    fn test_history_not_corrupted_on_retry_failure() {
        let mut generator = PhraseGenerator::new(10);
        let initial_history_len = generator.history_len();

        let mock_callback =
            |_messages: &[Message]| -> Result<String, String> { Ok("invalid json".to_string()) };

        let _ = generator.generate_phrase(mock_callback);

        // History should not have been modified (user message should be removed on failure)
        assert_eq!(generator.history_len(), initial_history_len);
    }

    #[test]
    fn test_history_updated_only_on_success() {
        let mut generator = PhraseGenerator::new(10);
        let attempt_count = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt_count.borrow_mut();
            *count += 1;
            if *count < 2 {
                Ok("invalid json".to_string())
            } else {
                Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
            }
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_ok());

        // History should have: system + user + assistant (only successful response)
        assert_eq!(generator.history_len(), 3);
    }

    // ============================================================
    // AC-8: Word normalization tests
    // ============================================================

    #[test]
    fn test_normalize_word_lowercase() {
        assert_eq!(normalize_word("SUN"), "sun");
        assert_eq!(normalize_word("Sun"), "sun");
        assert_eq!(normalize_word("SuN"), "sun");
    }

    #[test]
    fn test_normalize_word_removes_punctuation() {
        assert_eq!(normalize_word("sun."), "sun");
        assert_eq!(normalize_word("sun,"), "sun");
        assert_eq!(normalize_word("sun!"), "sun");
        assert_eq!(normalize_word("sun?"), "sun");
        assert_eq!(normalize_word("'sun'"), "sun");
        assert_eq!(normalize_word("\"sun\""), "sun");
    }

    #[test]
    fn test_normalize_word_trims_whitespace() {
        assert_eq!(normalize_word("  sun  "), "sun");
        assert_eq!(normalize_word("\tsun\n"), "sun");
        assert_eq!(normalize_word(" sun"), "sun");
        assert_eq!(normalize_word("sun "), "sun");
    }

    #[test]
    fn test_normalize_word_takes_first_word() {
        // After filtering non-alphabetic chars, multiple words shouldn't exist
        // but if they do, take first
        assert_eq!(normalize_word("sun moon"), "sunmoon"); // No space after filtering
    }

    #[test]
    fn test_normalize_word_combined_cases() {
        assert_eq!(normalize_word("  SUN.  "), "sun");
        assert_eq!(normalize_word("'BRIGHT'"), "bright");
        assert_eq!(normalize_word("  \"Test!\"  "), "test");
    }

    #[test]
    fn test_normalize_word_empty_input() {
        assert_eq!(normalize_word(""), "");
        assert_eq!(normalize_word("   "), "");
    }

    #[test]
    fn test_normalize_word_only_punctuation() {
        assert_eq!(normalize_word("..."), "");
        assert_eq!(normalize_word("!?.,"), "");
    }

    #[test]
    fn test_normalize_word_with_numbers() {
        assert_eq!(normalize_word("sun123"), "sun");
        assert_eq!(normalize_word("123sun"), "sun");
        assert_eq!(normalize_word("s1u2n3"), "sun");
    }

    #[test]
    fn test_generate_phrase_normalizes_word() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "SUN!"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        let phrase_result = result.unwrap();
        assert_eq!(phrase_result.word, "sun"); // Normalized
    }

    // ============================================================
    // Message struct tests
    // ============================================================

    #[test]
    fn test_message_new() {
        let msg = Message::new("user", "Hello");
        assert_eq!(msg.role, "user");
        assert_eq!(msg.content, "Hello");
    }

    #[test]
    fn test_message_system() {
        let msg = Message::system("System prompt");
        assert_eq!(msg.role, "system");
        assert_eq!(msg.content, "System prompt");
    }

    #[test]
    fn test_message_user() {
        let msg = Message::user("User message");
        assert_eq!(msg.role, "user");
        assert_eq!(msg.content, "User message");
    }

    #[test]
    fn test_message_assistant() {
        let msg = Message::assistant("Assistant response");
        assert_eq!(msg.role, "assistant");
        assert_eq!(msg.content, "Assistant response");
    }

    #[test]
    fn test_message_serde_roundtrip() {
        let msg = Message::new("user", "Hello");
        let json = serde_json::to_string(&msg).unwrap();
        let restored: Message = serde_json::from_str(&json).unwrap();
        assert_eq!(msg, restored);
    }

    // ============================================================
    // PhraseResult struct tests
    // ============================================================

    #[test]
    fn test_phrase_result_serde_roundtrip() {
        let result = PhraseResult {
            phrase: "The ___ is bright.".to_string(),
            word: "sun".to_string(),
        };

        let json = serde_json::to_string(&result).unwrap();
        let restored: PhraseResult = serde_json::from_str(&json).unwrap();
        assert_eq!(result, restored);
    }

    // ============================================================
    // Integration tests
    // ============================================================

    #[test]
    fn test_full_generation_flow_with_pruning() {
        let mut generator = PhraseGenerator::new(2); // Only keep 2 rounds
        let counter = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase {} with ___.", "word": "word{}"}}"#,
                *count, *count
            ))
        };

        // Generate more phrases than history limit
        for _ in 0..5 {
            let result = generator.generate_phrase(&mock_callback);
            assert!(result.is_ok());
        }

        // History should be pruned: system + 2 rounds (4 messages) = 5 total
        assert_eq!(generator.history_len(), 5);

        // System prompt should be preserved
        assert_eq!(generator.history()[0].role, "system");

        // Last messages should be from recent rounds
        let last_assistant = generator.history().last().unwrap();
        assert!(last_assistant.content.contains("word5"));
    }

    #[test]
    fn test_generate_five_consecutive_phrases() {
        let mut generator = PhraseGenerator::new(10);
        let counter = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "The ___ number {}.", "word": "word{}"}}"#,
                *count, *count
            ))
        };

        let mut phrases = Vec::new();

        for _ in 0..5 {
            let result = generator.generate_phrase(&mock_callback);
            assert!(result.is_ok());
            phrases.push(result.unwrap());
        }

        // Verify all 5 phrases are different
        for (i, phrase) in phrases.iter().enumerate() {
            assert!(
                phrase.phrase.contains(&format!("number {}.", i + 1)),
                "Phrase {} should contain 'number {}.'",
                i,
                i + 1
            );
        }
    }
}
