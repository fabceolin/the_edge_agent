//! Integration tests for PhraseGenerator (TEA-GAME-001.4)
//!
//! These tests verify the full integration flow of the phrase generator,
//! including conversation history management, retry logic, and context pruning.

/// Tests are gated behind the `game` feature flag
#[cfg(feature = "game")]
mod integration_tests {
    use std::cell::RefCell;
    use the_edge_agent::games::phrase_generator::{
        normalize_word, Message, PhraseError, PhraseGenerator, PhraseResult, PHRASE_SYSTEM_PROMPT,
    };

    // ============================================================
    // INT-001: History sequence verification
    // ============================================================

    #[test]
    fn test_int_001_history_sequence_system_user_assistant() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        let history = generator.history();
        assert_eq!(history.len(), 3);
        assert_eq!(history[0].role, "system");
        assert_eq!(history[1].role, "user");
        assert_eq!(history[2].role, "assistant");
    }

    // ============================================================
    // INT-002: History includes all prior phrases (repetition avoidance)
    // ============================================================

    #[test]
    fn test_int_002_history_includes_all_prior_phrases() {
        let mut generator = PhraseGenerator::new(10);
        let counter = RefCell::new(0);

        let mock_callback = |messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;

            // Verify that on second call, history includes first phrase
            if *count == 2 {
                // Should have: system + user + assistant (from first call) + user (current)
                assert_eq!(messages.len(), 4);
                // Previous assistant message should be in history
                assert!(messages[2].content.contains("word1"));
            }

            Ok(format!(
                r#"{{"phrase": "Phrase {}.", "word": "word{}"}}"#,
                count, count
            ))
        };

        // Generate two phrases
        let _ = generator.generate_phrase(&mock_callback);
        let _ = generator.generate_phrase(&mock_callback);

        assert_eq!(*counter.borrow(), 2);
    }

    // ============================================================
    // INT-003: generate_phrase returns Ok(PhraseResult) on success
    // ============================================================

    #[test]
    fn test_int_003_generate_phrase_returns_ok_on_success() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ shines bright.", "word": "moon"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());

        let phrase_result = result.unwrap();
        assert_eq!(phrase_result.phrase, "The ___ shines bright.");
        assert_eq!(phrase_result.word, "moon");
    }

    // ============================================================
    // INT-004: First attempt fails, second succeeds → returns success
    // ============================================================

    #[test]
    fn test_int_004_retry_first_fails_second_succeeds() {
        let mut generator = PhraseGenerator::new(10);
        let attempt = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt.borrow_mut();
            *count += 1;

            if *count == 1 {
                Ok("not valid json".to_string())
            } else {
                Ok(r#"{"phrase": "The ___ is here.", "word": "cat"}"#.to_string())
            }
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_ok());
        assert_eq!(*attempt.borrow(), 2);

        let phrase_result = result.unwrap();
        assert_eq!(phrase_result.word, "cat");
    }

    // ============================================================
    // INT-005: All 3 attempts fail → returns PhraseError::ParseFailed
    // ============================================================

    #[test]
    fn test_int_005_three_failures_returns_parse_failed() {
        let mut generator = PhraseGenerator::new(10);
        let attempt = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt.borrow_mut();
            *count += 1;
            Ok("invalid json response".to_string())
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_err());
        assert_eq!(*attempt.borrow(), 3);

        match result {
            Err(PhraseError::ParseFailed(_)) => {} // Expected
            other => panic!("Expected ParseFailed, got {:?}", other),
        }
    }

    // ============================================================
    // INT-006: LLM callback error returns LlmFailed immediately
    // ============================================================

    #[test]
    fn test_int_006_llm_callback_error_returns_llm_failed() {
        let mut generator = PhraseGenerator::new(10);
        let attempt = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = attempt.borrow_mut();
            *count += 1;
            Err("API connection failed".to_string())
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_err());

        // Should fail on first attempt without retrying
        assert_eq!(*attempt.borrow(), 1);

        match result {
            Err(PhraseError::LlmFailed(msg)) => {
                assert!(msg.contains("API connection failed"));
            }
            other => panic!("Expected LlmFailed, got {:?}", other),
        }
    }

    // ============================================================
    // INT-007: State mutation - no duplicate user messages on retry
    // ============================================================

    #[test]
    fn test_int_007_no_duplicate_user_messages_on_retry() {
        let mut generator = PhraseGenerator::new(10);
        let attempt = RefCell::new(0);

        let mock_callback = |messages: &[Message]| -> Result<String, String> {
            let mut count = attempt.borrow_mut();
            *count += 1;

            // Count user messages in the provided messages
            let user_count = messages.iter().filter(|m| m.role == "user").count();

            // On each attempt, there should only be ONE user message for this generation
            // (Plus any from previous successful generations, which is 0 in this test)
            assert_eq!(user_count, 1, "Should only have one user message");

            if *count < 3 {
                Ok("invalid".to_string())
            } else {
                Ok(r#"{"phrase": "The ___ works.", "word": "test"}"#.to_string())
            }
        };

        let result = generator.generate_phrase(&mock_callback);
        assert!(result.is_ok());
    }

    // ============================================================
    // INT-008: Pruning occurs after each successful generation
    // ============================================================

    #[test]
    fn test_int_008_pruning_after_successful_generation() {
        let mut generator = PhraseGenerator::new(2); // Only keep 2 rounds
        let counter = RefCell::new(0);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase {}.", "word": "word{}"}}"#,
                count, count
            ))
        };

        // Generate more than max_history_rounds
        for _ in 0..5 {
            let result = generator.generate_phrase(&mock_callback);
            assert!(result.is_ok());
        }

        // History should be pruned to: system + 2 rounds (4 messages) = 5 total
        assert_eq!(generator.history_len(), 5);

        // System prompt must be preserved
        assert_eq!(generator.history()[0].role, "system");
    }

    // ============================================================
    // E2E-001: Generate 5 consecutive phrases without repetition
    // ============================================================

    #[test]
    fn test_e2e_001_five_consecutive_unique_phrases() {
        let mut generator = PhraseGenerator::new(10);
        let counter = RefCell::new(0);

        // Words that remain unique after normalization (just alphabetic chars, lowercase)
        let words = ["sun", "moon", "star", "cloud", "rain"];

        // Simulate LLM generating unique phrases each time
        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            let word = words[*count % words.len()];
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "The ___ phrase number {}.", "word": "{}"}}"#,
                count, word
            ))
        };

        let mut phrases: Vec<PhraseResult> = Vec::new();

        for _ in 0..5 {
            let result = generator.generate_phrase(&mock_callback);
            assert!(result.is_ok());
            phrases.push(result.unwrap());
        }

        // Verify all phrases are unique
        let phrase_strings: Vec<&str> = phrases.iter().map(|p| p.phrase.as_str()).collect();
        let unique_count = phrase_strings
            .iter()
            .collect::<std::collections::HashSet<_>>()
            .len();
        assert_eq!(unique_count, 5, "All 5 phrases should be unique");

        // Verify words are unique
        let words: Vec<&str> = phrases.iter().map(|p| p.word.as_str()).collect();
        let unique_words = words.iter().collect::<std::collections::HashSet<_>>().len();
        assert_eq!(unique_words, 5, "All 5 words should be unique");
    }

    // ============================================================
    // E2E-002: Generate until context pruning triggers
    // ============================================================

    #[test]
    fn test_e2e_002_generate_until_pruning_triggers() {
        let mut generator = PhraseGenerator::new(3); // Only keep 3 rounds
        let counter = RefCell::new(0);
        let max_messages_seen = RefCell::new(0usize);

        let mock_callback = |messages: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;

            // Track maximum messages seen (to verify pruning is working)
            let mut max = max_messages_seen.borrow_mut();
            if messages.len() > *max {
                *max = messages.len();
            }

            Ok(format!(
                r#"{{"phrase": "Generated phrase {}.", "word": "word{}"}}"#,
                count, count
            ))
        };

        // Generate 10 phrases (more than max_history_rounds)
        for _ in 0..10 {
            let result = generator.generate_phrase(&mock_callback);
            assert!(result.is_ok());
        }

        // History should be pruned: system + 3 rounds = 7 messages max
        assert!(
            generator.history_len() <= 7,
            "History should be pruned to 7 or fewer messages"
        );

        // Maximum messages seen during generation should have been bounded
        // (system + 3 rounds + 1 new user = 8 max before pruning kicks in)
        assert!(
            *max_messages_seen.borrow() <= 8,
            "Max messages seen should be bounded by pruning"
        );
    }

    // ============================================================
    // Word normalization integration tests
    // ============================================================

    #[test]
    fn test_normalize_word_integration_uppercase() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "SUN"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().word, "sun"); // Normalized to lowercase
    }

    #[test]
    fn test_normalize_word_integration_punctuation() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "sun!"}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().word, "sun"); // Punctuation removed
    }

    #[test]
    fn test_normalize_word_integration_whitespace() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "  sun  "}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().word, "sun"); // Whitespace trimmed
    }

    // ============================================================
    // Error handling integration tests
    // ============================================================

    #[test]
    fn test_empty_response_handling() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            Ok("   ".to_string()) // Empty/whitespace only
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());

        match result {
            Err(PhraseError::EmptyResponse) => {} // Expected
            other => panic!("Expected EmptyResponse, got {:?}", other),
        }
    }

    #[test]
    fn test_partial_json_handling() {
        let mut generator = PhraseGenerator::new(10);

        let mock_callback = |_messages: &[Message]| -> Result<String, String> {
            // Missing "word" field
            Ok(r#"{"phrase": "The ___ is bright."}"#.to_string())
        };

        let result = generator.generate_phrase(mock_callback);
        assert!(result.is_err());

        match result {
            Err(PhraseError::ParseFailed(_)) => {} // Expected
            other => panic!("Expected ParseFailed, got {:?}", other),
        }
    }

    // ============================================================
    // Message struct tests
    // ============================================================

    #[test]
    fn test_message_constructors() {
        let system = Message::system("System content");
        assert_eq!(system.role, "system");
        assert_eq!(system.content, "System content");

        let user = Message::user("User content");
        assert_eq!(user.role, "user");
        assert_eq!(user.content, "User content");

        let assistant = Message::assistant("Assistant content");
        assert_eq!(assistant.role, "assistant");
        assert_eq!(assistant.content, "Assistant content");
    }

    #[test]
    fn test_message_serde() {
        let msg = Message::new("user", "Test message");
        let json = serde_json::to_string(&msg).unwrap();
        let restored: Message = serde_json::from_str(&json).unwrap();
        assert_eq!(msg, restored);
    }

    // ============================================================
    // PhraseResult struct tests
    // ============================================================

    #[test]
    fn test_phrase_result_serde() {
        let result = PhraseResult {
            phrase: "The ___ is shining.".to_string(),
            word: "sun".to_string(),
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("phrase"));
        assert!(json.contains("word"));

        let restored: PhraseResult = serde_json::from_str(&json).unwrap();
        assert_eq!(result, restored);
    }

    // ============================================================
    // System prompt verification
    // ============================================================

    #[test]
    fn test_system_prompt_content() {
        assert!(PHRASE_SYSTEM_PROMPT.contains("word-guessing game"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("___"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("JSON"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("DO NOT repeat"));
        assert!(PHRASE_SYSTEM_PROMPT.contains("5-12 words"));
    }

    // ============================================================
    // normalize_word function tests
    // ============================================================

    #[test]
    fn test_normalize_word_edge_cases() {
        assert_eq!(normalize_word(""), "");
        assert_eq!(normalize_word("   "), "");
        assert_eq!(normalize_word("..."), "");
        assert_eq!(normalize_word("123"), "");
        assert_eq!(normalize_word("a1b2c3"), "abc");
    }
}
