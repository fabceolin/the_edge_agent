//! Game Engine Module for The Edge Agent
//!
//! This module provides game state management, scoring, and difficulty adjustment
//! for educational vocabulary games.
//!
//! # Features
//!
//! - `GameSession` for tracking player progress and statistics
//! - `GameRound` for individual round data including choices and timing
//! - Scoring algorithms with weighted difficulty consideration
//! - Adaptive difficulty adjustment based on rolling performance
//!
//! # Example
//!
//! ```rust
//! use the_edge_agent::games::{GameSession, GameRound, adjust_difficulty};
//!
//! let mut session = GameSession::default();
//! session.username = "SwiftFox42".to_string();
//!
//! // Simulate some correct answers to increase difficulty
//! for _ in 0..5 {
//!     session.recent_answers.push(true);
//! }
//!
//! // Adjust difficulty based on recent performance
//! adjust_difficulty(&mut session, 5);
//! assert!(session.current_difficulty > 0.5); // Difficulty increased
//! ```

use rand::Rng;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[cfg(feature = "game-duckdb")]
pub mod db;

#[cfg(feature = "game-duckdb")]
pub mod embeddings;

#[cfg(feature = "game-duckdb")]
pub mod engine;

// Re-export embedding types for convenience
#[cfg(feature = "game-duckdb")]
pub use embeddings::{difficulty_to_similarity_range, Difficulty, EmbeddingSearch};

// Re-export engine types for convenience (TEA-GAME-001.5)
#[cfg(feature = "game-duckdb")]
pub use engine::{
    AnswerResult, GameEngine, GameEngineConfig, GameError, LeaderboardResult, SessionInfo,
};

/// Opik integration for game tracing (TEA-GAME-001.8)
pub mod opik;

/// Phrase generation with LLM integration (TEA-GAME-001.4)
pub mod phrase_generator;

// Re-export key Opik types for convenience
pub use opik::{
    clear_game_opik_handler, configure_game_opik, get_game_opik_config, has_game_opik_handler,
    is_game_opik_enabled, send_game_span, set_game_opik_handler, GameOpikConfig, GameSpanType,
    OpikGameSpan,
};

/// Adjectives for username generation (AC-3)
const ADJECTIVES: &[&str] = &[
    "Swift", "Brave", "Clever", "Quick", "Wise", "Bold", "Sharp", "Keen", "Nimble", "Bright",
];

/// Animals for username generation (AC-3)
const ANIMALS: &[&str] = &[
    "Fox", "Owl", "Wolf", "Raven", "Tiger", "Eagle", "Hawk", "Bear", "Deer", "Lion",
];

/// Minimum difficulty bound (AC-6)
pub const MIN_DIFFICULTY: f64 = 0.1;

/// Maximum difficulty bound (AC-6)
pub const MAX_DIFFICULTY: f64 = 0.95;

/// Difficulty adjustment step size
pub const DIFFICULTY_STEP: f64 = 0.05;

/// Accuracy threshold above which difficulty increases
pub const ACCURACY_INCREASE_THRESHOLD: f64 = 0.8;

/// Accuracy threshold below which difficulty decreases
pub const ACCURACY_DECREASE_THRESHOLD: f64 = 0.4;

/// A game session tracking player progress and statistics.
///
/// # Fields
///
/// - `id`: Unique session identifier (UUID v4)
/// - `username`: Player's display name (e.g., "SwiftFox42")
/// - `total_answers`: Total number of answers submitted
/// - `correct_answers`: Number of correct answers
/// - `current_difficulty`: Current difficulty level (0.1 to 0.95)
/// - `sum_difficulty`: Sum of all difficulty levels for average calculation
/// - `recent_answers`: Rolling window of recent answer correctness for difficulty adjustment
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GameSession {
    /// Unique session identifier
    pub id: String,

    /// Player's display name
    pub username: String,

    /// Total number of answers submitted
    pub total_answers: u32,

    /// Number of correct answers
    pub correct_answers: u32,

    /// Current difficulty level (bounded to 0.1 - 0.95)
    pub current_difficulty: f64,

    /// Sum of difficulty levels across all rounds (for average calculation)
    pub sum_difficulty: f64,

    /// Rolling window of recent answers for difficulty adjustment
    /// True = correct, False = incorrect
    pub recent_answers: Vec<bool>,
}

impl Default for GameSession {
    fn default() -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            username: String::new(),
            total_answers: 0,
            correct_answers: 0,
            current_difficulty: 0.5, // Start at medium difficulty
            sum_difficulty: 0.0,
            recent_answers: Vec::new(),
        }
    }
}

impl GameSession {
    /// Create a new game session with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new game session with a specific username
    pub fn with_username(username: impl Into<String>) -> Self {
        Self {
            username: username.into(),
            ..Self::default()
        }
    }
}

/// A single game round containing the phrase, choices, and result.
///
/// # Fields
///
/// - `phrase`: The sentence or phrase containing the target word
/// - `choices`: Vector of 5 word choices for the player
/// - `correct_word`: The correct answer
/// - `selected_word`: The word the player selected (None if not yet answered)
/// - `is_correct`: Whether the selection was correct (None if not yet answered)
/// - `response_time_ms`: Time taken to answer in milliseconds (None if not yet answered)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GameRound {
    /// The sentence or phrase containing the target word
    pub phrase: String,

    /// Vector of 5 word choices for the player to select from
    pub choices: Vec<String>,

    /// The correct word (answer)
    pub correct_word: String,

    /// The word the player selected (None if not yet answered)
    pub selected_word: Option<String>,

    /// Whether the player's selection was correct (None if not yet answered)
    pub is_correct: Option<bool>,

    /// Time taken to answer in milliseconds (None if not yet answered)
    pub response_time_ms: Option<u64>,
}

impl Default for GameRound {
    fn default() -> Self {
        Self {
            phrase: String::new(),
            choices: Vec::with_capacity(5),
            correct_word: String::new(),
            selected_word: None,
            is_correct: None,
            response_time_ms: None,
        }
    }
}

impl GameRound {
    /// Create a new game round with the given phrase, choices, and correct word
    pub fn new(
        phrase: impl Into<String>,
        choices: Vec<String>,
        correct_word: impl Into<String>,
    ) -> Self {
        Self {
            phrase: phrase.into(),
            choices,
            correct_word: correct_word.into(),
            ..Self::default()
        }
    }

    /// Record the player's answer for this round
    pub fn record_answer(&mut self, selected_word: impl Into<String>, response_time_ms: u64) {
        let selected = selected_word.into();
        self.is_correct = Some(selected == self.correct_word);
        self.selected_word = Some(selected);
        self.response_time_ms = Some(response_time_ms);
    }
}

/// Adjust difficulty based on rolling accuracy over a window of recent answers (AC-5, AC-6).
///
/// This function implements adaptive difficulty adjustment using a rolling window approach:
/// - If accuracy > 80%: increase difficulty by 0.05
/// - If accuracy < 40%: decrease difficulty by 0.05
/// - Otherwise: keep difficulty unchanged
///
/// The result is clamped to the range [0.1, 0.95] (AC-6).
///
/// # Arguments
///
/// * `session` - Mutable reference to the game session to adjust
/// * `window_size` - Number of recent answers to consider for rolling accuracy
///
/// # Example
///
/// ```rust
/// use the_edge_agent::games::{GameSession, adjust_difficulty};
///
/// let mut session = GameSession::default();
/// session.current_difficulty = 0.5;
///
/// // Add 5 correct answers (100% accuracy)
/// session.recent_answers = vec![true, true, true, true, true];
///
/// // With >80% accuracy, difficulty should increase
/// adjust_difficulty(&mut session, 5);
/// assert!((session.current_difficulty - 0.55).abs() < f64::EPSILON);
/// ```
pub fn adjust_difficulty(session: &mut GameSession, window_size: usize) {
    // If no recent answers, no adjustment needed
    if session.recent_answers.is_empty() || window_size == 0 {
        return;
    }

    // Calculate rolling accuracy over the window
    let window_start = session.recent_answers.len().saturating_sub(window_size);
    let window = &session.recent_answers[window_start..];

    let correct_count = window.iter().filter(|&&correct| correct).count();
    let rolling_accuracy = correct_count as f64 / window.len() as f64;

    // Adjust difficulty based on rolling accuracy
    if rolling_accuracy > ACCURACY_INCREASE_THRESHOLD {
        // Player is doing well, increase difficulty
        session.current_difficulty += DIFFICULTY_STEP;
    } else if rolling_accuracy < ACCURACY_DECREASE_THRESHOLD {
        // Player is struggling, decrease difficulty
        session.current_difficulty -= DIFFICULTY_STEP;
    }
    // If accuracy is between 40% and 80%, no change

    // Clamp difficulty to valid range [0.1, 0.95] (AC-6)
    session.current_difficulty = session
        .current_difficulty
        .clamp(MIN_DIFFICULTY, MAX_DIFFICULTY);
}

/// Generate a random username in the pattern `{Adjective}{Animal}{Number}` (AC-3).
///
/// Examples: "SwiftFox42", "BraveOwl07", "CleverWolf99"
///
/// # Format
///
/// - Adjective: One of 10 predefined adjectives (Swift, Brave, etc.)
/// - Animal: One of 10 predefined animals (Fox, Owl, etc.)
/// - Number: Two-digit number from 00-99
///
/// # Example
///
/// ```rust
/// use the_edge_agent::games::generate_username;
///
/// let username = generate_username();
/// // e.g., "SwiftFox42"
/// assert!(username.len() >= 6); // Minimum: "Bold" + "Fox" + "00" = 9 chars
/// ```
pub fn generate_username() -> String {
    let mut rng = rand::thread_rng();
    let adjective = ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let animal = ANIMALS[rng.gen_range(0..ANIMALS.len())];
    let number: u8 = rng.gen_range(0..100);
    format!("{}{}{:02}", adjective, animal, number)
}

/// Calculate the player's score based on session statistics (AC-4).
///
/// The score formula is:
/// ```text
/// score = accuracy * avg_difficulty * answer_factor
/// ```
///
/// Where:
/// - `accuracy = correct_answers / total_answers`
/// - `avg_difficulty = sum_difficulty / total_answers`
/// - `answer_factor = min(1.0, log2(total_answers + 1) / log2(50))`
///
/// The answer_factor rewards playing more rounds (up to 50 answers), after which
/// it caps at 1.0.
///
/// # Arguments
///
/// * `session` - Reference to the game session to calculate score for
///
/// # Returns
///
/// The calculated score (0.0 to ~1.0), or 0.0 if no answers have been submitted.
///
/// # Example
///
/// ```rust
/// use the_edge_agent::games::{GameSession, calculate_score};
///
/// let mut session = GameSession::default();
/// session.total_answers = 10;
/// session.correct_answers = 8;
/// session.sum_difficulty = 5.0; // avg difficulty = 0.5
///
/// let score = calculate_score(&session);
/// // accuracy = 0.8, avg_diff = 0.5, answer_factor ≈ 0.61
/// // score ≈ 0.8 * 0.5 * 0.61 ≈ 0.244
/// assert!(score > 0.0 && score < 1.0);
/// ```
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

#[cfg(test)]
mod tests {
    use super::*;

    // ============================================================
    // AC-1: GameSession struct tests
    // ============================================================

    #[test]
    fn test_game_session_has_required_fields() {
        // AC-1: GameSession struct with id, username, total_answers, correct_answers,
        // current_difficulty, sum_difficulty
        let session = GameSession::default();

        // Verify all required fields exist and have appropriate types
        assert!(
            !session.id.is_empty(),
            "id should be a non-empty UUID string"
        );
        assert!(
            session.username.is_empty(),
            "username should default to empty"
        );
        assert_eq!(session.total_answers, 0);
        assert_eq!(session.correct_answers, 0);
        assert!((session.current_difficulty - 0.5).abs() < f64::EPSILON);
        assert!((session.sum_difficulty - 0.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_game_session_default_values() {
        let session = GameSession::default();

        // Default difficulty should be 0.5 (medium)
        assert!((session.current_difficulty - 0.5).abs() < f64::EPSILON);

        // Recent answers should be empty
        assert!(session.recent_answers.is_empty());

        // ID should be a valid UUID v4 format (36 chars with hyphens)
        assert_eq!(session.id.len(), 36);
        assert!(session.id.contains('-'));
    }

    #[test]
    fn test_game_session_new() {
        let session = GameSession::new();
        assert!(!session.id.is_empty());
        assert_eq!(session.total_answers, 0);
    }

    #[test]
    fn test_game_session_with_username() {
        let session = GameSession::with_username("SwiftFox42");
        assert_eq!(session.username, "SwiftFox42");
        assert!(!session.id.is_empty());
    }

    #[test]
    fn test_game_session_serde_roundtrip() {
        let session = GameSession {
            id: "test-id-123".to_string(),
            username: "TestPlayer".to_string(),
            total_answers: 10,
            correct_answers: 8,
            current_difficulty: 0.7,
            sum_difficulty: 7.5,
            recent_answers: vec![true, true, false, true],
        };

        // Serialize to JSON
        let json = serde_json::to_string(&session).expect("Serialization should succeed");

        // Deserialize back
        let restored: GameSession =
            serde_json::from_str(&json).expect("Deserialization should succeed");

        assert_eq!(session, restored);
    }

    // ============================================================
    // AC-2: GameRound struct tests
    // ============================================================

    #[test]
    fn test_game_round_has_required_fields() {
        // AC-2: GameRound struct with phrase, choices (5 words), correct_word,
        // selected_word, is_correct, response_time_ms
        let round = GameRound::default();

        // Verify all required fields exist
        assert!(round.phrase.is_empty());
        assert!(round.choices.is_empty());
        assert!(round.correct_word.is_empty());
        assert!(round.selected_word.is_none());
        assert!(round.is_correct.is_none());
        assert!(round.response_time_ms.is_none());
    }

    #[test]
    fn test_game_round_new() {
        let choices = vec![
            "fast".to_string(),
            "slow".to_string(),
            "quick".to_string(),
            "lazy".to_string(),
            "red".to_string(),
        ];

        let round = GameRound::new("The quick brown fox", choices.clone(), "quick");

        assert_eq!(round.phrase, "The quick brown fox");
        assert_eq!(round.choices, choices);
        assert_eq!(round.correct_word, "quick");
        assert!(round.selected_word.is_none());
        assert!(round.is_correct.is_none());
        assert!(round.response_time_ms.is_none());
    }

    #[test]
    fn test_game_round_record_correct_answer() {
        let mut round = GameRound::new(
            "The quick brown fox",
            vec![
                "fast".into(),
                "slow".into(),
                "quick".into(),
                "lazy".into(),
                "red".into(),
            ],
            "quick",
        );

        round.record_answer("quick", 1500);

        assert_eq!(round.selected_word, Some("quick".to_string()));
        assert_eq!(round.is_correct, Some(true));
        assert_eq!(round.response_time_ms, Some(1500));
    }

    #[test]
    fn test_game_round_record_incorrect_answer() {
        let mut round = GameRound::new(
            "The quick brown fox",
            vec![
                "fast".into(),
                "slow".into(),
                "quick".into(),
                "lazy".into(),
                "red".into(),
            ],
            "quick",
        );

        round.record_answer("fast", 2000);

        assert_eq!(round.selected_word, Some("fast".to_string()));
        assert_eq!(round.is_correct, Some(false));
        assert_eq!(round.response_time_ms, Some(2000));
    }

    #[test]
    fn test_game_round_serde_roundtrip() {
        let round = GameRound {
            phrase: "The quick brown fox".to_string(),
            choices: vec![
                "fast".into(),
                "slow".into(),
                "quick".into(),
                "lazy".into(),
                "red".into(),
            ],
            correct_word: "quick".to_string(),
            selected_word: Some("quick".to_string()),
            is_correct: Some(true),
            response_time_ms: Some(1500),
        };

        // Serialize to JSON
        let json = serde_json::to_string(&round).expect("Serialization should succeed");

        // Deserialize back
        let restored: GameRound =
            serde_json::from_str(&json).expect("Deserialization should succeed");

        assert_eq!(round, restored);
    }

    #[test]
    fn test_game_round_with_5_choices() {
        // AC-2 specifies exactly 5 choices
        let choices = vec![
            "word1".to_string(),
            "word2".to_string(),
            "word3".to_string(),
            "word4".to_string(),
            "word5".to_string(),
        ];

        let round = GameRound::new("Test phrase", choices, "word3");

        assert_eq!(round.choices.len(), 5);
    }

    // ============================================================
    // AC-5: adjust_difficulty() tests
    // ============================================================

    #[test]
    fn test_adjust_difficulty_increase_on_high_accuracy() {
        // AC-5: If accuracy > 0.8: increase difficulty by 0.05
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 5/5 correct = 100% accuracy (> 80%)
        session.recent_answers = vec![true, true, true, true, true];

        adjust_difficulty(&mut session, 5);

        assert!(
            (session.current_difficulty - 0.55).abs() < f64::EPSILON,
            "Expected 0.55, got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_decrease_on_low_accuracy() {
        // AC-5: If accuracy < 0.4: decrease difficulty by 0.05
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 1/5 correct = 20% accuracy (< 40%)
        session.recent_answers = vec![true, false, false, false, false];

        adjust_difficulty(&mut session, 5);

        assert!(
            (session.current_difficulty - 0.45).abs() < f64::EPSILON,
            "Expected 0.45, got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_no_change_on_medium_accuracy() {
        // AC-5: No change when accuracy is between 40% and 80%
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 3/5 correct = 60% accuracy (between 40% and 80%)
        session.recent_answers = vec![true, true, true, false, false];

        adjust_difficulty(&mut session, 5);

        assert!(
            (session.current_difficulty - 0.5).abs() < f64::EPSILON,
            "Expected 0.5 (no change), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_exactly_80_percent_no_increase() {
        // Boundary test: exactly 80% should NOT increase (> not >=)
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 4/5 correct = 80% accuracy (not > 80%)
        session.recent_answers = vec![true, true, true, true, false];

        adjust_difficulty(&mut session, 5);

        assert!(
            (session.current_difficulty - 0.5).abs() < f64::EPSILON,
            "Expected 0.5 (no change at exactly 80%), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_exactly_40_percent_no_decrease() {
        // Boundary test: exactly 40% should NOT decrease (< not <=)
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 2/5 correct = 40% accuracy (not < 40%)
        session.recent_answers = vec![true, true, false, false, false];

        adjust_difficulty(&mut session, 5);

        assert!(
            (session.current_difficulty - 0.5).abs() < f64::EPSILON,
            "Expected 0.5 (no change at exactly 40%), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_rolling_window() {
        // AC-5: Uses rolling accuracy over window
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // 10 answers total, but window_size=5, so only last 5 count
        // First 5: all wrong, Last 5: all correct
        session.recent_answers = vec![
            false, false, false, false, false, // ignored
            true, true, true, true, true, // window: 100%
        ];

        adjust_difficulty(&mut session, 5);

        // Should increase because last 5 are 100% correct
        assert!(
            (session.current_difficulty - 0.55).abs() < f64::EPSILON,
            "Expected 0.55 (window considers only last 5), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_window_larger_than_answers() {
        // Window size larger than available answers
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // Only 3 answers, but window is 10
        session.recent_answers = vec![true, true, true]; // 100%

        adjust_difficulty(&mut session, 10);

        // Should use all 3 answers (100% accuracy)
        assert!(
            (session.current_difficulty - 0.55).abs() < f64::EPSILON,
            "Expected 0.55, got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_empty_answers() {
        // No answers yet
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;
        session.recent_answers = vec![];

        adjust_difficulty(&mut session, 5);

        // Should not change difficulty
        assert!(
            (session.current_difficulty - 0.5).abs() < f64::EPSILON,
            "Expected 0.5 (no change with empty answers), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_adjust_difficulty_zero_window_size() {
        // Window size of 0
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;
        session.recent_answers = vec![true, true, true, true, true];

        adjust_difficulty(&mut session, 0);

        // Should not change difficulty
        assert!(
            (session.current_difficulty - 0.5).abs() < f64::EPSILON,
            "Expected 0.5 (no change with window_size=0), got {}",
            session.current_difficulty
        );
    }

    // ============================================================
    // AC-6: Difficulty bounds tests
    // ============================================================

    #[test]
    fn test_difficulty_upper_bound_clamping() {
        // AC-6: Difficulty bounded to [0.1, 0.95]
        let mut session = GameSession::default();
        session.current_difficulty = 0.93; // Close to max

        // All correct answers should try to increase difficulty
        session.recent_answers = vec![true, true, true, true, true];

        adjust_difficulty(&mut session, 5);

        // Should be clamped to 0.95
        assert!(
            (session.current_difficulty - 0.95).abs() < f64::EPSILON,
            "Expected 0.95 (clamped at max), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_difficulty_lower_bound_clamping() {
        // AC-6: Difficulty bounded to [0.1, 0.95]
        let mut session = GameSession::default();
        session.current_difficulty = 0.12; // Close to min

        // All wrong answers should try to decrease difficulty
        session.recent_answers = vec![false, false, false, false, false];

        adjust_difficulty(&mut session, 5);

        // Should be clamped to 0.1
        assert!(
            (session.current_difficulty - 0.1).abs() < f64::EPSILON,
            "Expected 0.1 (clamped at min), got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_difficulty_at_max_stays_at_max() {
        // Already at max difficulty
        let mut session = GameSession::default();
        session.current_difficulty = MAX_DIFFICULTY; // 0.95

        session.recent_answers = vec![true, true, true, true, true];

        adjust_difficulty(&mut session, 5);

        // Should stay at 0.95
        assert!(
            (session.current_difficulty - 0.95).abs() < f64::EPSILON,
            "Expected 0.95, got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_difficulty_at_min_stays_at_min() {
        // Already at min difficulty
        let mut session = GameSession::default();
        session.current_difficulty = MIN_DIFFICULTY; // 0.1

        session.recent_answers = vec![false, false, false, false, false];

        adjust_difficulty(&mut session, 5);

        // Should stay at 0.1
        assert!(
            (session.current_difficulty - 0.1).abs() < f64::EPSILON,
            "Expected 0.1, got {}",
            session.current_difficulty
        );
    }

    #[test]
    fn test_difficulty_multiple_adjustments() {
        // Multiple adjustments in sequence
        let mut session = GameSession::default();
        session.current_difficulty = 0.5;

        // First adjustment: increase (100% accuracy)
        session.recent_answers = vec![true, true, true, true, true];
        adjust_difficulty(&mut session, 5);
        assert!((session.current_difficulty - 0.55).abs() < f64::EPSILON);

        // Second adjustment: increase again
        session.recent_answers = vec![true, true, true, true, true];
        adjust_difficulty(&mut session, 5);
        assert!((session.current_difficulty - 0.60).abs() < f64::EPSILON);

        // Third adjustment: decrease (0% accuracy)
        session.recent_answers = vec![false, false, false, false, false];
        adjust_difficulty(&mut session, 5);
        assert!((session.current_difficulty - 0.55).abs() < f64::EPSILON);
    }

    #[test]
    fn test_difficulty_clamp_from_extreme_values() {
        // Test clamping from values outside valid range
        let mut session = GameSession::default();

        // Manually set difficulty outside valid range (shouldn't happen normally)
        session.current_difficulty = 2.0; // Way above max
        session.recent_answers = vec![true, true, true, true, true]; // Would increase

        adjust_difficulty(&mut session, 5);

        // Should be clamped to 0.95
        assert!(
            (session.current_difficulty - 0.95).abs() < f64::EPSILON,
            "Expected 0.95, got {}",
            session.current_difficulty
        );

        // Test negative difficulty
        session.current_difficulty = -0.5; // Below min
        session.recent_answers = vec![false, false, false, false, false]; // Would decrease

        adjust_difficulty(&mut session, 5);

        // Should be clamped to 0.1
        assert!(
            (session.current_difficulty - 0.1).abs() < f64::EPSILON,
            "Expected 0.1, got {}",
            session.current_difficulty
        );
    }

    // ============================================================
    // Integration tests: GameSession + GameRound
    // ============================================================

    #[test]
    fn test_session_and_round_integration() {
        let mut session = GameSession::with_username("TestPlayer");
        let mut round = GameRound::new(
            "Test phrase",
            vec!["a".into(), "b".into(), "c".into(), "d".into(), "e".into()],
            "c",
        );

        // Simulate answering
        round.record_answer("c", 1000);

        // Update session stats
        session.total_answers += 1;
        if round.is_correct == Some(true) {
            session.correct_answers += 1;
        }
        session.sum_difficulty += session.current_difficulty;
        session
            .recent_answers
            .push(round.is_correct.unwrap_or(false));

        assert_eq!(session.total_answers, 1);
        assert_eq!(session.correct_answers, 1);
        assert!((session.sum_difficulty - 0.5).abs() < f64::EPSILON);
        assert_eq!(session.recent_answers, vec![true]);
    }

    #[test]
    fn test_full_game_flow_with_difficulty_adjustment() {
        let mut session = GameSession::with_username("Player1");

        // Simulate 5 rounds with all correct answers
        for _ in 0..5 {
            session.total_answers += 1;
            session.correct_answers += 1;
            session.sum_difficulty += session.current_difficulty;
            session.recent_answers.push(true);
        }

        // Difficulty should increase after adjustment
        let old_difficulty = session.current_difficulty;
        adjust_difficulty(&mut session, 5);

        assert!(
            session.current_difficulty > old_difficulty,
            "Difficulty should increase after 100% accuracy"
        );
        assert_eq!(session.total_answers, 5);
        assert_eq!(session.correct_answers, 5);
    }

    // ============================================================
    // AC-3: generate_username() tests
    // ============================================================

    #[test]
    fn test_generate_username_format() {
        // AC-3: generate_username() returns random {Adjective}{Animal}{Number} pattern
        let username = generate_username();

        // Should match pattern: starts with adjective, contains animal, ends with 2-digit number
        // Minimum length: "Bold" (4) + "Fox" (3) + "00" (2) = 9
        // Maximum length: "Nimble" (6) + "Tiger" (5) + "99" (2) = 13
        assert!(
            username.len() >= 9 && username.len() <= 13,
            "Username '{}' should be between 9 and 13 characters",
            username
        );

        // Should end with a 2-digit number
        let last_two = &username[username.len() - 2..];
        assert!(
            last_two.chars().all(|c| c.is_ascii_digit()),
            "Username '{}' should end with 2-digit number, got '{}'",
            username,
            last_two
        );
    }

    #[test]
    fn test_generate_username_starts_with_valid_adjective() {
        let username = generate_username();

        // Check that it starts with one of the valid adjectives
        let starts_with_adjective = ADJECTIVES.iter().any(|adj| username.starts_with(adj));

        assert!(
            starts_with_adjective,
            "Username '{}' should start with a valid adjective",
            username
        );
    }

    #[test]
    fn test_generate_username_contains_valid_animal() {
        let username = generate_username();

        // Check that it contains one of the valid animals (excluding the number at end)
        let without_number = &username[..username.len() - 2];
        let contains_animal = ANIMALS
            .iter()
            .any(|animal| without_number.ends_with(animal));

        assert!(
            contains_animal,
            "Username '{}' should contain a valid animal",
            username
        );
    }

    #[test]
    fn test_generate_username_number_range() {
        // Generate multiple usernames and verify number is always 00-99
        for _ in 0..20 {
            let username = generate_username();
            let last_two = &username[username.len() - 2..];
            let number: u8 = last_two.parse().expect("Last two chars should be a number");
            assert!(number < 100, "Number should be 00-99, got {}", number);
        }
    }

    #[test]
    fn test_generate_username_uniqueness() {
        // Generate multiple usernames and check they are not all the same
        // (probabilistically, 100 usernames should have some variety)
        let usernames: Vec<String> = (0..100).map(|_| generate_username()).collect();
        let unique_count = usernames
            .iter()
            .collect::<std::collections::HashSet<_>>()
            .len();

        // With 10 adjectives, 10 animals, 100 numbers = 10,000 possibilities
        // 100 samples should have high uniqueness
        assert!(
            unique_count > 50,
            "Expected at least 50 unique usernames from 100 generations, got {}",
            unique_count
        );
    }

    #[test]
    fn test_generate_username_examples() {
        // Validate that specific patterns are possible
        // Note: We can't test for specific outputs due to randomness,
        // but we can validate the format is consistent
        for _ in 0..10 {
            let username = generate_username();

            // Should be alphanumeric
            assert!(
                username.chars().all(|c| c.is_alphanumeric()),
                "Username '{}' should only contain alphanumeric characters",
                username
            );

            // Should start with uppercase (adjective starts with capital)
            assert!(
                username.chars().next().unwrap().is_uppercase(),
                "Username '{}' should start with uppercase letter",
                username
            );
        }
    }

    // ============================================================
    // AC-4: calculate_score() tests
    // ============================================================

    #[test]
    fn test_calculate_score_zero_total_answers() {
        // AC-4: Handle edge case: total == 0 returns 0.0
        let session = GameSession::default();
        let score = calculate_score(&session);

        assert!(
            (score - 0.0).abs() < f64::EPSILON,
            "Score should be 0.0 when total_answers is 0, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_basic_calculation() {
        // AC-4: score = accuracy * avg_difficulty * answer_factor
        let mut session = GameSession::default();
        session.total_answers = 10;
        session.correct_answers = 8;
        session.sum_difficulty = 5.0; // avg_difficulty = 0.5

        let score = calculate_score(&session);

        // accuracy = 0.8
        // avg_difficulty = 0.5
        // answer_factor = log2(11) / log2(50) ≈ 3.459 / 5.644 ≈ 0.613
        // score ≈ 0.8 * 0.5 * 0.613 ≈ 0.245
        assert!(
            score > 0.0 && score < 1.0,
            "Score should be between 0 and 1, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_perfect_accuracy() {
        let mut session = GameSession::default();
        session.total_answers = 50;
        session.correct_answers = 50;
        session.sum_difficulty = 47.5; // avg_difficulty = 0.95

        let score = calculate_score(&session);

        // accuracy = 1.0
        // avg_difficulty = 0.95
        // answer_factor = log2(51) / log2(50) ≈ 5.672 / 5.644 ≈ 1.005, capped at 1.0
        // score ≈ 1.0 * 0.95 * 1.0 = 0.95
        assert!(
            (score - 0.95).abs() < 0.01,
            "Perfect score with max difficulty should be ~0.95, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_zero_accuracy() {
        let mut session = GameSession::default();
        session.total_answers = 10;
        session.correct_answers = 0;
        session.sum_difficulty = 5.0;

        let score = calculate_score(&session);

        // accuracy = 0.0, so score should be 0
        assert!(
            (score - 0.0).abs() < f64::EPSILON,
            "Score should be 0.0 when accuracy is 0, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_answer_factor_cap() {
        // AC-4: answer_factor is capped at 1.0
        let mut session = GameSession::default();
        session.total_answers = 100; // > 50, so factor would be > 1.0 without cap
        session.correct_answers = 100;
        session.sum_difficulty = 95.0; // avg = 0.95

        let score = calculate_score(&session);

        // answer_factor = log2(101) / log2(50) ≈ 6.658 / 5.644 ≈ 1.18, but capped at 1.0
        // score = 1.0 * 0.95 * 1.0 = 0.95
        assert!(
            (score - 0.95).abs() < 0.01,
            "Score should be ~0.95 with capped answer_factor, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_single_answer() {
        let mut session = GameSession::default();
        session.total_answers = 1;
        session.correct_answers = 1;
        session.sum_difficulty = 0.5;

        let score = calculate_score(&session);

        // accuracy = 1.0
        // avg_difficulty = 0.5
        // answer_factor = log2(2) / log2(50) ≈ 1.0 / 5.644 ≈ 0.177
        // score ≈ 1.0 * 0.5 * 0.177 ≈ 0.089
        assert!(
            score > 0.0 && score < 0.15,
            "Single correct answer should give small score, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_formula_verification() {
        // Verify exact formula with known values
        let mut session = GameSession::default();
        session.total_answers = 10;
        session.correct_answers = 5;
        session.sum_difficulty = 5.0;

        let score = calculate_score(&session);

        // Manual calculation:
        let accuracy = 5.0 / 10.0; // 0.5
        let avg_difficulty = 5.0 / 10.0; // 0.5
        let answer_factor = (11.0_f64.log2() / 50.0_f64.log2()).min(1.0);
        let expected = accuracy * avg_difficulty * answer_factor;

        assert!(
            (score - expected).abs() < f64::EPSILON,
            "Score {} should match expected {}",
            score,
            expected
        );
    }

    #[test]
    fn test_calculate_score_low_difficulty() {
        let mut session = GameSession::default();
        session.total_answers = 50;
        session.correct_answers = 50;
        session.sum_difficulty = 5.0; // avg_difficulty = 0.1 (minimum)

        let score = calculate_score(&session);

        // accuracy = 1.0
        // avg_difficulty = 0.1
        // answer_factor ≈ 1.0
        // score ≈ 1.0 * 0.1 * 1.0 = 0.1
        assert!(
            (score - 0.1).abs() < 0.01,
            "Low difficulty score should be ~0.1, got {}",
            score
        );
    }

    #[test]
    fn test_calculate_score_progression() {
        // Score should increase with more correct answers
        let mut session = GameSession::default();
        session.sum_difficulty = 0.0;

        let mut scores = Vec::new();

        for i in 1..=20 {
            session.total_answers = i;
            session.correct_answers = i;
            session.sum_difficulty += 0.5;
            scores.push(calculate_score(&session));
        }

        // Each score should be greater than the previous
        for i in 1..scores.len() {
            assert!(
                scores[i] > scores[i - 1],
                "Score should increase: {} at {} answers should be > {} at {} answers",
                scores[i],
                i + 1,
                scores[i - 1],
                i
            );
        }
    }
}
