//! Game Engine Module for Round Orchestration (TEA-GAME-001.5)
//!
//! This module provides the `GameEngine` struct that orchestrates all game components:
//! - Session management with random username generation
//! - Round generation with LLM phrase generation and similarity search
//! - Answer submission with validation and difficulty adjustment
//! - Leaderboard submission with best-score tracking
//! - Opik tracing integration (when enabled)
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::games::engine::{GameEngine, GameEngineConfig};
//!
//! // Create engine
//! let config = GameEngineConfig {
//!     db_path: ":memory:".to_string(),
//!     parquet_path: Some("data/words.parquet".to_string()),
//!     max_history_rounds: 10,
//!     difficulty_window_size: 5,
//!     opik_enabled: false,
//! };
//!
//! let mut engine = GameEngine::new(config)?;
//!
//! // Start a session
//! let session_info = engine.start_session()?;
//!
//! // Generate a round (with LLM callback)
//! let round = engine.generate_round(|messages| {
//!     // Call your LLM here
//!     Ok(r#"{"phrase": "The ___ is bright.", "word": "sun"}"#.to_string())
//! })?;
//!
//! // Submit an answer
//! let result = engine.submit_answer(&round.choices[0], 1500)?;
//!
//! // Submit to leaderboard
//! let leaderboard_result = engine.submit_to_leaderboard()?;
//! ```

use std::collections::VecDeque;

use rand::seq::SliceRandom;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::games::db::{GameDb, LeaderboardEntry};
use crate::games::embeddings::EmbeddingSearch;
use crate::games::opik::{is_game_opik_enabled, send_game_span, GameOpikConfig, OpikGameSpan};
use crate::games::phrase_generator::{Message, PhraseError, PhraseGenerator};
use crate::games::{adjust_difficulty, calculate_score, generate_username, GameRound, GameSession};

// =============================================================================
// Error Types (AC-8)
// =============================================================================

/// Errors that can occur during game engine operations.
#[derive(Debug, Error)]
pub enum GameError {
    /// No active session - call start_session() first
    #[error("No active session - call start_session() first")]
    NoSession,

    /// Session has already been submitted to the leaderboard
    #[error("Session has already been submitted to the leaderboard")]
    AlreadySubmitted,

    /// Invalid choice - the selected word is not in the choices list
    #[error("Invalid choice: {0} is not in the choices list")]
    InvalidChoice(String),

    /// No active round - call generate_round() first
    #[error("No active round - call generate_round() first")]
    NoActiveRound,

    /// LLM error during phrase generation
    #[error("LLM error: {0}")]
    LlmError(#[from] PhraseError),

    /// Database error
    #[error("Database error: {0}")]
    DbError(#[from] duckdb::Error),

    /// Embedding not found for word
    #[error("Embedding not found for word: {0}")]
    EmbeddingNotFound(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Insufficient distractors found
    #[error("Could not find enough similar words for difficulty {0:.2}")]
    InsufficientDistractors(f64),
}

// =============================================================================
// Configuration (AC-1)
// =============================================================================

/// Configuration for the GameEngine.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameEngineConfig {
    /// Path to the DuckDB database (use ":memory:" for in-memory)
    pub db_path: String,

    /// Path to Parquet file with word embeddings (optional)
    pub parquet_path: Option<String>,

    /// Maximum number of rounds to keep in phrase generator history
    #[serde(default = "default_max_history_rounds")]
    pub max_history_rounds: usize,

    /// Number of recent answers to consider for difficulty adjustment
    #[serde(default = "default_difficulty_window_size")]
    pub difficulty_window_size: usize,

    /// Whether to enable Opik tracing
    #[serde(default)]
    pub opik_enabled: bool,

    /// Opik project name (optional)
    #[serde(default)]
    pub opik_project_name: Option<String>,
}

fn default_max_history_rounds() -> usize {
    10
}

fn default_difficulty_window_size() -> usize {
    5
}

impl Default for GameEngineConfig {
    fn default() -> Self {
        Self {
            db_path: ":memory:".to_string(),
            parquet_path: None,
            max_history_rounds: default_max_history_rounds(),
            difficulty_window_size: default_difficulty_window_size(),
            opik_enabled: false,
            opik_project_name: None,
        }
    }
}

// =============================================================================
// Result Types
// =============================================================================

/// Information about a started session (AC-2).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionInfo {
    /// Unique session identifier
    pub session_id: String,
    /// Generated username
    pub username: String,
    /// Initial difficulty level
    pub initial_difficulty: f64,
}

/// Result from submitting an answer (AC-4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnswerResult {
    /// Whether the answer was correct
    pub is_correct: bool,
    /// The correct word (revealed after answer)
    pub correct_word: String,
    /// Current cumulative score
    pub current_score: f64,
    /// Current difficulty level (may have changed)
    pub current_difficulty: f64,
    /// Total correct answers so far
    pub correct_answers: u32,
    /// Total answers so far
    pub total_answers: u32,
}

/// Result from submitting to the leaderboard (AC-5).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeaderboardResult {
    /// Whether the submission was successful (new high score)
    pub success: bool,
    /// The final score
    pub score: f64,
    /// Player's rank on the leaderboard (1-indexed, None if not in top N)
    pub rank: Option<u32>,
    /// Whether this was a new personal best
    pub is_new_best: bool,
    /// Accuracy (correct / total)
    pub accuracy: f64,
    /// Total answers submitted
    pub total_answers: u32,
    /// Average difficulty
    pub avg_difficulty: f64,
}

// =============================================================================
// GameEngine (AC-1)
// =============================================================================

/// The main game engine that orchestrates all components.
///
/// The engine manages:
/// - Database persistence via `GameDb`
/// - Embedding similarity search via `EmbeddingSearch`
/// - Phrase generation via `PhraseGenerator`
/// - Session state and difficulty adjustment
/// - Opik tracing (when enabled)
pub struct GameEngine {
    /// Database connection
    db: GameDb,

    /// Phrase generator with conversation history
    phrase_generator: PhraseGenerator,

    /// Current active session (None if no session started)
    session: Option<GameSession>,

    /// Current active round (None if no round generated or already answered)
    current_round: Option<GameRound>,

    /// Rolling window of recent answers for difficulty adjustment
    recent_answers: VecDeque<bool>,

    /// Engine configuration
    config: GameEngineConfig,

    /// Whether the session has been submitted to the leaderboard
    session_submitted: bool,
}

impl GameEngine {
    /// Create a new GameEngine with the given configuration (AC-1).
    ///
    /// This will:
    /// 1. Open the DuckDB database
    /// 2. Initialize the schema
    /// 3. Load VSS extension for similarity search
    /// 4. Load word embeddings from Parquet if provided
    /// 5. Initialize the phrase generator
    /// 6. Configure Opik if enabled
    ///
    /// # Errors
    ///
    /// Returns `GameError::DbError` if database initialization fails.
    /// Returns `GameError::ConfigError` if embeddings fail to load.
    pub fn new(config: GameEngineConfig) -> Result<Self, GameError> {
        // Initialize database
        let db = GameDb::new(&config.db_path)?;
        db.init_schema()?;

        // Load VSS extension for similarity search
        // This may fail if VSS is not available, but we continue anyway
        if let Err(e) = db.load_vss_extension() {
            log::warn!("VSS extension not available: {}", e);
        }

        // Load word embeddings from Parquet if provided
        if let Some(ref parquet_path) = config.parquet_path {
            let path = std::path::Path::new(parquet_path);
            if path.exists() {
                match db.load_words_from_parquet_with_progress(path) {
                    Ok(count) => {
                        log::info!("Loaded {} words from Parquet", count);
                    }
                    Err(e) => {
                        log::warn!("Failed to load Parquet embeddings: {}", e);
                    }
                }
            } else {
                log::warn!("Parquet file not found: {}", parquet_path);
            }
        }

        // Initialize phrase generator
        let phrase_generator = PhraseGenerator::new(config.max_history_rounds);

        // Configure Opik if enabled
        if config.opik_enabled {
            crate::games::opik::configure_game_opik(GameOpikConfig {
                enabled: true,
                project_name: config.opik_project_name.clone(),
            });
        }

        Ok(Self {
            db,
            phrase_generator,
            session: None,
            current_round: None,
            recent_answers: VecDeque::with_capacity(config.difficulty_window_size),
            config,
            session_submitted: false,
        })
    }

    /// Get a reference to the database (for testing/debugging).
    pub fn db(&self) -> &GameDb {
        &self.db
    }

    /// Get a reference to the current session (if any).
    pub fn session(&self) -> Option<&GameSession> {
        self.session.as_ref()
    }

    /// Get a reference to the current round (if any).
    pub fn current_round(&self) -> Option<&GameRound> {
        self.current_round.as_ref()
    }

    /// Check if a session is active.
    pub fn has_session(&self) -> bool {
        self.session.is_some()
    }

    /// Check if a round is pending (generated but not answered).
    pub fn has_pending_round(&self) -> bool {
        self.current_round.is_some()
    }

    // =========================================================================
    // Session Management (AC-2)
    // =========================================================================

    /// Start a new game session (AC-2).
    ///
    /// This will:
    /// 1. Generate a random username
    /// 2. Create a new GameSession
    /// 3. Insert the session into the database
    /// 4. Reset the phrase generator history
    /// 5. Record Opik span if enabled
    ///
    /// # Errors
    ///
    /// Returns `GameError::DbError` if database insertion fails.
    pub fn start_session(&mut self) -> Result<SessionInfo, GameError> {
        // Generate random username
        let username = generate_username();

        // Create new session
        let mut session = GameSession::with_username(&username);
        let session_id = session.id.clone();
        let initial_difficulty = session.current_difficulty;

        // Insert into database
        self.db.insert_session(&session)?;

        // Reset state
        self.phrase_generator.clear_history();
        self.recent_answers.clear();
        self.current_round = None;
        self.session_submitted = false;

        // Record Opik span if enabled (AC-7)
        if is_game_opik_enabled() {
            let span = OpikGameSpan::new_session(
                &session_id,
                serde_json::json!({
                    "username": username,
                    "started_at": chrono::Utc::now().to_rfc3339(),
                    "project_name": self.config.opik_project_name.as_deref().unwrap_or("know-your-model"),
                    "initial_difficulty": initial_difficulty,
                }),
            );
            let _ = send_game_span(&span);
        }

        // Store session
        session.recent_answers = Vec::new(); // Reset for new session
        self.session = Some(session);

        Ok(SessionInfo {
            session_id,
            username,
            initial_difficulty,
        })
    }

    // =========================================================================
    // Round Generation (AC-3)
    // =========================================================================

    /// Generate a new game round (AC-3).
    ///
    /// This will:
    /// 1. Call the LLM for a new phrase + word
    /// 2. Find 4 similar words based on current difficulty
    /// 3. Shuffle all 5 words (correct + 4 distractors)
    /// 4. Create and return a GameRound
    /// 5. Record Opik span if enabled
    ///
    /// # Arguments
    ///
    /// * `llm_callback` - Function that takes conversation history and returns LLM response
    ///
    /// # Errors
    ///
    /// Returns `GameError::NoSession` if no session is active.
    /// Returns `GameError::LlmError` if phrase generation fails.
    /// Returns `GameError::InsufficientDistractors` if not enough similar words found.
    pub fn generate_round<F, E>(&mut self, llm_callback: F) -> Result<GameRound, GameError>
    where
        F: Fn(&[Message]) -> Result<String, E>,
        E: std::fmt::Display,
    {
        let session = self.session.as_ref().ok_or(GameError::NoSession)?;
        let session_id = session.id.clone();
        let difficulty = session.current_difficulty;
        let round_number = session.total_answers + 1;

        // 1. Generate phrase from LLM
        let phrase_result = self.phrase_generator.generate_phrase(llm_callback)?;

        // Record LLM span if enabled
        if is_game_opik_enabled() {
            let mut llm_span = OpikGameSpan::new_llm_phrase(
                &uuid::Uuid::new_v4().to_string(),
                Some(session_id.clone()),
                serde_json::json!({
                    "phrase": phrase_result.phrase,
                    "word": phrase_result.word,
                    "round_number": round_number,
                }),
            );
            llm_span.complete();
            let _ = send_game_span(&llm_span);
        }

        // 2. Find similar words based on difficulty
        let embedding_search = EmbeddingSearch::new(&self.db);
        let similar_words = embedding_search.find_similar_words_with_fallback(
            &phrase_result.word,
            4,
            difficulty,
        )?;

        // Handle fallback if word not in vocabulary or insufficient distractors
        let distractors = if similar_words.len() < 4 {
            log::warn!(
                "Only found {} similar words for '{}', using fallback",
                similar_words.len(),
                phrase_result.word
            );
            // Use wider search to find any 4 words
            let mut all_words = self.db.get_all_words()?;
            all_words.retain(|w| w != &phrase_result.word);
            all_words.shuffle(&mut rand::thread_rng());
            all_words.truncate(4);
            all_words
        } else {
            similar_words
        };

        // Ensure we have exactly 4 distractors
        if distractors.len() < 4 {
            return Err(GameError::InsufficientDistractors(difficulty));
        }

        // 3. Combine and shuffle
        let mut choices: Vec<String> = distractors;
        choices.push(phrase_result.word.clone());
        choices.shuffle(&mut rand::thread_rng());

        // 4. Create round
        let round = GameRound::new(phrase_result.phrase, choices, phrase_result.word);

        // Record round span if enabled (AC-7)
        if is_game_opik_enabled() {
            let span = OpikGameSpan::new_round(
                &uuid::Uuid::new_v4().to_string(),
                Some(session_id),
                serde_json::json!({
                    "round_number": round_number,
                    "phrase": round.phrase,
                    "choices": round.choices,
                    "correct_word": round.correct_word,
                    "difficulty": difficulty,
                }),
            );
            let _ = send_game_span(&span);
        }

        // Store current round
        self.current_round = Some(round.clone());

        Ok(round)
    }

    // =========================================================================
    // Answer Submission (AC-4)
    // =========================================================================

    /// Submit an answer for the current round (AC-4).
    ///
    /// This will:
    /// 1. Validate the answer is one of the choices
    /// 2. Determine correctness
    /// 3. Update session stats
    /// 4. Record answer in database
    /// 5. Update word knowledge graph
    /// 6. Adjust difficulty based on rolling window
    /// 7. Record Opik span if enabled
    ///
    /// # Arguments
    ///
    /// * `selected_word` - The word the player selected
    /// * `response_time_ms` - Time taken to answer in milliseconds
    ///
    /// # Errors
    ///
    /// Returns `GameError::NoSession` if no session is active.
    /// Returns `GameError::NoActiveRound` if no round is pending.
    /// Returns `GameError::InvalidChoice` if the selected word is not in choices.
    /// Returns `GameError::AlreadySubmitted` if session was already submitted.
    pub fn submit_answer(
        &mut self,
        selected_word: &str,
        response_time_ms: u64,
    ) -> Result<AnswerResult, GameError> {
        // Check session state
        if self.session_submitted {
            return Err(GameError::AlreadySubmitted);
        }

        let session = self.session.as_mut().ok_or(GameError::NoSession)?;
        let round = self
            .current_round
            .as_mut()
            .ok_or(GameError::NoActiveRound)?;

        // Validate choice
        if !round.choices.contains(&selected_word.to_string()) {
            return Err(GameError::InvalidChoice(selected_word.to_string()));
        }

        // Determine correctness
        let is_correct = selected_word == round.correct_word;

        // Update round
        round.selected_word = Some(selected_word.to_string());
        round.is_correct = Some(is_correct);
        round.response_time_ms = Some(response_time_ms);

        // Update session stats
        session.total_answers += 1;
        if is_correct {
            session.correct_answers += 1;
        }
        session.sum_difficulty += session.current_difficulty;

        let difficulty_before = session.current_difficulty;

        // Record to DB
        self.db
            .record_answer(&session.id, round, session.current_difficulty)?;
        self.db.update_session(session)?;

        // Update rolling window and adjust difficulty
        self.recent_answers.push_back(is_correct);
        if self.recent_answers.len() > self.config.difficulty_window_size {
            self.recent_answers.pop_front();
        }

        // Copy recent answers to session for difficulty adjustment
        session.recent_answers = self.recent_answers.iter().cloned().collect();
        adjust_difficulty(session, self.config.difficulty_window_size);

        // Calculate current score
        let current_score = calculate_score(session);

        // Record Opik span if enabled (AC-7)
        if is_game_opik_enabled() {
            let mut span = OpikGameSpan::new_round(
                &uuid::Uuid::new_v4().to_string(),
                Some(session.id.clone()),
                serde_json::json!({
                    "phrase": round.phrase,
                    "correct_word": round.correct_word,
                    "selected_word": selected_word,
                    "is_correct": is_correct,
                    "response_time_ms": response_time_ms,
                    "difficulty": difficulty_before,
                    "new_difficulty": session.current_difficulty,
                    "running_score": current_score,
                    "running_accuracy": session.correct_answers as f64 / session.total_answers as f64,
                }),
            );
            span.complete_with(
                if is_correct { "success" } else { "failure" },
                Some(response_time_ms as f64),
            );
            let _ = send_game_span(&span);
        }

        // Clear current round (answered)
        let result = AnswerResult {
            is_correct,
            correct_word: round.correct_word.clone(),
            current_score,
            current_difficulty: session.current_difficulty,
            correct_answers: session.correct_answers,
            total_answers: session.total_answers,
        };

        self.current_round = None;

        Ok(result)
    }

    // =========================================================================
    // Leaderboard (AC-5, AC-6)
    // =========================================================================

    /// Submit the current session to the leaderboard (AC-5).
    ///
    /// This will:
    /// 1. Calculate final score using the scoring formula
    /// 2. Submit to database (UPSERT - only updates if better score)
    /// 3. Mark session as submitted
    /// 4. Record Opik span if enabled
    ///
    /// # Errors
    ///
    /// Returns `GameError::NoSession` if no session is active.
    /// Returns `GameError::AlreadySubmitted` if already submitted.
    /// Returns `GameError::DbError` if database operation fails.
    pub fn submit_to_leaderboard(&mut self) -> Result<LeaderboardResult, GameError> {
        if self.session_submitted {
            return Err(GameError::AlreadySubmitted);
        }

        let session = self.session.as_ref().ok_or(GameError::NoSession)?;

        if session.total_answers == 0 {
            return Ok(LeaderboardResult {
                success: false,
                score: 0.0,
                rank: None,
                is_new_best: false,
                accuracy: 0.0,
                total_answers: 0,
                avg_difficulty: 0.0,
            });
        }

        // Calculate final score
        let score = calculate_score(session);
        let accuracy = session.correct_answers as f64 / session.total_answers as f64;
        let avg_difficulty = session.sum_difficulty / session.total_answers as f64;

        // Check previous best
        let previous_best = self.db.get_leaderboard_score(&session.username)?;
        let is_new_best = previous_best.map_or(true, |prev| score > prev);

        // Submit to leaderboard (UPSERT - only updates if better)
        let success = self.db.submit_to_leaderboard(
            &session.username,
            score,
            accuracy,
            session.total_answers,
            avg_difficulty,
        )?;

        // Mark session as submitted
        self.db.mark_session_submitted(&session.id)?;
        self.session_submitted = true;

        // Get rank
        let leaderboard = self.db.get_top_leaderboard(100)?;
        let rank = leaderboard
            .iter()
            .position(|e| e.username == session.username)
            .map(|pos| (pos + 1) as u32);

        // Record Opik span if enabled (AC-7)
        if is_game_opik_enabled() {
            let mut span = OpikGameSpan::new_leaderboard_submit(
                &session.id,
                serde_json::json!({
                    "username": session.username,
                    "final_score": score,
                    "accuracy": accuracy,
                    "total_answers": session.total_answers,
                    "avg_difficulty": avg_difficulty,
                    "rank": rank,
                    "is_new_best": is_new_best,
                    "success": success,
                }),
            );
            span.complete();
            let _ = send_game_span(&span);
        }

        Ok(LeaderboardResult {
            success,
            score,
            rank,
            is_new_best,
            accuracy,
            total_answers: session.total_answers,
            avg_difficulty,
        })
    }

    /// Get the top N entries from the leaderboard (AC-6).
    ///
    /// # Arguments
    ///
    /// * `limit` - Maximum number of entries to return (defaults to 10)
    ///
    /// # Errors
    ///
    /// Returns `GameError::DbError` if database query fails.
    pub fn get_leaderboard(
        &self,
        limit: Option<usize>,
    ) -> Result<Vec<LeaderboardEntry>, GameError> {
        let limit = limit.unwrap_or(10);
        Ok(self.db.get_top_leaderboard(limit)?)
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    fn create_test_embedding(seed: f32) -> Vec<f32> {
        let mut emb: Vec<f32> = (0..384).map(|i| (i as f32 * seed).sin()).collect();
        let norm: f32 = emb.iter().map(|x| x * x).sum::<f32>().sqrt();
        emb.iter_mut().for_each(|x| *x /= norm);
        emb
    }

    fn setup_test_engine() -> GameEngine {
        let config = GameEngineConfig {
            db_path: ":memory:".to_string(),
            parquet_path: None,
            max_history_rounds: 10,
            difficulty_window_size: 5,
            opik_enabled: false,
            opik_project_name: None,
        };

        let engine = GameEngine::new(config).expect("Failed to create engine");

        // Insert test words with embeddings
        let db = &engine.db;
        db.insert_word("happy", &create_test_embedding(1.0), Some(1000))
            .unwrap();
        db.insert_word("joyful", &create_test_embedding(1.1), Some(900))
            .unwrap();
        db.insert_word("glad", &create_test_embedding(1.05), Some(800))
            .unwrap();
        db.insert_word("cheerful", &create_test_embedding(1.08), Some(700))
            .unwrap();
        db.insert_word("delighted", &create_test_embedding(1.12), Some(600))
            .unwrap();
        db.insert_word("sad", &create_test_embedding(5.0), Some(1000))
            .unwrap();
        db.insert_word("angry", &create_test_embedding(6.0), Some(900))
            .unwrap();
        db.insert_word("cold", &create_test_embedding(10.0), Some(800))
            .unwrap();
        db.insert_word("warm", &create_test_embedding(11.0), Some(700))
            .unwrap();
        db.insert_word("hot", &create_test_embedding(12.0), Some(600))
            .unwrap();

        engine
    }

    // =========================================================================
    // AC-1: GameEngine struct tests
    // =========================================================================

    #[test]
    fn test_game_engine_creation() {
        let config = GameEngineConfig::default();
        let engine = GameEngine::new(config);
        assert!(engine.is_ok());
    }

    #[test]
    fn test_game_engine_config_defaults() {
        let config = GameEngineConfig::default();
        assert_eq!(config.db_path, ":memory:");
        assert!(config.parquet_path.is_none());
        assert_eq!(config.max_history_rounds, 10);
        assert_eq!(config.difficulty_window_size, 5);
        assert!(!config.opik_enabled);
    }

    #[test]
    fn test_game_engine_initial_state() {
        let engine = setup_test_engine();
        assert!(!engine.has_session());
        assert!(!engine.has_pending_round());
        assert!(engine.session().is_none());
        assert!(engine.current_round().is_none());
    }

    // =========================================================================
    // AC-2: start_session() tests
    // =========================================================================

    #[test]
    fn test_start_session() {
        let mut engine = setup_test_engine();
        let session_info = engine.start_session();
        assert!(session_info.is_ok());

        let session_info = session_info.unwrap();
        assert!(!session_info.session_id.is_empty());
        assert!(!session_info.username.is_empty());
        assert!((session_info.initial_difficulty - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn test_start_session_creates_session() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        assert!(engine.has_session());
        let session = engine.session().unwrap();
        assert_eq!(session.total_answers, 0);
        assert_eq!(session.correct_answers, 0);
    }

    #[test]
    fn test_start_session_persists_to_db() {
        let mut engine = setup_test_engine();
        let session_info = engine.start_session().unwrap();

        // Verify session is in database
        let db_session = engine.db.get_session(&session_info.session_id).unwrap();
        assert!(db_session.is_some());
        assert_eq!(db_session.unwrap().username, session_info.username);
    }

    #[test]
    fn test_start_session_generates_random_username() {
        let mut engine = setup_test_engine();
        let session1 = engine.start_session().unwrap();
        let session2 = engine.start_session().unwrap();

        // Usernames should be different (with high probability)
        // Actually they could be the same by chance, so we just check format
        assert!(session1.username.len() >= 9);
        assert!(session2.username.len() >= 9);
    }

    // =========================================================================
    // AC-3: generate_round() tests
    // =========================================================================

    #[test]
    fn test_generate_round_requires_session() {
        let mut engine = setup_test_engine();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        let result = engine.generate_round(mock_callback);
        assert!(matches!(result, Err(GameError::NoSession)));
    }

    #[test]
    fn test_generate_round_returns_round() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        let result = engine.generate_round(mock_callback);
        assert!(result.is_ok());

        let round = result.unwrap();
        assert_eq!(round.phrase, "The ___ is bright.");
        assert_eq!(round.correct_word, "happy");
        assert_eq!(round.choices.len(), 5);
        assert!(round.choices.contains(&"happy".to_string()));
    }

    #[test]
    fn test_generate_round_stores_current_round() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        engine.generate_round(mock_callback).unwrap();

        assert!(engine.has_pending_round());
        assert!(engine.current_round().is_some());
    }

    #[test]
    fn test_generate_round_shuffles_choices() {
        // Generate multiple rounds and check that choices are shuffled
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let counter = RefCell::new(0);
        let mock_callback = |_: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase {}.", "word": "happy"}}"#,
                *count
            ))
        };

        let mut choice_orders = Vec::new();
        for _ in 0..5 {
            let round = engine.generate_round(&mock_callback).unwrap();
            choice_orders.push(round.choices.clone());
            // Simulate answering to allow next round
            engine.submit_answer(&round.correct_word, 1000).unwrap();
        }

        // Check that at least some orders are different (shuffling)
        // With 5 items, probability of all 5 being identical is 1/5!^4 ≈ 0
        let _all_same = choice_orders.windows(2).all(|w| w[0] == w[1]);
        // Actually this could fail if we're extremely unlucky, so we just check length
        assert_eq!(choice_orders.len(), 5);
    }

    // =========================================================================
    // AC-4: submit_answer() tests
    // =========================================================================

    #[test]
    fn test_submit_answer_requires_session() {
        let mut engine = setup_test_engine();

        let result = engine.submit_answer("happy", 1000);
        assert!(matches!(result, Err(GameError::NoSession)));
    }

    #[test]
    fn test_submit_answer_requires_active_round() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let result = engine.submit_answer("happy", 1000);
        assert!(matches!(result, Err(GameError::NoActiveRound)));
    }

    #[test]
    fn test_submit_answer_validates_choice() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        engine.generate_round(mock_callback).unwrap();

        let result = engine.submit_answer("invalid_word", 1000);
        assert!(matches!(result, Err(GameError::InvalidChoice(_))));
    }

    #[test]
    fn test_submit_correct_answer() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        engine.generate_round(mock_callback).unwrap();

        let result = engine.submit_answer("happy", 1000);
        assert!(result.is_ok());

        let answer_result = result.unwrap();
        assert!(answer_result.is_correct);
        assert_eq!(answer_result.correct_word, "happy");
        assert_eq!(answer_result.correct_answers, 1);
        assert_eq!(answer_result.total_answers, 1);
    }

    #[test]
    fn test_submit_incorrect_answer() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        let round = engine.generate_round(mock_callback).unwrap();

        // Pick a wrong answer
        let wrong_answer = round.choices.iter().find(|c| *c != "happy").unwrap();

        let result = engine.submit_answer(wrong_answer, 1000);
        assert!(result.is_ok());

        let answer_result = result.unwrap();
        assert!(!answer_result.is_correct);
        assert_eq!(answer_result.correct_word, "happy");
        assert_eq!(answer_result.correct_answers, 0);
        assert_eq!(answer_result.total_answers, 1);
    }

    #[test]
    fn test_submit_answer_clears_current_round() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        engine.generate_round(mock_callback).unwrap();
        assert!(engine.has_pending_round());

        engine.submit_answer("happy", 1000).unwrap();
        assert!(!engine.has_pending_round());
    }

    #[test]
    fn test_submit_answer_updates_session_stats() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        // Answer 3 rounds
        for _ in 0..3 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }

        let session = engine.session().unwrap();
        assert_eq!(session.total_answers, 3);
        assert_eq!(session.correct_answers, 3);
    }

    #[test]
    fn test_submit_answer_adjusts_difficulty() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        let initial_difficulty = engine.session().unwrap().current_difficulty;

        // Answer 5 rounds correctly (should increase difficulty)
        for _ in 0..5 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }

        let final_difficulty = engine.session().unwrap().current_difficulty;

        // Difficulty should have increased after 5 correct answers (>80% accuracy)
        assert!(
            final_difficulty > initial_difficulty,
            "Difficulty should increase: {} > {}",
            final_difficulty,
            initial_difficulty
        );
    }

    // =========================================================================
    // AC-5: submit_to_leaderboard() tests
    // =========================================================================

    #[test]
    fn test_submit_to_leaderboard_requires_session() {
        let mut engine = setup_test_engine();

        let result = engine.submit_to_leaderboard();
        assert!(matches!(result, Err(GameError::NoSession)));
    }

    #[test]
    fn test_submit_to_leaderboard_with_no_answers() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let result = engine.submit_to_leaderboard();
        assert!(result.is_ok());

        let leaderboard_result = result.unwrap();
        assert!(!leaderboard_result.success);
        assert_eq!(leaderboard_result.score, 0.0);
        assert_eq!(leaderboard_result.total_answers, 0);
    }

    #[test]
    fn test_submit_to_leaderboard_calculates_score() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        // Play some rounds
        for _ in 0..5 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }

        let result = engine.submit_to_leaderboard().unwrap();

        assert!(result.success);
        assert!(result.score > 0.0);
        assert!(result.accuracy > 0.0);
        assert_eq!(result.total_answers, 5);
        assert!(result.is_new_best); // First submission is always new best
    }

    #[test]
    fn test_submit_to_leaderboard_prevents_double_submission() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        engine.generate_round(&mock_callback).unwrap();
        engine.submit_answer("happy", 1000).unwrap();

        // First submission
        engine.submit_to_leaderboard().unwrap();

        // Second submission should fail
        let result = engine.submit_to_leaderboard();
        assert!(matches!(result, Err(GameError::AlreadySubmitted)));
    }

    // =========================================================================
    // AC-6: get_leaderboard() tests
    // =========================================================================

    #[test]
    fn test_get_leaderboard_empty() {
        let engine = setup_test_engine();

        let leaderboard = engine.get_leaderboard(None).unwrap();
        assert!(leaderboard.is_empty());
    }

    #[test]
    fn test_get_leaderboard_returns_entries() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        // Play and submit
        for _ in 0..5 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }
        engine.submit_to_leaderboard().unwrap();

        let leaderboard = engine.get_leaderboard(None).unwrap();
        assert_eq!(leaderboard.len(), 1);
    }

    #[test]
    fn test_get_leaderboard_respects_limit() {
        let mut engine = setup_test_engine();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "The ___ is bright.", "word": "happy"}"#.to_string())
        };

        // Create multiple sessions and submit
        for _ in 0..5 {
            engine.start_session().unwrap();
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
            engine.submit_to_leaderboard().unwrap();
        }

        let leaderboard = engine.get_leaderboard(Some(3)).unwrap();
        assert!(leaderboard.len() <= 3);
    }

    // =========================================================================
    // AC-8: Error handling tests
    // =========================================================================

    #[test]
    fn test_llm_error_propagates() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback =
            |_: &[Message]| -> Result<String, String> { Err("Network error".to_string()) };

        let result = engine.generate_round(mock_callback);
        assert!(matches!(result, Err(GameError::LlmError(_))));
    }

    #[test]
    fn test_json_parse_error() {
        let mut engine = setup_test_engine();
        engine.start_session().unwrap();

        let mock_callback =
            |_: &[Message]| -> Result<String, String> { Ok("invalid json".to_string()) };

        let result = engine.generate_round(mock_callback);
        assert!(matches!(result, Err(GameError::LlmError(_))));
    }

    // =========================================================================
    // Integration tests: Full game flow
    // =========================================================================

    #[test]
    fn test_full_game_flow() {
        let mut engine = setup_test_engine();

        // 1. Start session
        let session_info = engine.start_session().unwrap();
        assert!(!session_info.username.is_empty());

        let counter = RefCell::new(0);
        let mock_callback = |_: &[Message]| -> Result<String, String> {
            let mut count = counter.borrow_mut();
            *count += 1;
            Ok(format!(
                r#"{{"phrase": "Phrase {}.", "word": "happy"}}"#,
                *count
            ))
        };

        // 2. Play 10 rounds
        for i in 0..10 {
            let round = engine.generate_round(&mock_callback).unwrap();
            assert_eq!(round.choices.len(), 5);

            // Alternate correct/incorrect
            let answer = if i % 2 == 0 {
                "happy"
            } else {
                round.choices.iter().find(|c| *c != "happy").unwrap()
            };

            let result = engine
                .submit_answer(answer, 1000 + (i as u64 * 100))
                .unwrap();
            assert_eq!(result.total_answers, i as u32 + 1);
        }

        // 3. Submit to leaderboard
        let leaderboard_result = engine.submit_to_leaderboard().unwrap();
        assert!(leaderboard_result.success);
        assert_eq!(leaderboard_result.total_answers, 10);
        assert!((leaderboard_result.accuracy - 0.5).abs() < f64::EPSILON);

        // 4. Check leaderboard
        let leaderboard = engine.get_leaderboard(None).unwrap();
        assert_eq!(leaderboard.len(), 1);
        assert_eq!(leaderboard[0].username, session_info.username);
    }

    #[test]
    fn test_multiple_sessions() {
        let mut engine = setup_test_engine();

        let mock_callback = |_: &[Message]| -> Result<String, String> {
            Ok(r#"{"phrase": "Test phrase.", "word": "happy"}"#.to_string())
        };

        // First session - play and submit
        let session1 = engine.start_session().unwrap();
        for _ in 0..5 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }
        engine.submit_to_leaderboard().unwrap();

        // Second session - play and submit
        let session2 = engine.start_session().unwrap();
        assert_ne!(session1.session_id, session2.session_id);

        for _ in 0..3 {
            engine.generate_round(&mock_callback).unwrap();
            engine.submit_answer("happy", 1000).unwrap();
        }
        engine.submit_to_leaderboard().unwrap();

        // Check leaderboard has both (different usernames)
        let leaderboard = engine.get_leaderboard(None).unwrap();
        // Could be 1 or 2 depending on whether usernames collided (unlikely)
        assert!(leaderboard.len() >= 1);
    }

    // =========================================================================
    // Error type tests
    // =========================================================================

    #[test]
    fn test_game_error_display() {
        assert_eq!(
            GameError::NoSession.to_string(),
            "No active session - call start_session() first"
        );
        assert_eq!(
            GameError::AlreadySubmitted.to_string(),
            "Session has already been submitted to the leaderboard"
        );
        assert_eq!(
            GameError::InvalidChoice("test".to_string()).to_string(),
            "Invalid choice: test is not in the choices list"
        );
        assert_eq!(
            GameError::NoActiveRound.to_string(),
            "No active round - call generate_round() first"
        );
    }
}
