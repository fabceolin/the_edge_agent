//! Game WASM Exports (TEA-GAME-001.6)
//!
//! This module provides WASM exports for the "Know Your Model" game engine,
//! allowing it to run in the browser alongside the existing demo.
//!
//! ## Architecture
//!
//! The game engine uses a thread-local singleton pattern (since WASM is single-threaded):
//! - `GAME_ENGINE`: The GameEngine instance
//! - `GAME_SESSION_ID`: Current session ID for validation
//!
//! All functions return JSON with a consistent structure:
//! - Success: `{"success": true, "data": {...}}`
//! - Error: `{"success": false, "error": "...", "error_type": "..."}`

use rand::seq::SliceRandom;
use rand::Rng;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

use crate::game_opik::{
    has_game_opik_handler, send_game_opik_span, OpikGameSpan,
};

// =============================================================================
// Thread-Local State
// =============================================================================

thread_local! {
    /// The game engine instance (initialized lazily)
    static GAME_ENGINE: RefCell<Option<GameEngineWasm>> = const { RefCell::new(None) };

    /// LLM callback function registered from JavaScript
    static LLM_CALLBACK: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

// =============================================================================
// Error Types (AC-6)
// =============================================================================

/// Error types for WASM game operations
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum GameErrorType {
    /// No active session
    SessionError,
    /// LLM callback error
    LlmError,
    /// Database error
    DbError,
    /// Invalid choice selected
    InvalidChoice,
    /// Configuration error
    ConfigError,
    /// No active round
    RoundError,
    /// Already submitted to leaderboard
    AlreadySubmitted,
    /// Initialization error
    InitError,
}

impl std::fmt::Display for GameErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameErrorType::SessionError => write!(f, "session_error"),
            GameErrorType::LlmError => write!(f, "llm_error"),
            GameErrorType::DbError => write!(f, "db_error"),
            GameErrorType::InvalidChoice => write!(f, "invalid_choice"),
            GameErrorType::ConfigError => write!(f, "config_error"),
            GameErrorType::RoundError => write!(f, "round_error"),
            GameErrorType::AlreadySubmitted => write!(f, "already_submitted"),
            GameErrorType::InitError => write!(f, "init_error"),
        }
    }
}

// =============================================================================
// Response Types (AC-5)
// =============================================================================

/// Game session information returned from start_session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameSessionInfo {
    pub id: String,
    pub username: String,
    pub total_answers: u32,
    pub correct_answers: u32,
    pub current_difficulty: f64,
    pub score: f64,
}

/// Game round information (correct_word not exposed to client)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameRoundInfo {
    pub id: String,
    pub phrase: String,
    pub choices: Vec<String>,
}

/// Answer result returned from submit_answer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnswerResultInfo {
    pub is_correct: bool,
    pub correct_word: String,
    pub current_score: f64,
    pub current_difficulty: f64,
}

/// Leaderboard entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeaderboardEntryInfo {
    pub rank: u32,
    pub username: String,
    pub score: f64,
    pub accuracy: f64,
    pub total_answers: u32,
    pub avg_difficulty: f64,
}

// =============================================================================
// WASM Game Engine Wrapper
// =============================================================================

/// WASM-compatible game engine wrapper
///
/// This wraps the game state management logic for browser execution.
/// Since we can't use the native GameEngine directly (it requires DuckDB),
/// we implement a simpler in-memory version for WASM.
pub struct GameEngineWasm {
    /// Current session state
    session: Option<GameSessionState>,

    /// Current round (if any)
    current_round: Option<GameRoundState>,

    /// Leaderboard entries (in-memory)
    leaderboard: Vec<LeaderboardEntryInfo>,

    /// Rolling window of recent answers
    recent_answers: Vec<bool>,

    /// Window size for difficulty adjustment
    difficulty_window_size: usize,
}

/// In-memory session state
#[derive(Debug, Clone)]
struct GameSessionState {
    id: String,
    username: String,
    total_answers: u32,
    correct_answers: u32,
    current_difficulty: f64,
    sum_difficulty: f64,
    submitted: bool,
}

/// In-memory round state
#[derive(Debug, Clone)]
struct GameRoundState {
    id: String,
    phrase: String,
    choices: Vec<String>,
    correct_word: String,
}

impl GameEngineWasm {
    pub fn new() -> Self {
        Self {
            session: None,
            current_round: None,
            leaderboard: Vec::new(),
            recent_answers: Vec::new(),
            difficulty_window_size: 5,
        }
    }

    /// Start a new session
    pub fn start_session(&mut self) -> GameSessionInfo {
        let session_id = uuid::Uuid::new_v4().to_string();
        let username = generate_username();

        let session = GameSessionState {
            id: session_id.clone(),
            username: username.clone(),
            total_answers: 0,
            correct_answers: 0,
            current_difficulty: 0.5,
            sum_difficulty: 0.0,
            submitted: false,
        };

        self.session = Some(session);
        self.current_round = None;
        self.recent_answers.clear();

        GameSessionInfo {
            id: session_id,
            username,
            total_answers: 0,
            correct_answers: 0,
            current_difficulty: 0.5,
            score: 0.0,
        }
    }

    /// Store a round that was generated externally
    pub fn store_round(&mut self, phrase: String, choices: Vec<String>, correct_word: String) -> Result<GameRoundInfo, (String, GameErrorType)> {
        if self.session.is_none() {
            return Err(("No active session - call start_session() first".to_string(), GameErrorType::SessionError));
        }

        let round_id = uuid::Uuid::new_v4().to_string();

        self.current_round = Some(GameRoundState {
            id: round_id.clone(),
            phrase: phrase.clone(),
            choices: choices.clone(),
            correct_word,
        });

        Ok(GameRoundInfo {
            id: round_id,
            phrase,
            choices,
        })
    }

    /// Submit an answer
    pub fn submit_answer(&mut self, choice: &str, _time_ms: u32) -> Result<AnswerResultInfo, (String, GameErrorType)> {
        // Check session and submitted status
        {
            let session = self.session.as_ref().ok_or_else(|| {
                ("No active session - call start_session() first".to_string(), GameErrorType::SessionError)
            })?;

            if session.submitted {
                return Err(("Session has already been submitted to the leaderboard".to_string(), GameErrorType::AlreadySubmitted));
            }
        }

        let round = self.current_round.take().ok_or_else(|| {
            ("No active round - call generate_round() first".to_string(), GameErrorType::RoundError)
        })?;

        // Validate choice
        if !round.choices.contains(&choice.to_string()) {
            // Put round back
            self.current_round = Some(round);
            return Err((format!("Invalid choice: {} is not in the choices list", choice), GameErrorType::InvalidChoice));
        }

        let is_correct = choice == round.correct_word;
        let correct_word = round.correct_word.clone();

        // Update session stats
        {
            let session = self.session.as_mut().unwrap();
            session.total_answers += 1;
            if is_correct {
                session.correct_answers += 1;
            }
            session.sum_difficulty += session.current_difficulty;
        }

        // Update rolling window
        self.recent_answers.push(is_correct);
        if self.recent_answers.len() > self.difficulty_window_size {
            self.recent_answers.remove(0);
        }

        // Adjust difficulty
        self.adjust_difficulty();

        let score = self.calculate_score();
        let current_difficulty = self.session.as_ref().unwrap().current_difficulty;

        Ok(AnswerResultInfo {
            is_correct,
            correct_word,
            current_score: score,
            current_difficulty,
        })
    }

    /// Submit to leaderboard
    pub fn submit_to_leaderboard(&mut self) -> Result<JsonValue, (String, GameErrorType)> {
        // Extract session data first to avoid borrow conflicts
        let (username, total_answers, correct_answers, sum_difficulty) = {
            let session = self.session.as_ref().ok_or_else(|| {
                ("No active session - call start_session() first".to_string(), GameErrorType::SessionError)
            })?;

            if session.submitted {
                return Err(("Session has already been submitted to the leaderboard".to_string(), GameErrorType::AlreadySubmitted));
            }

            if session.total_answers == 0 {
                return Ok(json!({
                    "success": false,
                    "rank": null,
                    "is_new_best": false,
                    "score": 0.0
                }));
            }

            (session.username.clone(), session.total_answers, session.correct_answers, session.sum_difficulty)
        };

        let score = self.calculate_score();
        let accuracy = correct_answers as f64 / total_answers as f64;
        let avg_difficulty = sum_difficulty / total_answers as f64;

        // Find or update leaderboard entry
        let existing_idx = self.leaderboard.iter().position(|e| e.username == username);

        let is_new_best = match existing_idx {
            Some(idx) => {
                if score > self.leaderboard[idx].score {
                    self.leaderboard[idx] = LeaderboardEntryInfo {
                        rank: 0, // Will be recalculated
                        username: username.clone(),
                        score,
                        accuracy,
                        total_answers,
                        avg_difficulty,
                    };
                    true
                } else {
                    false
                }
            }
            None => {
                self.leaderboard.push(LeaderboardEntryInfo {
                    rank: 0,
                    username: username.clone(),
                    score,
                    accuracy,
                    total_answers,
                    avg_difficulty,
                });
                true
            }
        };

        // Sort leaderboard by score descending
        self.leaderboard.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));

        // Update ranks
        for (i, entry) in self.leaderboard.iter_mut().enumerate() {
            entry.rank = (i + 1) as u32;
        }

        // Find current user's rank
        let rank = self.leaderboard.iter().find(|e| e.username == username).map(|e| e.rank);

        // Mark session as submitted
        if let Some(session) = self.session.as_mut() {
            session.submitted = true;
        }

        Ok(json!({
            "success": true,
            "rank": rank,
            "is_new_best": is_new_best,
            "score": score,
            "accuracy": accuracy,
            "total_answers": total_answers,
            "avg_difficulty": avg_difficulty
        }))
    }

    /// Get leaderboard
    pub fn get_leaderboard(&self, limit: u32) -> Vec<LeaderboardEntryInfo> {
        self.leaderboard.iter().take(limit as usize).cloned().collect()
    }

    /// Get current session stats
    pub fn get_session_stats(&self) -> Result<GameSessionInfo, (String, GameErrorType)> {
        let session = self.session.as_ref().ok_or_else(|| {
            ("No active session - call start_session() first".to_string(), GameErrorType::SessionError)
        })?;

        let score = self.calculate_score();

        Ok(GameSessionInfo {
            id: session.id.clone(),
            username: session.username.clone(),
            total_answers: session.total_answers,
            correct_answers: session.correct_answers,
            current_difficulty: session.current_difficulty,
            score,
        })
    }

    /// Calculate current score
    fn calculate_score(&self) -> f64 {
        let session = match &self.session {
            Some(s) => s,
            None => return 0.0,
        };

        if session.total_answers == 0 {
            return 0.0;
        }

        let accuracy = session.correct_answers as f64 / session.total_answers as f64;
        let avg_difficulty = session.sum_difficulty / session.total_answers as f64;
        let answer_factor = ((session.total_answers as f64 + 1.0).log2() / 50_f64.log2()).min(1.0);

        accuracy * avg_difficulty * answer_factor
    }

    /// Adjust difficulty based on recent performance
    fn adjust_difficulty(&mut self) {
        if self.recent_answers.is_empty() {
            return;
        }

        let session = match &mut self.session {
            Some(s) => s,
            None => return,
        };

        let correct_count = self.recent_answers.iter().filter(|&&c| c).count();
        let accuracy = correct_count as f64 / self.recent_answers.len() as f64;

        if accuracy > 0.8 {
            session.current_difficulty = (session.current_difficulty + 0.05).min(0.95);
        } else if accuracy < 0.4 {
            session.current_difficulty = (session.current_difficulty - 0.05).max(0.1);
        }
    }

    /// Get current difficulty for round generation
    pub fn get_current_difficulty(&self) -> f64 {
        self.session.as_ref().map(|s| s.current_difficulty).unwrap_or(0.5)
    }
}

impl Default for GameEngineWasm {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Username Generation (matches games/mod.rs)
// =============================================================================

const ADJECTIVES: &[&str] = &[
    "Swift", "Brave", "Clever", "Quick", "Wise", "Bold", "Sharp", "Keen", "Nimble", "Bright",
];

const ANIMALS: &[&str] = &[
    "Fox", "Owl", "Wolf", "Raven", "Tiger", "Eagle", "Hawk", "Bear", "Deer", "Lion",
];

fn generate_username() -> String {
    let mut rng = rand::thread_rng();
    let adjective = ADJECTIVES[rng.gen_range(0..ADJECTIVES.len())];
    let animal = ANIMALS[rng.gen_range(0..ANIMALS.len())];
    let number: u8 = rng.gen_range(0..100);
    format!("{}{}{:02}", adjective, animal, number)
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Create a success JSON response
fn success_response<T: Serialize>(data: T) -> String {
    json!({
        "success": true,
        "data": data
    }).to_string()
}

/// Create an error JSON response (AC-6)
fn error_response(message: &str, error_type: GameErrorType) -> String {
    json!({
        "success": false,
        "error": message,
        "error_type": error_type.to_string()
    }).to_string()
}

// =============================================================================
// WASM Exports (AC-1)
// =============================================================================

/// Initialize the game engine
///
/// Must be called before any other game functions.
/// This creates the in-memory game engine instance.
#[wasm_bindgen]
pub fn game_init() -> String {
    GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        *engine = Some(GameEngineWasm::new());
    });

    success_response(json!({"initialized": true}))
}

/// Set the LLM callback for phrase generation (AC-3)
///
/// The callback should be an async function that takes a JSON string
/// with the prompt and returns a JSON string with the response.
#[wasm_bindgen]
pub fn game_set_llm_handler(callback: js_sys::Function) {
    LLM_CALLBACK.with(|cb| {
        *cb.borrow_mut() = Some(callback);
    });
}

/// Clear the LLM callback
#[wasm_bindgen]
pub fn game_clear_llm_handler() {
    LLM_CALLBACK.with(|cb| {
        *cb.borrow_mut() = None;
    });
}

/// Check if LLM handler is set
#[wasm_bindgen]
pub fn game_has_llm_handler() -> bool {
    LLM_CALLBACK.with(|cb| cb.borrow().is_some())
}

// Note: Opik handler functions are re-exported from game_opik module.
// Use: set_game_opik_handler, clear_game_opik_handler, has_game_opik_handler
// from the game_opik module directly.

/// Start a new game session (AC-1)
///
/// Returns a Promise that resolves to a JSON string with session info:
/// ```json
/// {"success": true, "data": {"id": "...", "username": "SwiftFox42", ...}}
/// ```
#[wasm_bindgen]
pub fn game_start_session() -> String {
    GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();

        // Initialize if needed
        if engine.is_none() {
            *engine = Some(GameEngineWasm::new());
        }

        let engine = engine.as_mut().unwrap();
        let session = engine.start_session();

        // Send Opik span if handler is set (AC-2: Session-level trace)
        if has_game_opik_handler() {
            let span = OpikGameSpan::new_session(
                &session.id,
                json!({
                    "username": session.username,
                    "started_at": js_sys::Date::new_0().to_iso_string().as_string().unwrap_or_default(),
                    "project_name": "know-your-model",
                    "initial_difficulty": session.current_difficulty,
                }),
            );
            send_game_opik_span(&span);
        }

        success_response(session)
    })
}

/// Generate a new game round (AC-1)
///
/// This function requires an LLM callback to be set via `game_set_llm_handler`.
/// The LLM is called to generate a phrase with a missing word.
///
/// Returns a Promise that resolves to a JSON string with round info:
/// ```json
/// {"success": true, "data": {"id": "...", "phrase": "The ___ is bright.", "choices": [...]}}
/// ```
#[wasm_bindgen]
pub async fn game_generate_round() -> String {
    // Check if LLM handler is set
    let has_llm = LLM_CALLBACK.with(|cb| cb.borrow().is_some());
    if !has_llm {
        return error_response("LLM handler not set - call game_set_llm_handler() first", GameErrorType::LlmError);
    }

    // Check if session exists
    let difficulty = GAME_ENGINE.with(|engine| {
        let engine = engine.borrow();
        match engine.as_ref() {
            Some(e) => Ok(e.get_current_difficulty()),
            None => Err(error_response("No active session - call game_start_session() first", GameErrorType::SessionError)),
        }
    });

    let difficulty = match difficulty {
        Ok(d) => d,
        Err(e) => return e,
    };

    // Call LLM to generate phrase
    let llm_result = LLM_CALLBACK.with(|cb| {
        let callback = cb.borrow();
        let callback = match callback.as_ref() {
            Some(c) => c,
            None => return Err("LLM handler not set".to_string()),
        };

        // Build prompt for phrase generation
        let prompt = json!({
            "type": "generate_phrase",
            "difficulty": difficulty,
            "instruction": "Generate a phrase with a missing word. Return JSON with 'phrase' (containing ___ where the word goes), 'word' (the correct word), and 'distractors' (4 similar words)."
        });

        // Call the JS callback
        let result = callback.call1(&JsValue::NULL, &JsValue::from_str(&prompt.to_string()));

        match result {
            Ok(val) => {
                if let Some(promise) = val.dyn_ref::<js_sys::Promise>() {
                    Ok(promise.clone())
                } else if let Some(s) = val.as_string() {
                    // Synchronous result
                    Ok(js_sys::Promise::resolve(&JsValue::from_str(&s)))
                } else {
                    Err("LLM callback returned invalid type".to_string())
                }
            }
            Err(e) => Err(format!("LLM callback error: {:?}", e)),
        }
    });

    let llm_promise = match llm_result {
        Ok(p) => p,
        Err(e) => return error_response(&e, GameErrorType::LlmError),
    };

    // Await the promise
    let llm_response = wasm_bindgen_futures::JsFuture::from(llm_promise).await;

    let response_json = match llm_response {
        Ok(val) => {
            if let Some(s) = val.as_string() {
                s
            } else {
                return error_response("LLM callback returned non-string", GameErrorType::LlmError);
            }
        }
        Err(e) => return error_response(&format!("LLM promise rejected: {:?}", e), GameErrorType::LlmError),
    };

    // Parse LLM response
    let llm_data: JsonValue = match serde_json::from_str(&response_json) {
        Ok(d) => d,
        Err(e) => return error_response(&format!("Failed to parse LLM response: {}", e), GameErrorType::LlmError),
    };

    // Extract phrase, word, and distractors
    let phrase = match llm_data.get("phrase").and_then(|v| v.as_str()) {
        Some(p) => p.to_string(),
        None => return error_response("LLM response missing 'phrase' field", GameErrorType::LlmError),
    };

    let word = match llm_data.get("word").and_then(|v| v.as_str()) {
        Some(w) => w.to_string(),
        None => return error_response("LLM response missing 'word' field", GameErrorType::LlmError),
    };

    let distractors: Vec<String> = match llm_data.get("distractors").and_then(|v| v.as_array()) {
        Some(arr) => arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect(),
        None => return error_response("LLM response missing 'distractors' field", GameErrorType::LlmError),
    };

    if distractors.len() < 4 {
        return error_response(&format!("LLM returned only {} distractors, need 4", distractors.len()), GameErrorType::LlmError);
    }

    // Combine and shuffle choices
    let mut choices: Vec<String> = distractors.into_iter().take(4).collect();
    choices.push(word.clone());

    // Shuffle using Fisher-Yates
    use rand::seq::SliceRandom;
    choices.shuffle(&mut rand::thread_rng());

    // Store round in engine
    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        let engine = match engine.as_mut() {
            Some(e) => e,
            None => return Err(error_response("Engine not initialized", GameErrorType::InitError)),
        };

        match engine.store_round(phrase, choices, word) {
            Ok(round) => Ok(round),
            Err((msg, err_type)) => Err(error_response(&msg, err_type)),
        }
    });

    match result {
        Ok(round) => {
            // Get session ID for parent span linking
            let session_id = GAME_ENGINE.with(|engine| {
                engine.borrow().as_ref().and_then(|e| e.session.as_ref().map(|s| s.id.clone()))
            });

            // Send Opik span for round (AC-1: Round trace with required fields)
            if has_game_opik_handler() {
                let span = OpikGameSpan::new_round(
                    &round.id,
                    session_id,
                    json!({
                        "round_number": GAME_ENGINE.with(|engine| {
                            engine.borrow().as_ref().map(|e| e.session.as_ref().map(|s| s.total_answers + 1).unwrap_or(1)).unwrap_or(1)
                        }),
                        "phrase": round.phrase,
                        "choices": round.choices,
                        "difficulty": difficulty,
                    }),
                );
                send_game_opik_span(&span);
            }

            success_response(round)
        }
        Err(e) => e,
    }
}

/// Submit an answer for the current round (AC-1)
///
/// # Arguments
/// * `choice` - The word the player selected
/// * `time_ms` - Time taken to answer in milliseconds
///
/// Returns a JSON string with the result:
/// ```json
/// {"success": true, "data": {"is_correct": true, "correct_word": "sun", ...}}
/// ```
#[wasm_bindgen]
pub fn game_submit_answer(choice: &str, time_ms: u32) -> String {
    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        let engine = match engine.as_mut() {
            Some(e) => e,
            None => return Err(error_response("No active session - call game_start_session() first", GameErrorType::SessionError)),
        };

        match engine.submit_answer(choice, time_ms) {
            Ok(result) => Ok(result),
            Err((msg, err_type)) => Err(error_response(&msg, err_type)),
        }
    });

    match result {
        Ok(answer_result) => {
            // Get session info for the span
            let (session_id, session_stats) = GAME_ENGINE.with(|engine| {
                let engine = engine.borrow();
                match engine.as_ref() {
                    Some(e) => {
                        let session = e.session.as_ref();
                        (
                            session.map(|s| s.id.clone()),
                            session.map(|s| (s.total_answers, s.correct_answers)),
                        )
                    }
                    None => (None, None),
                }
            });

            // Send Opik span for answer (AC-1: Round trace with all required fields)
            if has_game_opik_handler() {
                let running_accuracy = session_stats
                    .map(|(total, correct)| {
                        if total > 0 {
                            correct as f64 / total as f64
                        } else {
                            0.0
                        }
                    })
                    .unwrap_or(0.0);

                let mut span = OpikGameSpan::new_round(
                    &uuid::Uuid::new_v4().to_string(),
                    session_id,
                    json!({
                        "selected_word": choice,
                        "correct_word": answer_result.correct_word,
                        "is_correct": answer_result.is_correct,
                        "response_time_ms": time_ms,
                        "difficulty": answer_result.current_difficulty,
                        "running_score": answer_result.current_score,
                        "running_accuracy": running_accuracy,
                    }),
                );
                span.complete_with(
                    if answer_result.is_correct { "success" } else { "failure" },
                    Some(time_ms as f64),
                );
                send_game_opik_span(&span);
            }

            success_response(answer_result)
        }
        Err(e) => e,
    }
}

/// Submit the current session to the leaderboard (AC-1)
///
/// Returns a JSON string with the result:
/// ```json
/// {"success": true, "data": {"rank": 5, "is_new_best": true, "score": 0.75}}
/// ```
#[wasm_bindgen]
pub fn game_submit_to_leaderboard() -> String {
    let result = GAME_ENGINE.with(|engine| {
        let mut engine = engine.borrow_mut();
        let engine = match engine.as_mut() {
            Some(e) => e,
            None => return Err(error_response("No active session - call game_start_session() first", GameErrorType::SessionError)),
        };

        match engine.submit_to_leaderboard() {
            Ok(result) => Ok(result),
            Err((msg, err_type)) => Err(error_response(&msg, err_type)),
        }
    });

    match result {
        Ok(leaderboard_result) => {
            // Get session info for the span
            let session_info = GAME_ENGINE.with(|engine| {
                let engine = engine.borrow();
                engine.as_ref().and_then(|e| {
                    e.session.as_ref().map(|s| (s.id.clone(), s.username.clone()))
                })
            });

            // Send Opik span for leaderboard submission (AC-4)
            if has_game_opik_handler() {
                if let Some((session_id, username)) = session_info {
                    let mut span = OpikGameSpan::new_leaderboard_submit(
                        &session_id,
                        json!({
                            "username": username,
                            "final_score": leaderboard_result["score"],
                            "accuracy": leaderboard_result["accuracy"],
                            "total_answers": leaderboard_result["total_answers"],
                            "avg_difficulty": leaderboard_result["avg_difficulty"],
                            "rank": leaderboard_result["rank"],
                            "is_new_best": leaderboard_result["is_new_best"],
                        }),
                    );
                    span.complete();
                    send_game_opik_span(&span);
                }
            }

            // The leaderboard result is already a JsonValue with success field
            leaderboard_result.to_string()
        }
        Err(e) => e,
    }
}

/// Get the leaderboard (AC-1)
///
/// # Arguments
/// * `limit` - Maximum number of entries to return (default: 10)
///
/// Returns a JSON string with the leaderboard:
/// ```json
/// {"success": true, "data": [{"rank": 1, "username": "SwiftFox42", "score": 0.95, ...}]}
/// ```
#[wasm_bindgen]
pub fn game_get_leaderboard(limit: u32) -> String {
    let entries = GAME_ENGINE.with(|engine| {
        let engine = engine.borrow();
        match engine.as_ref() {
            Some(e) => e.get_leaderboard(if limit == 0 { 10 } else { limit }),
            None => Vec::new(),
        }
    });

    success_response(entries)
}

/// Get current session stats (AC-1)
///
/// Returns a JSON string with session statistics:
/// ```json
/// {"success": true, "data": {"id": "...", "username": "...", "score": 0.5, ...}}
/// ```
#[wasm_bindgen]
pub fn game_get_session_stats() -> String {
    let result = GAME_ENGINE.with(|engine| {
        let engine = engine.borrow();
        let engine = match engine.as_ref() {
            Some(e) => e,
            None => return Err(error_response("No active session - call game_start_session() first", GameErrorType::SessionError)),
        };

        match engine.get_session_stats() {
            Ok(stats) => Ok(stats),
            Err((msg, err_type)) => Err(error_response(&msg, err_type)),
        }
    });

    match result {
        Ok(stats) => success_response(stats),
        Err(e) => e,
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_game_engine_wasm_new() {
        let engine = GameEngineWasm::new();
        assert!(engine.session.is_none());
        assert!(engine.current_round.is_none());
        assert!(engine.leaderboard.is_empty());
    }

    #[test]
    fn test_start_session() {
        let mut engine = GameEngineWasm::new();
        let session = engine.start_session();

        assert!(!session.id.is_empty());
        assert!(!session.username.is_empty());
        assert_eq!(session.total_answers, 0);
        assert_eq!(session.correct_answers, 0);
        assert!((session.current_difficulty - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn test_username_format() {
        for _ in 0..10 {
            let username = generate_username();
            // Should be at least 9 characters (shortest adjective + animal + 2 digits)
            assert!(username.len() >= 9);
            // Should end with 2 digits
            assert!(username.chars().rev().take(2).all(|c| c.is_ascii_digit()));
        }
    }

    #[test]
    fn test_store_round() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();

        let result = engine.store_round(
            "The ___ is bright.".to_string(),
            vec!["sun".to_string(), "moon".to_string(), "star".to_string(), "sky".to_string(), "day".to_string()],
            "sun".to_string(),
        );

        assert!(result.is_ok());
        let round = result.unwrap();
        assert_eq!(round.phrase, "The ___ is bright.");
        assert_eq!(round.choices.len(), 5);
    }

    #[test]
    fn test_store_round_requires_session() {
        let mut engine = GameEngineWasm::new();

        let result = engine.store_round(
            "Test".to_string(),
            vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()],
            "a".to_string(),
        );

        assert!(result.is_err());
        let (_, err_type) = result.unwrap_err();
        assert_eq!(err_type, GameErrorType::SessionError);
    }

    #[test]
    fn test_submit_correct_answer() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();
        engine.store_round(
            "The ___ is bright.".to_string(),
            vec!["sun".to_string(), "moon".to_string(), "star".to_string(), "sky".to_string(), "day".to_string()],
            "sun".to_string(),
        ).unwrap();

        let result = engine.submit_answer("sun", 1000);
        assert!(result.is_ok());

        let answer = result.unwrap();
        assert!(answer.is_correct);
        assert_eq!(answer.correct_word, "sun");
    }

    #[test]
    fn test_submit_incorrect_answer() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();
        engine.store_round(
            "The ___ is bright.".to_string(),
            vec!["sun".to_string(), "moon".to_string(), "star".to_string(), "sky".to_string(), "day".to_string()],
            "sun".to_string(),
        ).unwrap();

        let result = engine.submit_answer("moon", 1000);
        assert!(result.is_ok());

        let answer = result.unwrap();
        assert!(!answer.is_correct);
        assert_eq!(answer.correct_word, "sun");
    }

    #[test]
    fn test_submit_invalid_choice() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();
        engine.store_round(
            "The ___ is bright.".to_string(),
            vec!["sun".to_string(), "moon".to_string(), "star".to_string(), "sky".to_string(), "day".to_string()],
            "sun".to_string(),
        ).unwrap();

        let result = engine.submit_answer("invalid", 1000);
        assert!(result.is_err());

        let (_, err_type) = result.unwrap_err();
        assert_eq!(err_type, GameErrorType::InvalidChoice);
    }

    #[test]
    fn test_difficulty_adjustment() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();

        let initial_difficulty = engine.get_current_difficulty();

        // Answer 5 rounds correctly
        for _ in 0..5 {
            engine.store_round(
                "Test".to_string(),
                vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()],
                "a".to_string(),
            ).unwrap();
            engine.submit_answer("a", 1000).unwrap();
        }

        // Difficulty should have increased
        assert!(engine.get_current_difficulty() > initial_difficulty);
    }

    #[test]
    fn test_leaderboard_submission() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();

        // Play a round
        engine.store_round(
            "Test".to_string(),
            vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()],
            "a".to_string(),
        ).unwrap();
        engine.submit_answer("a", 1000).unwrap();

        // Submit to leaderboard
        let result = engine.submit_to_leaderboard();
        assert!(result.is_ok());

        let leaderboard_result = result.unwrap();
        assert_eq!(leaderboard_result["success"], true);
    }

    #[test]
    fn test_double_submission_blocked() {
        let mut engine = GameEngineWasm::new();
        engine.start_session();

        engine.store_round(
            "Test".to_string(),
            vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()],
            "a".to_string(),
        ).unwrap();
        engine.submit_answer("a", 1000).unwrap();

        // First submission
        engine.submit_to_leaderboard().unwrap();

        // Second submission should fail
        let result = engine.submit_to_leaderboard();
        assert!(result.is_err());

        let (_, err_type) = result.unwrap_err();
        assert_eq!(err_type, GameErrorType::AlreadySubmitted);
    }

    #[test]
    fn test_get_leaderboard() {
        let mut engine = GameEngineWasm::new();

        // Play and submit for multiple sessions
        for _ in 0..3 {
            engine.start_session();
            engine.store_round(
                "Test".to_string(),
                vec!["a".to_string(), "b".to_string(), "c".to_string(), "d".to_string(), "e".to_string()],
                "a".to_string(),
            ).unwrap();
            engine.submit_answer("a", 1000).unwrap();
            engine.submit_to_leaderboard().unwrap();
        }

        let leaderboard = engine.get_leaderboard(10);
        assert!(!leaderboard.is_empty());
        // Ranks should be sequential
        for (i, entry) in leaderboard.iter().enumerate() {
            assert_eq!(entry.rank, (i + 1) as u32);
        }
    }

    #[test]
    fn test_error_response_format() {
        let response = error_response("Test error", GameErrorType::SessionError);
        let parsed: JsonValue = serde_json::from_str(&response).unwrap();

        assert_eq!(parsed["success"], false);
        assert_eq!(parsed["error"], "Test error");
        assert_eq!(parsed["error_type"], "session_error");
    }

    #[test]
    fn test_success_response_format() {
        let response = success_response(json!({"test": "data"}));
        let parsed: JsonValue = serde_json::from_str(&response).unwrap();

        assert_eq!(parsed["success"], true);
        assert_eq!(parsed["data"]["test"], "data");
    }
}
