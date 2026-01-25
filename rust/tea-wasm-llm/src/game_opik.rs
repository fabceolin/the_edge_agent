//! Game-specific Opik Tracing for WASM (TEA-GAME-001.8)
//!
//! This module provides Opik game spans compatible with the native implementation
//! in `rust/src/games/opik.rs`. It uses a separate callback from the general
//! `opik.rs` module to allow independent configuration.
//!
//! # Architecture
//!
//! Uses a callback pattern to interface with JavaScript's Opik client:
//! 1. JavaScript registers a handler via `set_game_opik_handler()`
//! 2. Rust fires spans via `send_game_opik_span()` (fire-and-forget)
//! 3. The JS handler sends spans to Opik REST API
//!
//! # Graceful Degradation (AC-6)
//!
//! If no handler is registered, tracing is silently skipped without errors.
//! This allows the game to function normally when Opik is not configured.
//!
//! # Example
//!
//! ```javascript
//! import { set_game_opik_handler, game_start_session } from './pkg/tea_wasm_llm.js';
//!
//! // Register Opik handler for game spans
//! set_game_opik_handler((spanJson) => {
//!     const span = JSON.parse(spanJson);
//!     console.log('Game span:', span.span_type, span.name);
//!     // Send to Opik backend...
//! });
//!
//! // Start game - spans will be sent automatically
//! game_start_session();
//! ```

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

// =============================================================================
// Span Types and Structures (AC-1, AC-2, AC-3, AC-4)
// =============================================================================

/// Types of game spans for Opik tracing.
///
/// Each span type represents a different level of game observability:
/// - `Session`: Top-level trace grouping all rounds in a game session
/// - `Round`: Individual round with phrase, choices, and answer data
/// - `LlmPhrase`: LLM call for phrase generation with token usage
/// - `LeaderboardSubmit`: Final score submission to leaderboard
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum GameSpanType {
    /// Session-level trace (AC-2)
    Session,
    /// Individual game round (AC-1)
    Round,
    /// LLM phrase generation call (AC-3)
    LlmPhrase,
    /// Leaderboard submission (AC-4)
    LeaderboardSubmit,
}

impl std::fmt::Display for GameSpanType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameSpanType::Session => write!(f, "session"),
            GameSpanType::Round => write!(f, "round"),
            GameSpanType::LlmPhrase => write!(f, "llm_phrase"),
            GameSpanType::LeaderboardSubmit => write!(f, "leaderboard_submit"),
        }
    }
}

/// Opik game span structure matching Opik REST API schema.
///
/// This structure captures all required fields for game tracing (AC-1):
/// - `phrase`: The sentence with the target word
/// - `choices`: Array of word options
/// - `correct_word`: The correct answer
/// - `selected_word`: The player's selection
/// - `is_correct`: Whether the answer was correct
/// - `response_time_ms`: Time taken to answer
/// - `difficulty`: Current difficulty level
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpikGameSpan {
    /// Unique identifier for this span
    pub span_id: String,

    /// Parent span ID (session ID for rounds, round ID for LLM calls)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,

    /// Trace ID (typically the session ID for correlation)
    pub trace_id: String,

    /// Human-readable span name
    pub name: String,

    /// Type of game span
    pub span_type: GameSpanType,

    /// Unix timestamp (seconds with microsecond precision) when span started
    pub start_time: f64,

    /// Unix timestamp when span ended (None if still running)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_time: Option<f64>,

    /// Duration in milliseconds (None if still running)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<f64>,

    /// Status: "success", "failure", "running", or "error"
    pub status: String,

    /// Additional metadata (varies by span type)
    pub metadata: serde_json::Value,
}

impl OpikGameSpan {
    /// Create a new session span (AC-2).
    pub fn new_session(session_id: &str, metadata: serde_json::Value) -> Self {
        Self {
            span_id: session_id.to_string(),
            parent_id: None,
            trace_id: session_id.to_string(),
            name: "game_session".to_string(),
            span_type: GameSpanType::Session,
            start_time: current_timestamp(),
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            metadata,
        }
    }

    /// Create a new round span (AC-1).
    pub fn new_round(
        round_id: &str,
        session_id: Option<String>,
        metadata: serde_json::Value,
    ) -> Self {
        Self {
            span_id: round_id.to_string(),
            parent_id: session_id.clone(),
            trace_id: session_id.unwrap_or_else(|| round_id.to_string()),
            name: "game_round".to_string(),
            span_type: GameSpanType::Round,
            start_time: current_timestamp(),
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            metadata,
        }
    }

    /// Create a new LLM phrase generation span (AC-3).
    pub fn new_llm_phrase(
        round_id: &str,
        session_id: Option<String>,
        metadata: serde_json::Value,
    ) -> Self {
        Self {
            span_id: uuid::Uuid::new_v4().to_string(),
            parent_id: Some(round_id.to_string()),
            trace_id: session_id.unwrap_or_else(|| round_id.to_string()),
            name: "phrase_generation".to_string(),
            span_type: GameSpanType::LlmPhrase,
            start_time: current_timestamp(),
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            metadata,
        }
    }

    /// Create a new leaderboard submit span (AC-4).
    pub fn new_leaderboard_submit(session_id: &str, metadata: serde_json::Value) -> Self {
        Self {
            span_id: uuid::Uuid::new_v4().to_string(),
            parent_id: Some(session_id.to_string()),
            trace_id: session_id.to_string(),
            name: "leaderboard_submit".to_string(),
            span_type: GameSpanType::LeaderboardSubmit,
            start_time: current_timestamp(),
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            metadata,
        }
    }

    /// Mark span as completed with success status.
    pub fn complete(&mut self) {
        let end = current_timestamp();
        self.end_time = Some(end);
        self.duration_ms = Some((end - self.start_time) * 1000.0);
        self.status = "success".to_string();
    }

    /// Mark span as completed with failure status.
    pub fn fail(&mut self) {
        let end = current_timestamp();
        self.end_time = Some(end);
        self.duration_ms = Some((end - self.start_time) * 1000.0);
        self.status = "failure".to_string();
    }

    /// Mark span as completed with a specific status and duration.
    pub fn complete_with(&mut self, status: &str, duration_ms: Option<f64>) {
        self.end_time = Some(current_timestamp());
        self.duration_ms = duration_ms;
        self.status = status.to_string();
    }
}

// =============================================================================
// WASM Callback Bridge (AC-5, AC-6)
// =============================================================================

// Thread-local storage for the game Opik callback
thread_local! {
    static GAME_OPIK_HANDLER: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
    static GAME_OPIK_CONFIG: RefCell<GameOpikConfig> = RefCell::new(GameOpikConfig::default());
}

/// Game Opik configuration.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GameOpikConfig {
    /// Project name for traces (defaults to "know-your-model")
    #[serde(default)]
    pub project_name: Option<String>,

    /// Whether tracing is enabled
    #[serde(default)]
    pub enabled: bool,
}

/// Set the game Opik handler (WASM export) (AC-5).
///
/// The handler should accept a JSON string (OpikGameSpan) and optionally
/// return a Promise for async sending.
#[wasm_bindgen]
pub fn set_game_opik_handler(handler: js_sys::Function) {
    GAME_OPIK_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    web_sys::console::log_1(&"[TEA-GAME-OPIK] Handler registered".into());
}

/// Clear the game Opik handler (WASM export).
#[wasm_bindgen]
pub fn clear_game_opik_handler() {
    GAME_OPIK_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-GAME-OPIK] Handler cleared".into());
}

/// Check if a game Opik handler is registered (WASM export) (AC-6).
#[wasm_bindgen]
pub fn has_game_opik_handler() -> bool {
    GAME_OPIK_HANDLER.with(|h| h.borrow().is_some())
}

/// Configure game Opik settings (WASM export).
#[wasm_bindgen]
pub fn configure_game_opik(config_json: &str) -> Result<(), JsValue> {
    let config: GameOpikConfig = serde_json::from_str(config_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid game Opik config: {}", e)))?;

    GAME_OPIK_CONFIG.with(|c| {
        *c.borrow_mut() = config;
    });

    web_sys::console::log_1(&"[TEA-GAME-OPIK] Configuration updated".into());
    Ok(())
}

/// Get current game Opik configuration as JSON (WASM export).
#[wasm_bindgen]
pub fn get_game_opik_config() -> String {
    GAME_OPIK_CONFIG.with(|c| {
        serde_json::to_string(&*c.borrow()).unwrap_or_else(|_| "{}".to_string())
    })
}

/// Check if game Opik tracing is enabled (AC-6).
pub fn is_game_opik_enabled() -> bool {
    has_game_opik_handler()
}

/// Send a game span to Opik (fire-and-forget) (AC-6).
///
/// This function implements graceful degradation:
/// - If no handler is registered, silently returns
/// - If handler fails, logs warning but doesn't fail
pub fn send_game_opik_span(span: &OpikGameSpan) {
    if !has_game_opik_handler() {
        return;
    }

    GAME_OPIK_HANDLER.with(|h| {
        if let Some(ref handler) = *h.borrow() {
            let span_json = match serde_json::to_string(span) {
                Ok(json) => json,
                Err(e) => {
                    web_sys::console::warn_1(
                        &format!("[TEA-GAME-OPIK] Failed to serialize span: {}", e).into(),
                    );
                    return;
                }
            };

            let this = JsValue::NULL;
            let span_js = JsValue::from_str(&span_json);

            match handler.call1(&this, &span_js) {
                Ok(_) => {
                    web_sys::console::log_1(
                        &format!("[TEA-GAME-OPIK] Span sent: {} ({})", span.name, span.span_type)
                            .into(),
                    );
                }
                Err(e) => {
                    // Log error but don't fail - graceful degradation
                    web_sys::console::warn_1(
                        &format!("[TEA-GAME-OPIK] Failed to send span: {:?}", e).into(),
                    );
                }
            }
        }
    });
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get current Unix timestamp with millisecond precision.
/// Uses JavaScript Date in WASM, std::time in native tests.
#[cfg(target_arch = "wasm32")]
pub fn current_timestamp() -> f64 {
    js_sys::Date::now() / 1000.0
}

/// Get current Unix timestamp with millisecond precision (native test fallback).
#[cfg(not(target_arch = "wasm32"))]
pub fn current_timestamp() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

/// Get current ISO 8601 timestamp string.
#[cfg(target_arch = "wasm32")]
pub fn current_iso_timestamp() -> String {
    js_sys::Date::new_0()
        .to_iso_string()
        .as_string()
        .unwrap_or_default()
}

/// Get current ISO 8601 timestamp string (native test fallback).
#[cfg(not(target_arch = "wasm32"))]
pub fn current_iso_timestamp() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    format!("{}", duration.as_secs())
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_game_span_type_serialization() {
        assert_eq!(
            serde_json::to_string(&GameSpanType::Session).unwrap(),
            "\"session\""
        );
        assert_eq!(
            serde_json::to_string(&GameSpanType::Round).unwrap(),
            "\"round\""
        );
        assert_eq!(
            serde_json::to_string(&GameSpanType::LlmPhrase).unwrap(),
            "\"llm_phrase\""
        );
        assert_eq!(
            serde_json::to_string(&GameSpanType::LeaderboardSubmit).unwrap(),
            "\"leaderboard_submit\""
        );
    }

    #[test]
    fn test_game_span_type_display() {
        assert_eq!(format!("{}", GameSpanType::Session), "session");
        assert_eq!(format!("{}", GameSpanType::Round), "round");
        assert_eq!(format!("{}", GameSpanType::LlmPhrase), "llm_phrase");
        assert_eq!(
            format!("{}", GameSpanType::LeaderboardSubmit),
            "leaderboard_submit"
        );
    }

    #[test]
    fn test_opik_game_span_session() {
        let span = OpikGameSpan::new_session(
            "sess_abc123",
            serde_json::json!({
                "username": "SwiftFox42",
                "project_name": "know-your-model"
            }),
        );

        assert_eq!(span.span_id, "sess_abc123");
        assert_eq!(span.trace_id, "sess_abc123");
        assert!(span.parent_id.is_none());
        assert_eq!(span.name, "game_session");
        assert_eq!(span.span_type, GameSpanType::Session);
        assert_eq!(span.status, "running");
    }

    #[test]
    fn test_opik_game_span_round() {
        let span = OpikGameSpan::new_round(
            "round_xyz789",
            Some("sess_abc123".to_string()),
            serde_json::json!({
                "phrase": "The ___ shines brightly.",
                "choices": ["sun", "moon", "star"],
                "correct_word": "sun"
            }),
        );

        assert_eq!(span.span_id, "round_xyz789");
        assert_eq!(span.trace_id, "sess_abc123");
        assert_eq!(span.parent_id, Some("sess_abc123".to_string()));
        assert_eq!(span.name, "game_round");
        assert_eq!(span.span_type, GameSpanType::Round);
    }

    #[test]
    fn test_opik_game_span_llm_phrase() {
        let span = OpikGameSpan::new_llm_phrase(
            "round_xyz789",
            Some("sess_abc123".to_string()),
            serde_json::json!({
                "model": "gemma-3-1b-it",
                "prompt_tokens": 150
            }),
        );

        assert_eq!(span.trace_id, "sess_abc123");
        assert_eq!(span.parent_id, Some("round_xyz789".to_string()));
        assert_eq!(span.name, "phrase_generation");
        assert_eq!(span.span_type, GameSpanType::LlmPhrase);
    }

    #[test]
    fn test_opik_game_span_leaderboard_submit() {
        let span = OpikGameSpan::new_leaderboard_submit(
            "sess_abc123",
            serde_json::json!({
                "username": "SwiftFox42",
                "final_score": 0.42,
                "rank": 3
            }),
        );

        assert_eq!(span.trace_id, "sess_abc123");
        assert_eq!(span.parent_id, Some("sess_abc123".to_string()));
        assert_eq!(span.name, "leaderboard_submit");
        assert_eq!(span.span_type, GameSpanType::LeaderboardSubmit);
    }

    #[test]
    fn test_opik_game_span_complete() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));
        assert_eq!(span.status, "running");
        assert!(span.end_time.is_none());

        span.complete();

        assert_eq!(span.status, "success");
        assert!(span.end_time.is_some());
        assert!(span.duration_ms.is_some());
    }

    #[test]
    fn test_opik_game_span_fail() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        span.fail();

        assert_eq!(span.status, "failure");
        assert!(span.end_time.is_some());
    }

    #[test]
    fn test_opik_game_span_complete_with() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        span.complete_with("timeout", Some(5000.0));

        assert_eq!(span.status, "timeout");
        assert_eq!(span.duration_ms, Some(5000.0));
    }

    #[test]
    fn test_game_opik_config_default() {
        let config = GameOpikConfig::default();
        assert!(config.project_name.is_none());
        assert!(!config.enabled);
    }
}
