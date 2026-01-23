//! Opik Integration for Game Tracing (TEA-GAME-001.8)
//!
//! This module provides Opik observability integration for game events:
//! - Session-level traces that group all rounds
//! - Round-level traces with detailed game metrics
//! - LLM phrase generation traces with token usage
//! - Leaderboard submission traces
//!
//! # Architecture
//!
//! Uses a callback pattern to interface with JavaScript's Opik client:
//! 1. JavaScript registers a handler via `set_game_opik_handler()`
//! 2. Rust fires spans via `send_game_span()` (fire-and-forget)
//! 3. The JS handler sends spans to Opik REST API
//!
//! # Graceful Degradation
//!
//! If no handler is registered, tracing is silently skipped without errors.
//! This allows the game to function normally when Opik is not configured.
//!
//! # Example
//!
//! ```ignore
//! use the_edge_agent::games::opik::{
//!     OpikGameSpan, GameSpanType, send_game_span, has_game_opik_handler,
//! };
//!
//! // Create a round span
//! let span = OpikGameSpan::new_round(
//!     "round_123",
//!     Some("session_456".to_string()),
//!     serde_json::json!({
//!         "phrase": "The ___ shines brightly.",
//!         "choices": ["sun", "moon", "star"],
//!         "correct_word": "sun",
//!         "selected_word": "moon",
//!         "is_correct": false,
//!         "response_time_ms": 2340,
//!         "difficulty": 0.65,
//!     }),
//! );
//!
//! // Send span (fire-and-forget)
//! send_game_span(&span);
//! ```

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

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
///
/// # Example JSON Output
///
/// ```json
/// {
///   "span_id": "abc123",
///   "parent_id": "session_456",
///   "trace_id": "session_456",
///   "name": "game_round",
///   "span_type": "round",
///   "start_time": 1705323045.123,
///   "end_time": 1705323047.463,
///   "duration_ms": 2340.0,
///   "status": "success",
///   "metadata": { ... }
/// }
/// ```
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
    ///
    /// # Arguments
    ///
    /// * `session_id` - Unique session identifier (becomes trace_id)
    /// * `metadata` - Session metadata (username, started_at, project_name)
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
    ///
    /// # Arguments
    ///
    /// * `round_id` - Unique round identifier
    /// * `session_id` - Parent session ID (for grouping)
    /// * `metadata` - Round data (phrase, choices, correct_word, etc.)
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
    ///
    /// # Arguments
    ///
    /// * `round_id` - Parent round ID
    /// * `session_id` - Trace/session ID for correlation
    /// * `metadata` - LLM data (model, prompt_tokens, completion_tokens, etc.)
    pub fn new_llm_phrase(
        round_id: &str,
        session_id: Option<String>,
        metadata: serde_json::Value,
    ) -> Self {
        Self {
            span_id: Uuid::new_v4().to_string(),
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
    ///
    /// # Arguments
    ///
    /// * `session_id` - Parent session ID
    /// * `metadata` - Submission data (username, score, accuracy, rank, etc.)
    pub fn new_leaderboard_submit(session_id: &str, metadata: serde_json::Value) -> Self {
        Self {
            span_id: Uuid::new_v4().to_string(),
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

    /// Update metadata field.
    pub fn set_metadata(&mut self, key: &str, value: serde_json::Value) {
        if let Some(obj) = self.metadata.as_object_mut() {
            obj.insert(key.to_string(), value);
        }
    }

    /// Get metadata field.
    pub fn get_metadata(&self, key: &str) -> Option<&serde_json::Value> {
        self.metadata.get(key)
    }
}

// =============================================================================
// WASM Callback Bridge (AC-5, AC-6)
// =============================================================================

// Thread-local storage for the game Opik callback
thread_local! {
    static GAME_OPIK_HANDLER: RefCell<Option<OpikGameHandler>> = const { RefCell::new(None) };
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

/// Callback handler for sending spans.
///
/// This is a function pointer that can be set from native Rust code.
/// For WASM, use `set_game_opik_handler_js()` instead.
pub type OpikGameHandler = Box<dyn Fn(&OpikGameSpan) -> Result<(), String> + Send + Sync>;

/// Set the game Opik handler (native Rust).
///
/// # Arguments
///
/// * `handler` - Callback function that receives spans
///
/// # Example
///
/// ```ignore
/// set_game_opik_handler(Box::new(|span| {
///     println!("Received span: {:?}", span);
///     Ok(())
/// }));
/// ```
pub fn set_game_opik_handler(handler: OpikGameHandler) {
    GAME_OPIK_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    log::debug!("[GAME-OPIK] Handler registered");
}

/// Clear the game Opik handler.
pub fn clear_game_opik_handler() {
    GAME_OPIK_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    log::debug!("[GAME-OPIK] Handler cleared");
}

/// Check if a game Opik handler is registered (AC-6).
///
/// Returns `true` if tracing is available, `false` otherwise.
/// Use this for graceful degradation.
pub fn has_game_opik_handler() -> bool {
    GAME_OPIK_HANDLER.with(|h| h.borrow().is_some())
}

/// Configure game Opik settings.
pub fn configure_game_opik(config: GameOpikConfig) {
    GAME_OPIK_CONFIG.with(|c| {
        *c.borrow_mut() = config;
    });
    log::debug!("[GAME-OPIK] Configuration updated");
}

/// Get current game Opik configuration.
pub fn get_game_opik_config() -> GameOpikConfig {
    GAME_OPIK_CONFIG.with(|c| c.borrow().clone())
}

/// Check if game Opik tracing is enabled (AC-6).
///
/// Returns `true` if both:
/// 1. A handler is registered
/// 2. Tracing is enabled in config (or config.enabled is not set)
pub fn is_game_opik_enabled() -> bool {
    let has_handler = has_game_opik_handler();
    let config_enabled = GAME_OPIK_CONFIG.with(|c| c.borrow().enabled);

    // If handler exists, check config. If no handler, always false.
    has_handler && config_enabled
}

/// Send a game span to Opik (fire-and-forget) (AC-6).
///
/// This function implements graceful degradation:
/// - If no handler is registered, silently returns Ok
/// - If handler fails, logs debug message but doesn't fail
///
/// # Arguments
///
/// * `span` - The span to send
///
/// # Returns
///
/// Always returns `Ok(())` - errors are logged but don't propagate.
pub fn send_game_span(span: &OpikGameSpan) -> Result<(), String> {
    // Graceful degradation: no handler = no-op
    if !has_game_opik_handler() {
        log::debug!(
            "[GAME-OPIK] No handler registered, skipping span: {}",
            span.name
        );
        return Ok(());
    }

    GAME_OPIK_HANDLER.with(|h| {
        if let Some(ref handler) = *h.borrow() {
            match handler(span) {
                Ok(()) => {
                    log::debug!("[GAME-OPIK] Span sent: {} ({})", span.name, span.span_type);
                }
                Err(e) => {
                    // Log error but don't fail - graceful degradation
                    log::debug!("[GAME-OPIK] Failed to send span: {}", e);
                }
            }
        }
        Ok(())
    })
}

/// Send a game span asynchronously (for WASM compatibility).
///
/// This is a convenience wrapper that serializes the span to JSON.
pub fn send_game_span_json(span_json: &str) -> Result<(), String> {
    let span: OpikGameSpan =
        serde_json::from_str(span_json).map_err(|e| format!("Invalid span JSON: {}", e))?;
    send_game_span(&span)
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get current Unix timestamp with microsecond precision.
pub fn current_timestamp() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

/// Convert Unix timestamp to ISO 8601 format.
pub fn timestamp_to_iso8601(timestamp: f64) -> String {
    use chrono::{TimeZone, Utc};
    let secs = timestamp as i64;
    let nanos = ((timestamp - secs as f64) * 1_000_000_000.0) as u32;
    Utc.timestamp_opt(secs, nanos)
        .single()
        .map(|dt| dt.format("%Y-%m-%dT%H:%M:%S%.3fZ").to_string())
        .unwrap_or_else(|| "1970-01-01T00:00:00.000Z".to_string())
}

// =============================================================================
// Tests (AC-7)
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    // =========================================================================
    // GAME-001.8-UNIT-001: OpikGameSpan serialization tests
    // =========================================================================

    #[test]
    fn test_game_span_type_serialization() {
        // Test each span type serializes to correct snake_case string
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
    fn test_game_span_type_deserialization() {
        assert_eq!(
            serde_json::from_str::<GameSpanType>("\"session\"").unwrap(),
            GameSpanType::Session
        );
        assert_eq!(
            serde_json::from_str::<GameSpanType>("\"round\"").unwrap(),
            GameSpanType::Round
        );
        assert_eq!(
            serde_json::from_str::<GameSpanType>("\"llm_phrase\"").unwrap(),
            GameSpanType::LlmPhrase
        );
        assert_eq!(
            serde_json::from_str::<GameSpanType>("\"leaderboard_submit\"").unwrap(),
            GameSpanType::LeaderboardSubmit
        );
    }

    #[test]
    fn test_opik_game_span_session_serialization() {
        let span = OpikGameSpan::new_session(
            "sess_abc123",
            serde_json::json!({
                "username": "SwiftFox42",
                "started_at": "2026-01-10T12:00:00Z",
                "project_name": "know-your-model"
            }),
        );

        let json = serde_json::to_string(&span).unwrap();

        // Verify required fields
        assert!(json.contains("\"span_id\":\"sess_abc123\""));
        assert!(json.contains("\"trace_id\":\"sess_abc123\""));
        assert!(json.contains("\"name\":\"game_session\""));
        assert!(json.contains("\"span_type\":\"session\""));
        assert!(json.contains("\"status\":\"running\""));
        assert!(json.contains("\"username\":\"SwiftFox42\""));

        // Verify parent_id is not present (session has no parent)
        assert!(!json.contains("\"parent_id\""));
    }

    #[test]
    fn test_opik_game_span_round_serialization() {
        let span = OpikGameSpan::new_round(
            "round_xyz789",
            Some("sess_abc123".to_string()),
            serde_json::json!({
                "round_number": 5,
                "phrase": "The ___ shines brightly.",
                "choices": ["sun", "moon", "star", "light", "lamp"],
                "correct_word": "sun",
                "selected_word": "moon",
                "is_correct": false,
                "response_time_ms": 2340,
                "difficulty": 0.65,
                "running_accuracy": 0.6,
                "running_score": 0.32
            }),
        );

        let json = serde_json::to_string(&span).unwrap();

        // Verify required fields per AC-1
        assert!(json.contains("\"span_type\":\"round\""));
        assert!(json.contains("\"parent_id\":\"sess_abc123\""));
        assert!(json.contains("\"trace_id\":\"sess_abc123\""));
        assert!(json.contains("\"phrase\":\"The ___ shines brightly.\""));
        assert!(json.contains("\"correct_word\":\"sun\""));
        assert!(json.contains("\"selected_word\":\"moon\""));
        assert!(json.contains("\"is_correct\":false"));
        assert!(json.contains("\"response_time_ms\":2340"));
        assert!(json.contains("\"difficulty\":0.65"));
    }

    #[test]
    fn test_opik_game_span_llm_phrase_serialization() {
        let span = OpikGameSpan::new_llm_phrase(
            "round_xyz789",
            Some("sess_abc123".to_string()),
            serde_json::json!({
                "model": "gemma-3-1b-it",
                "prompt_tokens": 150,
                "completion_tokens": 25,
                "total_tokens": 175,
                "latency_ms": 1200,
                "phrase": "The ___ shines brightly.",
                "word": "sun"
            }),
        );

        let json = serde_json::to_string(&span).unwrap();

        // Verify AC-3 fields
        assert!(json.contains("\"span_type\":\"llm_phrase\""));
        assert!(json.contains("\"parent_id\":\"round_xyz789\""));
        assert!(json.contains("\"name\":\"phrase_generation\""));
        assert!(json.contains("\"prompt_tokens\":150"));
        assert!(json.contains("\"completion_tokens\":25"));
        assert!(json.contains("\"total_tokens\":175"));
        assert!(json.contains("\"latency_ms\":1200"));
    }

    #[test]
    fn test_opik_game_span_leaderboard_serialization() {
        let span = OpikGameSpan::new_leaderboard_submit(
            "sess_abc123",
            serde_json::json!({
                "username": "SwiftFox42",
                "final_score": 0.42,
                "accuracy": 0.7,
                "total_answers": 10,
                "avg_difficulty": 0.6,
                "rank": 3,
                "is_new_best": true
            }),
        );

        let json = serde_json::to_string(&span).unwrap();

        // Verify AC-4 fields
        assert!(json.contains("\"span_type\":\"leaderboard_submit\""));
        assert!(json.contains("\"parent_id\":\"sess_abc123\""));
        assert!(json.contains("\"trace_id\":\"sess_abc123\""));
        assert!(json.contains("\"final_score\":0.42"));
        assert!(json.contains("\"accuracy\":0.7"));
        assert!(json.contains("\"rank\":3"));
        assert!(json.contains("\"is_new_best\":true"));
    }

    #[test]
    fn test_opik_game_span_roundtrip() {
        let original = OpikGameSpan::new_round(
            "round_001",
            Some("sess_001".to_string()),
            serde_json::json!({
                "phrase": "Test phrase",
                "is_correct": true
            }),
        );

        let json = serde_json::to_string(&original).unwrap();
        let restored: OpikGameSpan = serde_json::from_str(&json).unwrap();

        assert_eq!(original.span_id, restored.span_id);
        assert_eq!(original.parent_id, restored.parent_id);
        assert_eq!(original.trace_id, restored.trace_id);
        assert_eq!(original.name, restored.name);
        assert_eq!(original.span_type, restored.span_type);
        assert_eq!(original.status, restored.status);
    }

    // =========================================================================
    // GAME-001.8-UNIT-002: Span completion tests
    // =========================================================================

    #[test]
    fn test_span_complete_sets_success_status() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        assert_eq!(span.status, "running");
        assert!(span.end_time.is_none());
        assert!(span.duration_ms.is_none());

        span.complete();

        assert_eq!(span.status, "success");
        assert!(span.end_time.is_some());
        assert!(span.duration_ms.is_some());
    }

    #[test]
    fn test_span_fail_sets_failure_status() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        span.fail();

        assert_eq!(span.status, "failure");
        assert!(span.end_time.is_some());
    }

    #[test]
    fn test_span_complete_with_custom_status() {
        let mut span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        span.complete_with("timeout", Some(5000.0));

        assert_eq!(span.status, "timeout");
        assert_eq!(span.duration_ms, Some(5000.0));
    }

    // =========================================================================
    // GAME-001.8-UNIT-003: Metadata manipulation tests
    // =========================================================================

    #[test]
    fn test_set_metadata() {
        let mut span =
            OpikGameSpan::new_session("sess_001", serde_json::json!({"initial": "value"}));

        span.set_metadata("new_key", serde_json::json!("new_value"));

        assert_eq!(
            span.get_metadata("new_key"),
            Some(&serde_json::json!("new_value"))
        );
        assert_eq!(
            span.get_metadata("initial"),
            Some(&serde_json::json!("value"))
        );
    }

    #[test]
    fn test_get_metadata_nonexistent() {
        let span = OpikGameSpan::new_session("sess_001", serde_json::json!({}));

        assert!(span.get_metadata("nonexistent").is_none());
    }

    // =========================================================================
    // GAME-001.8-UNIT-007: Graceful degradation tests (AC-6)
    // =========================================================================

    #[test]
    fn test_has_game_opik_handler_false_by_default() {
        // Clear any existing handler
        clear_game_opik_handler();

        assert!(!has_game_opik_handler());
    }

    #[test]
    fn test_send_game_span_noop_without_handler() {
        clear_game_opik_handler();

        let span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));

        // Should return Ok without handler (graceful degradation)
        let result = send_game_span(&span);
        assert!(result.is_ok());
    }

    #[test]
    fn test_is_game_opik_enabled_requires_both() {
        clear_game_opik_handler();
        configure_game_opik(GameOpikConfig {
            enabled: true,
            project_name: None,
        });

        // Enabled in config but no handler = disabled
        assert!(!is_game_opik_enabled());
    }

    // =========================================================================
    // GAME-001.8-UNIT-004: Handler registration tests
    // =========================================================================

    #[test]
    fn test_handler_registration_and_clearing() {
        clear_game_opik_handler();
        assert!(!has_game_opik_handler());

        set_game_opik_handler(Box::new(|_| Ok(())));
        assert!(has_game_opik_handler());

        clear_game_opik_handler();
        assert!(!has_game_opik_handler());
    }

    #[test]
    fn test_handler_receives_spans() {
        let call_count = Arc::new(AtomicUsize::new(0));
        let count_clone = call_count.clone();

        set_game_opik_handler(Box::new(move |_span| {
            count_clone.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }));

        configure_game_opik(GameOpikConfig {
            enabled: true,
            project_name: Some("test".to_string()),
        });

        let span = OpikGameSpan::new_round("round_001", None, serde_json::json!({}));
        let _ = send_game_span(&span);

        assert_eq!(call_count.load(Ordering::SeqCst), 1);

        // Cleanup
        clear_game_opik_handler();
    }

    // =========================================================================
    // GAME-001.8-UNIT-005: Configuration tests
    // =========================================================================

    #[test]
    fn test_game_opik_config_default() {
        let config = GameOpikConfig::default();
        assert!(config.project_name.is_none());
        assert!(!config.enabled);
    }

    #[test]
    fn test_configure_and_get_config() {
        configure_game_opik(GameOpikConfig {
            enabled: true,
            project_name: Some("my-game".to_string()),
        });

        let config = get_game_opik_config();
        assert!(config.enabled);
        assert_eq!(config.project_name, Some("my-game".to_string()));

        // Reset
        configure_game_opik(GameOpikConfig::default());
    }

    // =========================================================================
    // GAME-001.8-UNIT-006: Timestamp helper tests
    // =========================================================================

    #[test]
    fn test_current_timestamp_is_reasonable() {
        let ts = current_timestamp();
        // Should be after 2020 and before 2100
        assert!(ts > 1577836800.0); // 2020-01-01
        assert!(ts < 4102444800.0); // 2100-01-01
    }

    #[test]
    fn test_timestamp_to_iso8601() {
        let iso = timestamp_to_iso8601(1705323045.123);
        assert!(iso.ends_with("Z"));
        assert!(iso.contains("2024-01-15"));
    }

    #[test]
    fn test_timestamp_to_iso8601_epoch() {
        let iso = timestamp_to_iso8601(0.0);
        assert_eq!(iso, "1970-01-01T00:00:00.000Z");
    }

    // =========================================================================
    // GAME-001.8-UNIT-008: Span JSON parsing tests
    // =========================================================================

    #[test]
    fn test_send_game_span_json_valid() {
        clear_game_opik_handler();

        let json = r#"{
            "span_id": "test_001",
            "trace_id": "trace_001",
            "name": "test_span",
            "span_type": "round",
            "start_time": 1705323045.0,
            "status": "success",
            "metadata": {}
        }"#;

        let result = send_game_span_json(json);
        assert!(result.is_ok());
    }

    #[test]
    fn test_send_game_span_json_invalid() {
        let result = send_game_span_json("not valid json");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Invalid span JSON"));
    }

    // =========================================================================
    // GAME-001.8-UNIT-009: Display trait tests
    // =========================================================================

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
}
