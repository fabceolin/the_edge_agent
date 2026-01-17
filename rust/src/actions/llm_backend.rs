//! LLM Backend abstraction trait
//!
//! Provides a unified interface for both local (llama.cpp) and API-based LLM backends.
//! This module is the core abstraction layer for LLM operations in TEA.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use thiserror::Error;

/// Errors that can occur during LLM operations
#[derive(Debug, Error)]
pub enum LlmError {
    #[error("Model not found: {0}")]
    ModelNotFound(String),
    #[error("Model load failed: {0}")]
    ModelLoadFailed(String),
    #[error("Inference failed: {0}")]
    InferenceFailed(String),
    #[error("Embedding not supported by this model")]
    EmbeddingNotSupported,
    #[error("API error: {0}")]
    ApiError(String),
    #[error("Configuration error: {0}")]
    ConfigError(String),
    #[error("Feature not available: {0}")]
    FeatureNotAvailable(String),
}

/// Result type for LLM operations
pub type LlmResult<T> = Result<T, LlmError>;

/// Parameters for LLM text completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmCallParams {
    /// The prompt text to complete
    pub prompt: String,
    /// Maximum number of tokens to generate
    #[serde(default = "default_max_tokens")]
    pub max_tokens: usize,
    /// Temperature for sampling (0.0 = deterministic, 1.0 = creative)
    #[serde(default = "default_temperature")]
    pub temperature: f32,
    /// Stop sequences to end generation
    #[serde(default)]
    pub stop: Option<Vec<String>>,
}

fn default_max_tokens() -> usize {
    100
}

fn default_temperature() -> f32 {
    0.7
}

/// A chat message in OpenAI-compatible format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatMessage {
    /// Role: "system", "user", or "assistant"
    pub role: String,
    /// Message content
    pub content: String,
}

/// Result of an LLM completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmCallResult {
    /// Generated text content
    pub content: String,
    /// Model identifier used
    pub model: String,
    /// Number of tokens used (if available)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tokens_used: Option<usize>,
    /// Finish reason (e.g., "stop", "length")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
}

/// Embedding result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbeddingResult {
    /// The embedding vector
    pub embedding: Vec<f32>,
    /// Model used for embedding
    pub model: String,
    /// Number of tokens in input
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tokens_used: Option<usize>,
}

/// Supported model configuration (auto-detected from filename)
#[derive(Debug, Clone)]
pub struct ModelConfig {
    /// Context window size
    pub n_ctx: u32,
    /// Chat format template (e.g., "chatml", "gemma")
    pub chat_format: Option<&'static str>,
    /// Model family identifier
    pub family: &'static str,
}

/// Default models in order of preference for bundled distributions
pub const DEFAULT_MODELS: &[&str] = &[
    "gemma-3-1b-it-Q8_0.gguf", // Gemma 3 1B (ultra-lightweight, 8K ctx)
    "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf", // Phi-4-mini (compact, 128K ctx)
    "gemma-3n-E4B-it-Q4_K_M.gguf", // Gemma 3n (larger, higher quality)
];

/// LLM Backend trait - implemented by LocalLlmBackend and ApiLlmBackend
///
/// NOTE: Sync trait because llama-cpp-2 is CPU-bound sync inference.
/// For async contexts, wrap calls in `spawn_blocking`.
pub trait LlmBackend: Send + Sync {
    /// Get the backend name for logging/debugging
    fn name(&self) -> &str;

    /// Check if this backend is available and ready
    fn is_available(&self) -> bool;

    /// Raw prompt completion
    fn call(&self, params: LlmCallParams) -> LlmResult<LlmCallResult>;

    /// OpenAI-compatible chat completion (recommended for instruction models)
    fn chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
    ) -> LlmResult<LlmCallResult>;

    /// Generate embeddings for text
    fn embed(&self, text: &str) -> LlmResult<EmbeddingResult>;

    /// Streaming text generation with callback
    fn stream(&self, params: LlmCallParams, callback: &dyn Fn(&str)) -> LlmResult<LlmCallResult>;

    /// Streaming chat completion with callback
    fn stream_chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
        callback: &dyn Fn(&str),
    ) -> LlmResult<LlmCallResult>;
}

/// Auto-detect model configuration from filename
pub fn get_model_config(model_path: &std::path::Path) -> ModelConfig {
    let filename = model_path
        .file_name()
        .map(|s| s.to_string_lossy().to_lowercase())
        .unwrap_or_default();

    if filename.contains("phi") {
        ModelConfig {
            n_ctx: 128_000, // Phi-4-mini's 128K context
            chat_format: Some("chatml"),
            family: "phi",
        }
    } else if filename.contains("gemma-3-1b") || filename.contains("gemma_3_1b") {
        // Gemma 3 1B has smaller 8K context
        ModelConfig {
            n_ctx: 8_192, // Gemma 3 1B's 8K context
            chat_format: Some("gemma"),
            family: "gemma",
        }
    } else if filename.contains("gemma") {
        // Larger Gemma models (3n, 4B, etc.) have 32K context
        ModelConfig {
            n_ctx: 32_768, // Gemma's 32K context
            chat_format: Some("gemma"),
            family: "gemma",
        }
    } else if filename.contains("tinyllama") || filename.contains("stories") {
        ModelConfig {
            n_ctx: 2_048, // TinyLlama's context
            chat_format: None,
            family: "tinyllama",
        }
    } else {
        ModelConfig {
            n_ctx: 4_096, // Safe default
            chat_format: None,
            family: "unknown",
        }
    }
}

/// Format chat messages according to model's chat template
pub fn format_chat_prompt(messages: &[ChatMessage], format: Option<&str>) -> String {
    match format {
        Some("chatml") => {
            // ChatML format for Phi-4-mini
            let mut prompt = String::new();
            for m in messages {
                prompt.push_str(&format!(
                    "<|im_start|>{}\n{}<|im_end|>\n",
                    m.role, m.content
                ));
            }
            prompt.push_str("<|im_start|>assistant\n");
            prompt
        }
        Some("gemma") => {
            // Gemma format
            let mut prompt = String::new();
            for m in messages {
                match m.role.as_str() {
                    "user" => {
                        prompt.push_str(&format!(
                            "<start_of_turn>user\n{}<end_of_turn>\n",
                            m.content
                        ));
                    }
                    "model" | "assistant" => {
                        prompt.push_str(&format!(
                            "<start_of_turn>model\n{}<end_of_turn>\n",
                            m.content
                        ));
                    }
                    _ => {
                        prompt.push_str(&format!("{}\n", m.content));
                    }
                }
            }
            prompt.push_str("<start_of_turn>model\n");
            prompt
        }
        _ => {
            // Simple concatenation for unknown formats
            let mut prompt = String::new();
            for m in messages {
                prompt.push_str(&format!("{}: {}\n", m.role, m.content));
            }
            prompt.push_str("assistant: ");
            prompt
        }
    }
}

/// Resolve model path with priority order:
/// 1. CLI --gguf parameter (highest priority) (TEA-CLI-007)
/// 2. TEA_MODEL_PATH environment variable
/// 3. Explicit model_path from settings
/// 4. APPDIR (AppImage bundled model)
/// 5. ~/.cache/tea/models/
pub fn resolve_model_path(explicit_path: Option<&str>) -> Option<PathBuf> {
    resolve_model_path_with_cli(None, explicit_path)
}

/// Resolve model path with CLI override support (TEA-CLI-007)
///
/// Priority order:
/// 1. CLI --gguf parameter (highest priority)
/// 2. TEA_MODEL_PATH environment variable
/// 3. Explicit model_path from settings
/// 4. APPDIR (AppImage bundled model)
/// 5. ~/.cache/tea/models/
pub fn resolve_model_path_with_cli(
    cli_path: Option<&str>,
    explicit_path: Option<&str>,
) -> Option<PathBuf> {
    // 1. CLI override (highest priority) - TEA-CLI-007
    if let Some(path_str) = cli_path {
        let path = PathBuf::from(path_str);
        if path.exists() {
            tracing::info!("Using model from --gguf CLI: {}", path.display());
            return Some(path);
        }
        tracing::warn!("CLI --gguf path not found: {}", path.display());
    }

    // 2. Explicit environment variable
    if let Ok(path) = std::env::var("TEA_MODEL_PATH") {
        let path = PathBuf::from(path);
        if path.exists() {
            tracing::info!("Using model from TEA_MODEL_PATH: {}", path.display());
            return Some(path);
        }
        tracing::warn!("TEA_MODEL_PATH set but file not found: {}", path.display());
    }

    // 2. Explicit path from settings
    if let Some(path_str) = explicit_path {
        let path = PathBuf::from(path_str);
        if path.exists() {
            tracing::info!("Using configured model_path: {}", path.display());
            return Some(path);
        }
        tracing::warn!("Configured model_path not found: {}", path.display());
    }

    // 3. AppImage bundle location (set by AppRun script)
    if let Ok(appdir) = std::env::var("APPDIR") {
        let models_dir = PathBuf::from(appdir).join("usr/share/models");
        for model_name in DEFAULT_MODELS {
            let candidate = models_dir.join(model_name);
            if candidate.exists() {
                tracing::info!("Found bundled model: {}", candidate.display());
                return Some(candidate);
            }
        }
    }

    // 4. Default cache location
    #[cfg(feature = "llm-local")]
    if let Some(home) = dirs::home_dir() {
        let cache_dir = home.join(".cache/tea/models");
        for model_name in DEFAULT_MODELS {
            let candidate = cache_dir.join(model_name);
            if candidate.exists() {
                tracing::info!("Found cached model: {}", candidate.display());
                return Some(candidate);
            }
        }
    }

    tracing::warn!("No local model found. Searched: TEA_MODEL_PATH, settings.model_path, APPDIR, ~/.cache/tea/models/");
    None
}

/// Backend type for configuration
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BackendType {
    /// Use local llama.cpp backend
    Local,
    /// Use API-based backend
    Api,
    /// Automatically select (try local first, fallback to API)
    #[default]
    Auto,
}

impl std::str::FromStr for BackendType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "local" => Ok(BackendType::Local),
            "api" => Ok(BackendType::Api),
            "auto" => Ok(BackendType::Auto),
            _ => Err(format!("Unknown backend type: {}", s)),
        }
    }
}

/// LLM settings from YAML configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LlmSettings {
    /// Backend type: "local", "api", or "auto"
    #[serde(default)]
    pub backend: BackendType,
    /// Path to GGUF model file (for local backend)
    #[serde(default)]
    pub model_path: Option<String>,
    /// Context window size override
    #[serde(default)]
    pub n_ctx: Option<u32>,
    /// Number of CPU threads (default: all cores)
    #[serde(default)]
    pub n_threads: Option<u32>,
    /// Number of GPU layers to offload (0 = CPU only)
    #[serde(default)]
    pub n_gpu_layers: Option<u32>,
    /// API base URL (for API backend)
    #[serde(default)]
    pub api_url: Option<String>,
    /// API key (for API backend)
    #[serde(default)]
    pub api_key: Option<String>,
    /// Default model name
    #[serde(default)]
    pub model: Option<String>,
}

/// Check if local LLM feature is compiled
pub fn is_local_llm_compiled() -> bool {
    cfg!(feature = "llm-local")
}

/// Check if a local model is available (feature compiled + model file exists)
pub fn is_local_llm_available(settings: &LlmSettings) -> bool {
    is_local_llm_available_with_cli(None, settings)
}

/// Check if a local model is available with CLI override support (TEA-CLI-007)
pub fn is_local_llm_available_with_cli(cli_path: Option<&str>, settings: &LlmSettings) -> bool {
    if !is_local_llm_compiled() {
        return false;
    }
    resolve_model_path_with_cli(cli_path, settings.model_path.as_deref()).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_values() {
        let params = LlmCallParams {
            prompt: "test".to_string(),
            max_tokens: default_max_tokens(),
            temperature: default_temperature(),
            stop: None,
        };
        assert_eq!(params.max_tokens, 100);
        assert!((params.temperature - 0.7).abs() < 0.001);
    }

    #[test]
    fn test_chat_message_serialization() {
        let msg = ChatMessage {
            role: "user".to_string(),
            content: "Hello".to_string(),
        };
        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("user"));
        assert!(json.contains("Hello"));
    }

    #[test]
    fn test_model_config_phi() {
        let path = std::path::Path::new("Phi-4-mini-instruct.gguf");
        let config = get_model_config(path);
        assert_eq!(config.family, "phi");
        assert_eq!(config.n_ctx, 128_000);
        assert_eq!(config.chat_format, Some("chatml"));
    }

    #[test]
    fn test_model_config_gemma() {
        let path = std::path::Path::new("gemma-3n-E4B-it.gguf");
        let config = get_model_config(path);
        assert_eq!(config.family, "gemma");
        assert_eq!(config.n_ctx, 32_768);
        assert_eq!(config.chat_format, Some("gemma"));
    }

    #[test]
    fn test_model_config_gemma_3_1b() {
        // Test Gemma 3 1B model detection (smaller 8K context)
        let path = std::path::Path::new("gemma-3-1b-it-Q8_0.gguf");
        let config = get_model_config(path);
        assert_eq!(config.family, "gemma");
        assert_eq!(config.n_ctx, 8_192);
        assert_eq!(config.chat_format, Some("gemma"));
    }

    #[test]
    fn test_model_config_tinyllama() {
        let path = std::path::Path::new("stories260K.gguf");
        let config = get_model_config(path);
        assert_eq!(config.family, "tinyllama");
        assert_eq!(config.n_ctx, 2_048);
    }

    #[test]
    fn test_model_config_unknown() {
        let path = std::path::Path::new("unknown-model.gguf");
        let config = get_model_config(path);
        assert_eq!(config.family, "unknown");
        assert_eq!(config.n_ctx, 4_096);
    }

    #[test]
    fn test_format_chat_chatml() {
        let messages = vec![
            ChatMessage {
                role: "system".to_string(),
                content: "You are helpful.".to_string(),
            },
            ChatMessage {
                role: "user".to_string(),
                content: "Hello".to_string(),
            },
        ];
        let prompt = format_chat_prompt(&messages, Some("chatml"));
        assert!(prompt.contains("<|im_start|>system"));
        assert!(prompt.contains("<|im_end|>"));
        assert!(prompt.contains("<|im_start|>assistant\n"));
    }

    #[test]
    fn test_format_chat_gemma() {
        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let prompt = format_chat_prompt(&messages, Some("gemma"));
        assert!(prompt.contains("<start_of_turn>user"));
        assert!(prompt.contains("<end_of_turn>"));
        assert!(prompt.contains("<start_of_turn>model\n"));
    }

    #[test]
    fn test_format_chat_unknown() {
        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let prompt = format_chat_prompt(&messages, None);
        assert!(prompt.contains("user: Hello"));
        assert!(prompt.contains("assistant: "));
    }

    #[test]
    fn test_backend_type_parse() {
        assert_eq!("local".parse::<BackendType>().unwrap(), BackendType::Local);
        assert_eq!("api".parse::<BackendType>().unwrap(), BackendType::Api);
        assert_eq!("auto".parse::<BackendType>().unwrap(), BackendType::Auto);
        assert_eq!("AUTO".parse::<BackendType>().unwrap(), BackendType::Auto);
    }

    #[test]
    fn test_llm_settings_default() {
        let settings = LlmSettings::default();
        assert_eq!(settings.backend, BackendType::Auto);
        assert!(settings.model_path.is_none());
    }

    #[test]
    fn test_is_local_llm_compiled() {
        // This test just verifies the function runs
        let _ = is_local_llm_compiled();
    }

    #[test]
    fn test_llm_error_display() {
        let err = LlmError::ModelNotFound("test.gguf".to_string());
        assert!(err.to_string().contains("test.gguf"));

        let err = LlmError::InferenceFailed("timeout".to_string());
        assert!(err.to_string().contains("timeout"));
    }

    #[test]
    fn test_resolve_model_path_nonexistent() {
        // Should return None when no model exists
        let result = resolve_model_path(Some("/nonexistent/path/model.gguf"));
        assert!(result.is_none());
    }

    // TEA-CLI-007: Tests for CLI override support
    #[test]
    fn test_resolve_model_path_with_cli_has_priority() {
        use std::io::Write;

        // Create a temp file to act as our CLI-specified model
        let temp_dir = tempfile::tempdir().unwrap();
        let cli_model_path = temp_dir.path().join("cli_model.gguf");
        std::fs::File::create(&cli_model_path)
            .unwrap()
            .write_all(b"fake gguf data")
            .unwrap();

        // CLI path should be returned even if explicit_path is also valid
        let result = resolve_model_path_with_cli(
            Some(cli_model_path.to_str().unwrap()),
            Some("/nonexistent/explicit.gguf"),
        );

        assert!(result.is_some());
        assert_eq!(result.unwrap(), cli_model_path);
    }

    #[test]
    fn test_resolve_model_path_with_cli_fallback_to_explicit() {
        use std::io::Write;

        // Create a temp file for explicit path
        let temp_dir = tempfile::tempdir().unwrap();
        let explicit_path = temp_dir.path().join("explicit_model.gguf");
        std::fs::File::create(&explicit_path)
            .unwrap()
            .write_all(b"fake gguf data")
            .unwrap();

        // When CLI path is None, should fall back to explicit_path
        let result = resolve_model_path_with_cli(None, Some(explicit_path.to_str().unwrap()));

        assert!(result.is_some());
        assert_eq!(result.unwrap(), explicit_path);
    }

    #[test]
    fn test_resolve_model_path_with_cli_nonexistent_cli_path() {
        // CLI path is provided but doesn't exist - should fall through
        let result = resolve_model_path_with_cli(
            Some("/nonexistent/cli_model.gguf"),
            None, // No fallback either
        );

        // Should return None since CLI path doesn't exist and no fallback
        assert!(result.is_none());
    }

    #[cfg(feature = "llm-local")]
    #[test]
    fn test_is_local_llm_available_with_cli_path() {
        use std::io::Write;

        // Create a temp file for CLI model path
        let temp_dir = tempfile::tempdir().unwrap();
        let cli_model = temp_dir.path().join("cli_model.gguf");
        std::fs::File::create(&cli_model)
            .unwrap()
            .write_all(b"fake gguf")
            .unwrap();

        let settings = LlmSettings::default();

        // Without CLI path, settings alone don't have a model
        // (assuming no TEA_MODEL_PATH or cached models)
        // With CLI path, should report available
        let available =
            is_local_llm_available_with_cli(Some(cli_model.to_str().unwrap()), &settings);
        assert!(available, "Should be available with CLI model path");
    }
}
