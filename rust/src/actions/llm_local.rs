//! Local LLM backend using llama-cpp-2
//!
//! This module provides local LLM inference via llama.cpp bindings.
//! Feature-gated behind `llm-local` to keep the base binary lightweight.

#![cfg(feature = "llm-local")]

use super::llm_backend::{
    format_chat_prompt, get_model_config, ChatMessage, EmbeddingResult, LlmBackend, LlmCallParams,
    LlmCallResult, LlmError, LlmResult, LlmSettings, ModelConfig,
};
use llama_cpp_2::context::params::LlamaContextParams;
use llama_cpp_2::llama_backend::LlamaBackend;
use llama_cpp_2::llama_batch::LlamaBatch;
use llama_cpp_2::model::params::LlamaModelParams;
use llama_cpp_2::model::{AddBos, LlamaModel, Special};
use llama_cpp_2::sampling::LlamaSampler;
use std::num::NonZeroU32;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tracing::{debug, info};

/// Local LLM backend using llama-cpp-2
pub struct LocalLlmBackend {
    /// llama.cpp backend handle (must outlive model)
    _backend: Arc<LlamaBackend>,
    /// Loaded model
    model: LlamaModel,
    /// Path to the model file
    model_path: PathBuf,
    /// Model name for identification
    model_name: String,
    /// Model configuration (context size, chat format, etc.)
    config: ModelConfig,
    /// Number of CPU threads to use
    n_threads: u32,
    /// Number of GPU layers (0 = CPU only)
    n_gpu_layers: u32,
}

impl LocalLlmBackend {
    /// Create new LocalLlmBackend with auto-detected configuration
    ///
    /// # Arguments
    /// * `model_path` - Path to GGUF model file
    ///
    /// # Errors
    /// Returns error if model file doesn't exist or fails to load
    pub fn new(model_path: &Path) -> LlmResult<Self> {
        Self::with_config(model_path, None, None, None)
    }

    /// Create new LocalLlmBackend with custom configuration
    ///
    /// # Arguments
    /// * `model_path` - Path to GGUF model file
    /// * `n_ctx` - Optional context size override
    /// * `n_threads` - Optional thread count override
    /// * `n_gpu_layers` - Optional GPU layer count
    pub fn with_config(
        model_path: &Path,
        n_ctx: Option<u32>,
        n_threads: Option<u32>,
        n_gpu_layers: Option<u32>,
    ) -> LlmResult<Self> {
        if !model_path.exists() {
            return Err(LlmError::ModelNotFound(model_path.display().to_string()));
        }

        // Auto-detect model configuration
        let mut config = get_model_config(model_path);

        // Apply overrides
        if let Some(ctx) = n_ctx {
            config.n_ctx = ctx;
        }

        info!(
            "Loading model: {} (n_ctx={}, family={})",
            model_path.display(),
            config.n_ctx,
            config.family
        );

        // Initialize llama.cpp backend (required before loading model)
        let backend = LlamaBackend::init()
            .map_err(|e| LlmError::ModelLoadFailed(format!("Backend init failed: {}", e)))?;
        let backend = Arc::new(backend);

        // Configure model parameters
        let mut model_params = LlamaModelParams::default();

        // Set GPU layers if specified
        let gpu_layers = n_gpu_layers.unwrap_or(0);
        if gpu_layers > 0 {
            model_params = model_params.with_n_gpu_layers(gpu_layers);
        }

        // Load model
        let model = LlamaModel::load_from_file(&backend, model_path, &model_params)
            .map_err(|e| LlmError::ModelLoadFailed(e.to_string()))?;

        // Use all available CPU cores by default
        let thread_count = n_threads.unwrap_or_else(|| {
            std::thread::available_parallelism()
                .map(|p| p.get() as u32)
                .unwrap_or(4)
        });

        let model_name = model_path
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        info!(
            "Model loaded successfully: {} ({} threads, {} GPU layers)",
            model_name, thread_count, gpu_layers
        );

        Ok(Self {
            _backend: backend,
            model,
            model_path: model_path.to_path_buf(),
            model_name,
            config,
            n_threads: thread_count,
            n_gpu_layers: gpu_layers,
        })
    }

    /// Create from LlmSettings
    pub fn from_settings(settings: &LlmSettings) -> LlmResult<Self> {
        let model_path = super::llm_backend::resolve_model_path(settings.model_path.as_deref())
            .ok_or_else(|| LlmError::ModelNotFound("No model found in search paths".to_string()))?;

        Self::with_config(
            &model_path,
            settings.n_ctx,
            settings.n_threads,
            settings.n_gpu_layers,
        )
    }

    /// Get the model's context size
    pub fn context_size(&self) -> u32 {
        self.config.n_ctx
    }

    /// Get the model path
    pub fn model_path(&self) -> &Path {
        &self.model_path
    }

    /// Create a new context for inference
    fn create_context(&self) -> LlmResult<llama_cpp_2::context::LlamaContext<'_>> {
        let ctx_params = LlamaContextParams::default()
            .with_n_ctx(NonZeroU32::new(self.config.n_ctx))
            .with_n_threads(self.n_threads as i32)
            .with_n_threads_batch(self.n_threads as i32);

        self.model
            .new_context(&self._backend, ctx_params)
            .map_err(|e| LlmError::InferenceFailed(format!("Failed to create context: {}", e)))
    }

    /// Tokenize a prompt string
    fn tokenize(
        &self,
        prompt: &str,
        add_bos: bool,
    ) -> LlmResult<Vec<llama_cpp_2::token::LlamaToken>> {
        let bos = if add_bos {
            AddBos::Always
        } else {
            AddBos::Never
        };
        self.model
            .str_to_token(prompt, bos)
            .map_err(|e| LlmError::InferenceFailed(format!("Tokenization failed: {}", e)))
    }
}

impl LlmBackend for LocalLlmBackend {
    fn name(&self) -> &str {
        "local"
    }

    fn is_available(&self) -> bool {
        true // If we got this far, the model is loaded
    }

    fn call(&self, params: LlmCallParams) -> LlmResult<LlmCallResult> {
        debug!(
            "LocalLlmBackend::call - prompt length: {}",
            params.prompt.len()
        );

        let mut ctx = self.create_context()?;
        let tokens = self.tokenize(&params.prompt, true)?;

        // Create batch and add initial tokens
        let mut batch = LlamaBatch::new(self.config.n_ctx as usize, 1);
        let last_idx = tokens.len().saturating_sub(1);
        for (i, token) in tokens.iter().enumerate() {
            batch
                .add(*token, i as i32, &[0], i == last_idx)
                .map_err(|e| LlmError::InferenceFailed(format!("Batch add failed: {}", e)))?;
        }

        // Decode initial tokens
        ctx.decode(&mut batch)
            .map_err(|e| LlmError::InferenceFailed(format!("Initial decode failed: {}", e)))?;

        // Create sampler chain with temperature and greedy selection
        let mut sampler = LlamaSampler::chain_simple([
            LlamaSampler::temp(params.temperature),
            LlamaSampler::dist(rand::random::<u32>()),
        ]);

        // Generate output
        let mut output = String::new();
        let mut tokens_generated = 0;
        let mut n_cur = batch.n_tokens() as i32;
        let mut finish_reason = None;

        for _ in 0..params.max_tokens {
            // Sample next token using the sampler chain
            let token = sampler.sample(&ctx, batch.n_tokens() as i32 - 1);

            // Check for end of generation
            if self.model.is_eog_token(token) {
                finish_reason = Some("stop".to_string());
                break;
            }

            // Accept the token into sampler state
            sampler.accept(token);

            // Decode token to text
            let piece = self
                .model
                .token_to_str(token, Special::Tokenize)
                .map_err(|e| LlmError::InferenceFailed(format!("Token decode failed: {}", e)))?;

            // Check for stop sequences
            if let Some(ref stops) = params.stop {
                let current = format!("{}{}", output, piece);
                for stop in stops {
                    if current.contains(stop) {
                        // Truncate at stop sequence
                        if let Some(idx) = current.find(stop) {
                            output = current[..idx].to_string();
                        }
                        finish_reason = Some("stop".to_string());
                        break;
                    }
                }
                if finish_reason.is_some() {
                    break;
                }
            }

            output.push_str(&piece);
            tokens_generated += 1;

            // Prepare next batch
            batch.clear();
            batch
                .add(token, n_cur, &[0], true)
                .map_err(|e| LlmError::InferenceFailed(format!("Batch add failed: {}", e)))?;
            n_cur += 1;

            ctx.decode(&mut batch)
                .map_err(|e| LlmError::InferenceFailed(format!("Decode failed: {}", e)))?;
        }

        if finish_reason.is_none() && tokens_generated >= params.max_tokens {
            finish_reason = Some("length".to_string());
        }

        Ok(LlmCallResult {
            content: output,
            model: self.model_name.clone(),
            tokens_used: Some(tokens_generated),
            finish_reason,
        })
    }

    fn chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
    ) -> LlmResult<LlmCallResult> {
        debug!(
            "LocalLlmBackend::chat - {} messages, format: {:?}",
            messages.len(),
            self.config.chat_format
        );

        // Format messages according to model's chat template
        let prompt = format_chat_prompt(&messages, self.config.chat_format);

        self.call(LlmCallParams {
            prompt,
            max_tokens,
            temperature,
            stop: None,
        })
    }

    fn embed(&self, text: &str) -> LlmResult<EmbeddingResult> {
        debug!("LocalLlmBackend::embed - text length: {}", text.len());

        // Create context with embedding mode enabled
        let ctx_params = LlamaContextParams::default()
            .with_n_ctx(NonZeroU32::new(self.config.n_ctx))
            .with_embeddings(true);

        let mut ctx = self
            .model
            .new_context(&self._backend, ctx_params)
            .map_err(|e| {
                LlmError::InferenceFailed(format!("Failed to create embedding context: {}", e))
            })?;

        let tokens = self.tokenize(text, true)?;

        // Create batch
        let mut batch = LlamaBatch::new(self.config.n_ctx as usize, 1);
        let last_idx = tokens.len().saturating_sub(1);
        for (i, token) in tokens.iter().enumerate() {
            batch
                .add(*token, i as i32, &[0], i == last_idx)
                .map_err(|e| LlmError::InferenceFailed(format!("Batch add failed: {}", e)))?;
        }

        ctx.decode(&mut batch)
            .map_err(|e| LlmError::InferenceFailed(format!("Embedding decode failed: {}", e)))?;

        // Get embeddings
        let embeddings = ctx
            .embeddings_seq_ith(0)
            .map_err(|_| LlmError::EmbeddingNotSupported)?;

        Ok(EmbeddingResult {
            embedding: embeddings.to_vec(),
            model: self.model_name.clone(),
            tokens_used: Some(tokens.len()),
        })
    }

    fn stream(&self, params: LlmCallParams, callback: &dyn Fn(&str)) -> LlmResult<LlmCallResult> {
        debug!(
            "LocalLlmBackend::stream - prompt length: {}",
            params.prompt.len()
        );

        let mut ctx = self.create_context()?;
        let tokens = self.tokenize(&params.prompt, true)?;

        // Create batch and add initial tokens
        let mut batch = LlamaBatch::new(self.config.n_ctx as usize, 1);
        let last_idx = tokens.len().saturating_sub(1);
        for (i, token) in tokens.iter().enumerate() {
            batch
                .add(*token, i as i32, &[0], i == last_idx)
                .map_err(|e| LlmError::InferenceFailed(format!("Batch add failed: {}", e)))?;
        }

        ctx.decode(&mut batch)
            .map_err(|e| LlmError::InferenceFailed(format!("Initial decode failed: {}", e)))?;

        // Create sampler chain with temperature
        let mut sampler = LlamaSampler::chain_simple([
            LlamaSampler::temp(params.temperature),
            LlamaSampler::dist(rand::random::<u32>()),
        ]);

        // Generate with streaming
        let mut output = String::new();
        let mut tokens_generated = 0;
        let mut n_cur = batch.n_tokens() as i32;
        let mut finish_reason = None;

        for _ in 0..params.max_tokens {
            // Sample next token using the sampler chain
            let token = sampler.sample(&ctx, batch.n_tokens() as i32 - 1);

            if self.model.is_eog_token(token) {
                finish_reason = Some("stop".to_string());
                break;
            }

            // Accept the token into sampler state
            sampler.accept(token);

            let piece = self
                .model
                .token_to_str(token, Special::Tokenize)
                .map_err(|e| LlmError::InferenceFailed(format!("Token decode failed: {}", e)))?;

            // Invoke streaming callback
            callback(&piece);

            // Check stop sequences
            if let Some(ref stops) = params.stop {
                let current = format!("{}{}", output, piece);
                for stop in stops {
                    if current.contains(stop) {
                        if let Some(idx) = current.find(stop) {
                            output = current[..idx].to_string();
                        }
                        finish_reason = Some("stop".to_string());
                        break;
                    }
                }
                if finish_reason.is_some() {
                    break;
                }
            }

            output.push_str(&piece);
            tokens_generated += 1;

            batch.clear();
            batch
                .add(token, n_cur, &[0], true)
                .map_err(|e| LlmError::InferenceFailed(format!("Batch add failed: {}", e)))?;
            n_cur += 1;

            ctx.decode(&mut batch)
                .map_err(|e| LlmError::InferenceFailed(format!("Decode failed: {}", e)))?;
        }

        if finish_reason.is_none() && tokens_generated >= params.max_tokens {
            finish_reason = Some("length".to_string());
        }

        Ok(LlmCallResult {
            content: output,
            model: self.model_name.clone(),
            tokens_used: Some(tokens_generated),
            finish_reason,
        })
    }

    fn stream_chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
        callback: &dyn Fn(&str),
    ) -> LlmResult<LlmCallResult> {
        let prompt = format_chat_prompt(&messages, self.config.chat_format);
        self.stream(
            LlmCallParams {
                prompt,
                max_tokens,
                temperature,
                stop: None,
            },
            callback,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: Most tests require an actual model file, so they are marked #[ignore]
    // Run with: cargo test --features llm-local -- --ignored

    #[test]
    fn test_model_not_found() {
        let result = LocalLlmBackend::new(Path::new("/nonexistent/model.gguf"));
        assert!(result.is_err());
        match result {
            Err(LlmError::ModelNotFound(path)) => {
                assert!(path.contains("nonexistent"));
            }
            _ => panic!("Expected ModelNotFound error"),
        }
    }

    #[test]
    fn test_settings_creation() {
        // Test that from_settings works with proper error handling
        let settings = LlmSettings {
            model_path: Some("/nonexistent/model.gguf".to_string()),
            ..Default::default()
        };
        let result = LocalLlmBackend::from_settings(&settings);
        assert!(result.is_err());
    }

    /// Integration test with TinyLlama (requires model file)
    /// Download: wget https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories260K.gguf
    /// Run: TEA_MODEL_PATH=stories260K.gguf cargo test --features llm-local test_tinyllama_inference -- --ignored
    #[test]
    #[ignore]
    fn test_tinyllama_inference() {
        // Try to load from TEA_MODEL_PATH
        let model_path = std::env::var("TEA_MODEL_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("stories260K.gguf"));

        if !model_path.exists() {
            eprintln!(
                "Test model not found: {}. Download with: wget https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories260K.gguf",
                model_path.display()
            );
            return;
        }

        let backend = LocalLlmBackend::new(&model_path).expect("Failed to load model");
        assert_eq!(backend.name(), "local");
        assert!(backend.is_available());

        // Test basic completion
        let result = backend
            .call(LlmCallParams {
                prompt: "Once upon a time".to_string(),
                max_tokens: 20,
                temperature: 0.8,
                stop: None,
            })
            .expect("Inference failed");

        println!("Generated: {}", result.content);
        assert!(!result.content.is_empty());
        assert!(result.tokens_used.is_some());
    }

    #[test]
    #[ignore]
    fn test_streaming() {
        use std::cell::RefCell;

        let model_path = std::env::var("TEA_MODEL_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("stories260K.gguf"));

        if !model_path.exists() {
            return;
        }

        let backend = LocalLlmBackend::new(&model_path).expect("Failed to load model");

        let chunks = RefCell::new(Vec::new());
        let result = backend
            .stream(
                LlmCallParams {
                    prompt: "Once upon a time".to_string(),
                    max_tokens: 10,
                    temperature: 0.8,
                    stop: None,
                },
                &|chunk| {
                    chunks.borrow_mut().push(chunk.to_string());
                },
            )
            .expect("Streaming failed");

        let chunks = chunks.into_inner();
        println!("Chunks: {:?}", chunks);
        println!("Full output: {}", result.content);
        assert!(!chunks.is_empty());
    }

    #[test]
    #[ignore]
    fn test_chat_completion() {
        let model_path = std::env::var("TEA_MODEL_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("stories260K.gguf"));

        if !model_path.exists() {
            return;
        }

        let backend = LocalLlmBackend::new(&model_path).expect("Failed to load model");

        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: "Tell me a short story".to_string(),
        }];

        let result = backend.chat(messages, 50, 0.8).expect("Chat failed");
        println!("Chat response: {}", result.content);
        assert!(!result.content.is_empty());
    }
}
