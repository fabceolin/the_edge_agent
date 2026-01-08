# Story TEA-RELEASE-004.4: Rust Local LLM Actions Integration

## Status

Draft

## Story

**As a** developer using Rust TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

## LLM Backend Comparison

Understanding how Rust's local LLM compares to Python and WASM approaches:

```
┌─────────────────────────────────────────────────────────────────────┐
│                         llama.cpp (C++)                             │
│              Core inference engine for GGUF models                  │
└─────────────────────────────────────────────────────────────────────┘
        │                        │                        │
        ▼                        ▼                        ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  llama-cpp-2    │    │ llama-cpp-python│    │     wllama      │
│  (Rust crate)   │    │  (Python pkg)   │    │   (JS/WASM)     │
│                 │    │                 │    │                 │
│ Direct bindings │    │ Direct bindings │    │ Callback bridge │
│ via bindgen     │    │ via ctypes/cffi │    │ Rust→JS→WASM    │
│                 │    │                 │    │                 │
│ THIS STORY      │    │ TEA-RELEASE-    │    │ TEA-RELEASE-    │
│                 │    │ 004.5           │    │ 004.3           │
└─────────────────┘    └─────────────────┘    └─────────────────┘
        │                        │                        │
        ▼                        ▼                        ▼
   TEA Rust CLI            TEA Python CLI           TEA Browser
```

**Key advantage of Rust:** Direct bindgen bindings offer the best performance and type safety. No runtime overhead from FFI marshalling like Python's ctypes.

## Story Context

**Existing System Integration:**

- Integrates with: Rust actions system (`rust/src/actions/`)
- Technology: Rust + llama-cpp-2 crate
- Follows pattern: Existing action modules (http, file, data)
- Touch points: `rust/Cargo.toml`, `rust/src/actions/`, `rust/src/engine/yaml.rs`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: `llm.call` action generates text using local llama-cpp-2 backend (raw prompt)
2. **AC-2**: `llm.chat` action generates text using OpenAI-compatible chat format (recommended for Phi-4-mini)
3. **AC-3**: `llm.embed` action generates embeddings using local model
4. **AC-4**: `llm.stream` action supports streaming generation (callback-based)
5. **AC-5**: Model path configurable via YAML settings or environment variable
6. **AC-6**: Graceful fallback to API-based LLM if local model not found
7. **AC-7**: Auto-detect model configuration (context size, chat format) from filename

### Configuration Requirements

8. **AC-8**: Feature flag `--features llm-local` enables local LLM support
9. **AC-9**: YAML settings support `llm.backend: local` configuration
10. **AC-10**: `llm.model_path` setting specifies GGUF file location
11. **AC-11**: Default model search paths: `$APPDIR`, `~/.cache/tea/models/`
12. **AC-12**: Support both Phi-4-mini (128K ctx) and Gemma (32K ctx) models

### Quality Requirements

13. **AC-13**: Unit tests for LlmBackend trait implementation
14. **AC-14**: Integration test with small test model (TinyLlama ~500MB)
15. **AC-15**: Build without `llm-local` feature excludes llama-cpp-2 dependency
16. **AC-16**: No regressions in existing Rust tests

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-2 dependency with feature flag (AC: 8, 15)
  - [ ] Add `llama-cpp-2` to Cargo.toml as optional dependency
  - [ ] Create `llm-local` feature flag
  - [ ] Add `llm-local-cuda` and `llm-local-metal` feature variants
  - [ ] Add conditional compilation attributes
  - [ ] Verify build with and without feature

- [ ] Task 2: Define LlmBackend trait (AC: 1, 2, 3, 4, 6)
  - [ ] Create `rust/src/actions/llm_backend.rs`
  - [ ] Define `LlmBackend` trait with `call`, `chat`, `embed`, `stream` methods
  - [ ] Define `LlmCallParams`, `ChatMessage`, `LlmCallResult` structs
  - [ ] Implement `ApiLlmBackend` for existing HTTP-based LLM

- [ ] Task 3: Implement LocalLlmBackend (AC: 1, 2, 3, 4, 7, 12)
  - [ ] Create `rust/src/actions/llm_local.rs`
  - [ ] Initialize llama-cpp-2 with model path and auto-config
  - [ ] Implement `call()` for raw prompt completion
  - [ ] Implement `chat()` for OpenAI-compatible chat completion
  - [ ] Implement `embed()` for embeddings
  - [ ] Implement `stream()` and `stream_chat()` with callbacks
  - [ ] Add model auto-detection (Phi-4-mini 128K, Gemma 32K)

- [ ] Task 4: Add model path resolution (AC: 5, 10, 11)
  - [ ] Implement `resolve_model_path()` with priority order
  - [ ] Implement `get_model_info()` for auto-configuration
  - [ ] Support TEA_MODEL_PATH environment variable
  - [ ] Support APPDIR for AppImage detection
  - [ ] Support default cache path `~/.cache/tea/models/`

- [ ] Task 5: Add YAML configuration support (AC: 9, 10)
  - [ ] Add `llm` section to settings schema
  - [ ] Support `backend: local | api` selection
  - [ ] Support `model_path`, `n_ctx`, `n_threads`, `n_gpu_layers` options
  - [ ] Document settings in YAML_REFERENCE.md

- [ ] Task 6: Integrate with yaml engine action dispatcher (AC: 1, 2, 3, 4, 6)
  - [ ] Update `rust/src/engine/yaml.rs` with lazy LLM backend initialization
  - [ ] Register `llm.call`, `llm.chat`, `llm.stream`, `llm.embed` actions
  - [ ] Implement backend factory with fallback logic
  - [ ] Log backend selection with tracing

- [ ] Task 7: Add tests (AC: 13, 14, 16)
  - [ ] Add unit tests for LlmBackend trait
  - [ ] Add unit tests for LocalLlmBackend (mocked)
  - [ ] Add unit tests for model path resolution
  - [ ] Add integration test with TinyLlama test model
  - [ ] Run full test suite to verify no regressions

## Dev Notes

### Cargo.toml Configuration

```toml
[dependencies]
llama-cpp-2 = { version = "0.1", optional = true }

[features]
default = []
llm-local = ["llama-cpp-2"]
llm-local-cuda = ["llm-local", "llama-cpp-2/cuda"]
llm-local-metal = ["llm-local", "llama-cpp-2/metal"]
```

### LlmBackend Trait Design

```rust
// rust/src/actions/llm_backend.rs
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use thiserror::Error;

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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmCallParams {
    pub prompt: String,
    #[serde(default = "default_max_tokens")]
    pub max_tokens: usize,
    #[serde(default = "default_temperature")]
    pub temperature: f32,
    pub stop: Option<Vec<String>>,
}

fn default_max_tokens() -> usize { 100 }
fn default_temperature() -> f32 { 0.7 }

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatMessage {
    pub role: String,      // "system", "user", "assistant"
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmCallResult {
    pub content: String,
    pub model: String,
    pub tokens_used: Option<usize>,
}

/// LLM Backend trait - implemented by LocalLlmBackend and ApiLlmBackend
#[async_trait]
pub trait LlmBackend: Send + Sync {
    /// Raw prompt completion
    async fn call(&self, params: LlmCallParams) -> Result<LlmCallResult, LlmError>;

    /// OpenAI-compatible chat completion (recommended for instruction models)
    async fn chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
    ) -> Result<LlmCallResult, LlmError>;

    /// Generate embeddings for text
    async fn embed(&self, text: &str) -> Result<Vec<f32>, LlmError>;

    /// Streaming text generation with callback
    async fn stream(
        &self,
        params: LlmCallParams,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError>;

    /// Streaming chat completion with callback
    async fn stream_chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError>;
}
```

### Supported Models Configuration

```rust
// rust/src/actions/llm_local.rs

/// Supported model configurations (aligned with TEA-RELEASE-004 epic)
#[derive(Debug, Clone)]
pub struct ModelConfig {
    pub n_ctx: u32,
    pub chat_format: Option<&'static str>,
    pub family: &'static str,
}

/// Auto-detect model configuration from filename
pub fn get_model_config(model_path: &Path) -> ModelConfig {
    let filename = model_path
        .file_name()
        .map(|s| s.to_string_lossy().to_lowercase())
        .unwrap_or_default();

    if filename.contains("phi") {
        ModelConfig {
            n_ctx: 128_000,  // Phi-4-mini's 128K context
            chat_format: Some("chatml"),
            family: "phi",
        }
    } else if filename.contains("gemma") {
        ModelConfig {
            n_ctx: 32_768,   // Gemma's 32K context
            chat_format: Some("gemma"),
            family: "gemma",
        }
    } else {
        ModelConfig {
            n_ctx: 4_096,    // Safe default
            chat_format: None,
            family: "unknown",
        }
    }
}

/// Default model filenames in order of preference
pub const DEFAULT_MODELS: &[&str] = &[
    "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",  // Phi-4-mini (smaller, 128K ctx)
    "gemma-3n-E4B-it-Q4_K_M.gguf",                 // Gemma (larger, higher quality)
];
```

### LocalLlmBackend Implementation

```rust
// rust/src/actions/llm_local.rs
#[cfg(feature = "llm-local")]
use llama_cpp_2::model::{LlamaModel, params::LlamaModelParams};
#[cfg(feature = "llm-local")]
use llama_cpp_2::context::params::LlamaContextParams;
#[cfg(feature = "llm-local")]
use llama_cpp_2::token::data_array::LlamaTokenDataArray;

use std::path::{Path, PathBuf};
use tracing::{info, warn};

#[cfg(feature = "llm-local")]
pub struct LocalLlmBackend {
    model: LlamaModel,
    model_path: PathBuf,
    model_name: String,
    config: ModelConfig,
}

#[cfg(feature = "llm-local")]
impl LocalLlmBackend {
    /// Create new LocalLlmBackend with auto-detected configuration
    pub fn new(model_path: &Path) -> Result<Self, LlmError> {
        if !model_path.exists() {
            return Err(LlmError::ModelNotFound(
                model_path.display().to_string()
            ));
        }

        // Auto-detect model configuration
        let config = get_model_config(model_path);
        info!(
            "Loading model: {} (n_ctx={}, family={})",
            model_path.display(),
            config.n_ctx,
            config.family
        );

        // Configure model parameters
        let mut params = LlamaModelParams::default();
        params.n_ctx = config.n_ctx;

        // Use all available CPU cores
        let n_threads = std::thread::available_parallelism()
            .map(|p| p.get() as i32)
            .unwrap_or(4);
        params.n_threads = n_threads;

        let model = LlamaModel::load_from_file(model_path, params)
            .map_err(|e| LlmError::ModelLoadFailed(e.to_string()))?;

        let model_name = model_path
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        Ok(Self {
            model,
            model_path: model_path.to_path_buf(),
            model_name,
            config,
        })
    }

    /// Create with custom configuration
    pub fn with_config(
        model_path: &Path,
        n_ctx: Option<u32>,
        n_threads: Option<i32>,
        n_gpu_layers: Option<i32>,
    ) -> Result<Self, LlmError> {
        let mut backend = Self::new(model_path)?;

        // Override auto-detected config if specified
        if let Some(ctx) = n_ctx {
            backend.config.n_ctx = ctx;
        }

        Ok(backend)
    }
}

#[cfg(feature = "llm-local")]
#[async_trait]
impl LlmBackend for LocalLlmBackend {
    async fn call(&self, params: LlmCallParams) -> Result<LlmCallResult, LlmError> {
        let ctx_params = LlamaContextParams::default()
            .with_n_ctx(std::num::NonZeroU32::new(self.config.n_ctx));

        let mut ctx = self.model
            .new_context(&ctx_params)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        // Tokenize prompt
        let tokens = ctx.model()
            .str_to_token(&params.prompt, llama_cpp_2::model::AddBos::Always)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        // Decode tokens into context
        ctx.decode(&tokens)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        // Generate output
        let mut output = String::new();
        let mut tokens_generated = 0;

        for _ in 0..params.max_tokens {
            let candidates = ctx.candidates_ith(ctx.n_tokens() - 1);
            let mut candidates = LlamaTokenDataArray::from_iter(candidates, false);

            // Apply temperature sampling
            ctx.sample_temp(&mut candidates, params.temperature);
            let token = ctx.sample_token(&mut candidates);

            // Check for EOS
            if token == ctx.model().token_eos() {
                break;
            }

            // Decode token to text
            let piece = ctx.model()
                .token_to_str(token)
                .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

            output.push_str(&piece);
            tokens_generated += 1;

            // Feed token back into context
            ctx.decode(&[token])
                .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;
        }

        Ok(LlmCallResult {
            content: output,
            model: self.model_name.clone(),
            tokens_used: Some(tokens_generated),
        })
    }

    async fn chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
    ) -> Result<LlmCallResult, LlmError> {
        // Format messages according to model's chat format
        let prompt = format_chat_prompt(&messages, self.config.chat_format);

        self.call(LlmCallParams {
            prompt,
            max_tokens,
            temperature,
            stop: None,
        }).await
    }

    async fn embed(&self, text: &str) -> Result<Vec<f32>, LlmError> {
        // Note: Requires model loaded with embedding=true
        let ctx_params = LlamaContextParams::default()
            .with_embedding(true);

        let ctx = self.model
            .new_context(&ctx_params)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        let tokens = ctx.model()
            .str_to_token(text, llama_cpp_2::model::AddBos::Always)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        ctx.decode(&tokens)
            .map_err(|e| LlmError::InferenceFailed(e.to_string()))?;

        let embeddings = ctx.get_embeddings()
            .map_err(|e| LlmError::EmbeddingNotSupported)?;

        Ok(embeddings.to_vec())
    }

    async fn stream(
        &self,
        params: LlmCallParams,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError> {
        // Similar to call() but invoke callback for each token
        // ... implementation similar to call() with callback invocation
        todo!("Streaming implementation")
    }

    async fn stream_chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError> {
        let prompt = format_chat_prompt(&messages, self.config.chat_format);
        self.stream(
            LlmCallParams {
                prompt,
                max_tokens,
                temperature,
                stop: None,
            },
            callback,
        ).await
    }
}

/// Format chat messages according to model's chat template
fn format_chat_prompt(messages: &[ChatMessage], format: Option<&str>) -> String {
    match format {
        Some("chatml") => {
            // ChatML format for Phi-4-mini
            messages.iter().map(|m| {
                format!("<|im_start|>{}\n{}<|im_end|>\n", m.role, m.content)
            }).collect::<String>() + "<|im_start|>assistant\n"
        }
        Some("gemma") => {
            // Gemma format
            messages.iter().map(|m| {
                match m.role.as_str() {
                    "user" => format!("<start_of_turn>user\n{}<end_of_turn>\n", m.content),
                    "model" | "assistant" => format!("<start_of_turn>model\n{}<end_of_turn>\n", m.content),
                    _ => format!("{}\n", m.content),
                }
            }).collect::<String>() + "<start_of_turn>model\n"
        }
        _ => {
            // Simple concatenation for unknown formats
            messages.iter().map(|m| {
                format!("{}: {}\n", m.role, m.content)
            }).collect::<String>() + "assistant: "
        }
    }
}
```

### YAML Settings Schema

```yaml
# Example workflow with local LLM settings (Phi-4-mini)
settings:
  llm:
    backend: local           # 'local' or 'api'
    model_path: ~/.cache/tea/models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf
    # Auto-detected from model, but can override:
    # n_ctx: 128000          # Phi-4-mini supports 128K context
    # n_threads: 8           # CPU threads (default: all cores)
    # n_gpu_layers: 0        # GPU offload layers (0 = CPU only)

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.question }}"
      max_tokens: 100
```

### YAML Action Usage Examples

**1. Raw prompt completion (`llm.call`):**
```yaml
nodes:
  - name: complete
    action: llm.call
    params:
      prompt: "Complete this sentence: The capital of France is"
      max_tokens: 50
      temperature: 0.7
```

**2. Chat completion (`llm.chat`) - Recommended for instruction models:**
```yaml
nodes:
  - name: chat_response
    action: llm.chat
    params:
      messages:
        - role: system
          content: "You are a helpful coding assistant."
        - role: user
          content: "{{ state.question }}"
      max_tokens: 500
      temperature: 0.3
```

**3. Streaming output (`llm.stream`):**
```yaml
nodes:
  - name: stream_response
    action: llm.stream
    params:
      prompt: "Write a short poem about programming"
      max_tokens: 200
    # Output streams to stdout as tokens arrive
```

**4. Embeddings (`llm.embed`):**
```yaml
nodes:
  - name: embed_text
    action: llm.embed
    params:
      text: "{{ state.document }}"
    # Output: state.embed_text = [0.123, -0.456, ...]
```

**5. Multi-turn conversation:**
```yaml
state_schema:
  messages: list
  response: str

nodes:
  - name: add_user_message
    run: |
      state.messages.push(serde_json::json!({
        "role": "user",
        "content": state.input
      }));
      Ok(state)

  - name: get_response
    action: llm.chat
    params:
      messages: "{{ state.messages }}"
      max_tokens: 500

  - name: add_assistant_message
    run: |
      state.messages.push(serde_json::json!({
        "role": "assistant",
        "content": state.get_response
      }));
      state.response = state.get_response.clone();
      Ok(state)
```

### Model Path Resolution Order

```rust
// rust/src/actions/llm_local.rs

/// Resolve model path with priority order
fn resolve_model_path(settings: &YamlSettings) -> Option<PathBuf> {
    // 1. Explicit environment variable
    if let Ok(path) = std::env::var("TEA_MODEL_PATH") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Some(path);
        }
        tracing::warn!("TEA_MODEL_PATH set but file not found: {}", path.display());
    }

    // 2. YAML settings
    if let Some(path) = settings.llm.as_ref().and_then(|l| l.model_path.as_ref()) {
        let path = PathBuf::from(path);
        if path.exists() {
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

    tracing::warn!("No local model found. Searched: TEA_MODEL_PATH, APPDIR, ~/.cache/tea/models/");
    None
}
```

### Backend Selection Logic

```rust
// rust/src/actions/llm_backend.rs

use std::sync::OnceLock;

/// Global LLM backend (lazy initialized)
static LLM_BACKEND: OnceLock<Box<dyn LlmBackend>> = OnceLock::new();

/// Get or create LLM backend
pub fn get_llm_backend(settings: &YamlSettings) -> &'static dyn LlmBackend {
    LLM_BACKEND.get_or_init(|| create_llm_backend(settings)).as_ref()
}

/// Create LLM backend with fallback logic
fn create_llm_backend(settings: &YamlSettings) -> Box<dyn LlmBackend> {
    let backend_type = settings.llm.as_ref()
        .and_then(|l| l.backend.as_ref())
        .map(|s| s.as_str())
        .unwrap_or("auto");  // Default to auto-detection

    match backend_type {
        #[cfg(feature = "llm-local")]
        "local" | "auto" => {
            // Try to find and load local model
            if let Some(path) = resolve_model_path(settings) {
                let config = get_model_config(&path);
                tracing::info!(
                    "Initializing local LLM backend: {} (family={}, n_ctx={})",
                    path.display(),
                    config.family,
                    config.n_ctx
                );

                match LocalLlmBackend::new(&path) {
                    Ok(backend) => {
                        tracing::info!("Local LLM backend initialized successfully");
                        return Box::new(backend);
                    }
                    Err(e) => {
                        tracing::warn!("Failed to load local model: {}", e);
                        if backend_type == "local" {
                            // Explicit local requested, don't fallback
                            panic!("Local LLM backend requested but failed to initialize: {}", e);
                        }
                        tracing::info!("Falling back to API backend");
                    }
                }
            } else if backend_type == "local" {
                panic!("Local LLM backend requested but no model found");
            }

            // Fallback to API for "auto" mode
            tracing::info!("Using API LLM backend");
            Box::new(ApiLlmBackend::new(settings))
        }

        #[cfg(not(feature = "llm-local"))]
        "local" => {
            panic!(
                "Local LLM backend requested but llm-local feature not compiled. \
                 Rebuild with: cargo build --features llm-local"
            );
        }

        "api" | _ => {
            tracing::info!("Using API LLM backend");
            Box::new(ApiLlmBackend::new(settings))
        }
    }
}

/// Check if local LLM is available (for feature detection)
pub fn is_local_llm_available() -> bool {
    #[cfg(feature = "llm-local")]
    {
        resolve_model_path(&YamlSettings::default()).is_some()
    }
    #[cfg(not(feature = "llm-local"))]
    {
        false
    }
}
```

### API Backend (Fallback Implementation)

```rust
// rust/src/actions/llm_api.rs

/// API-based LLM backend (uses configured API like OpenAI, Anthropic, etc.)
pub struct ApiLlmBackend {
    client: reqwest::Client,
    api_url: String,
    api_key: Option<String>,
    model: String,
}

impl ApiLlmBackend {
    pub fn new(settings: &YamlSettings) -> Self {
        let api_config = settings.llm.as_ref();

        Self {
            client: reqwest::Client::new(),
            api_url: api_config
                .and_then(|l| l.api_url.clone())
                .unwrap_or_else(|| "https://api.openai.com/v1".to_string()),
            api_key: api_config.and_then(|l| l.api_key.clone())
                .or_else(|| std::env::var("OPENAI_API_KEY").ok()),
            model: api_config
                .and_then(|l| l.model.clone())
                .unwrap_or_else(|| "gpt-4o-mini".to_string()),
        }
    }
}

#[async_trait]
impl LlmBackend for ApiLlmBackend {
    async fn call(&self, params: LlmCallParams) -> Result<LlmCallResult, LlmError> {
        // Call OpenAI-compatible API
        let response = self.client
            .post(format!("{}/completions", self.api_url))
            .bearer_auth(self.api_key.as_ref().ok_or(LlmError::ApiError("No API key".into()))?)
            .json(&serde_json::json!({
                "model": self.model,
                "prompt": params.prompt,
                "max_tokens": params.max_tokens,
                "temperature": params.temperature,
            }))
            .send()
            .await
            .map_err(|e| LlmError::ApiError(e.to_string()))?;

        // Parse response...
        todo!("Parse API response")
    }

    async fn chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
    ) -> Result<LlmCallResult, LlmError> {
        let response = self.client
            .post(format!("{}/chat/completions", self.api_url))
            .bearer_auth(self.api_key.as_ref().ok_or(LlmError::ApiError("No API key".into()))?)
            .json(&serde_json::json!({
                "model": self.model,
                "messages": messages,
                "max_tokens": max_tokens,
                "temperature": temperature,
            }))
            .send()
            .await
            .map_err(|e| LlmError::ApiError(e.to_string()))?;

        // Parse response...
        todo!("Parse API response")
    }

    async fn embed(&self, text: &str) -> Result<Vec<f32>, LlmError> {
        // Call embeddings API
        todo!("Implement embeddings API call")
    }

    async fn stream(
        &self,
        params: LlmCallParams,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError> {
        // Streaming API call with SSE
        todo!("Implement streaming API call")
    }

    async fn stream_chat(
        &self,
        messages: Vec<ChatMessage>,
        max_tokens: usize,
        temperature: f32,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError> {
        todo!("Implement streaming chat API call")
    }
}
```

### Relevant Source Files

```
rust/src/
├── actions/
│   ├── mod.rs              # Register llm module
│   ├── llm_backend.rs      # NEW: LlmBackend trait
│   ├── llm_local.rs        # NEW: LocalLlmBackend (feature-gated)
│   └── llm_api.rs          # Existing API-based backend
└── engine/
    └── yaml.rs             # Action routing, settings parsing
```

### Testing Strategy

| Test Type | File | Description |
|-----------|------|-------------|
| Unit | `tests/test_llm_backend.rs` | Trait implementation tests |
| Integration | `tests/test_llm_local.rs` | End-to-end with TinyLlama model |
| Regression | `cargo test` | Full test suite |

### Test Model for CI

Use TinyLlama for CI testing (small, fast):
```bash
# ~500MB model for testing
wget https://huggingface.co/ggml-org/models/resolve/main/tinyllamas/stories260K.gguf
```

## Definition of Done

- [ ] `LlmBackend` trait defined with `call`, `chat`, `embed`, `stream`, `stream_chat` methods
- [ ] `LocalLlmBackend` implements trait using llama-cpp-2
- [ ] `ApiLlmBackend` implements trait for fallback to external APIs
- [ ] `llm.chat` action supports OpenAI-compatible chat format (messages array)
- [ ] Feature flag `llm-local` gates llama-cpp-2 dependency
- [ ] YAML settings support `llm.backend: local | api | auto` configuration
- [ ] YAML settings support `llm.model_path`, `llm.n_ctx`, `llm.n_threads`
- [ ] Model auto-detection from filename (Phi-4-mini 128K ctx, Gemma 32K ctx)
- [ ] Chat format auto-detection (ChatML for Phi-4-mini, Gemma format for Gemma)
- [ ] Model path resolution: TEA_MODEL_PATH → YAML → APPDIR → ~/.cache/tea/models/
- [ ] Fallback to API if local model unavailable (when backend=auto)
- [ ] Unit tests for LlmBackend trait implementation pass
- [ ] Unit tests for model path resolution and auto-config pass
- [ ] Integration test with TinyLlama test model passes
- [ ] No regressions in existing tests (`cargo test` passes)

## Risk and Compatibility Check

**Primary Risk:** llama-cpp-2 API changes or compilation issues

**Mitigation:** Pin version, add compile-time checks

**Rollback:** Feature-flagged, can be excluded

## Compatibility Verification

- [x] No breaking changes to existing APIs (additive)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: None without feature flag

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2026-01-08 | 0.2 | Expanded to full parity with Python story (TEA-RELEASE-004.5): added `llm.chat` action, model auto-detection (Phi-4-mini 128K, Gemma 32K), chat format templates (ChatML, Gemma), backend factory with fallback, YAML action examples, API backend implementation | Sarah (PO Agent) |
