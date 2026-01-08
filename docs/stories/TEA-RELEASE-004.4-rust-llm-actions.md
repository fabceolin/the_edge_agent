# Story TEA-RELEASE-004.4: Rust Local LLM Actions Integration

## Status

Draft

## Story

**As a** developer using Rust TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

## Story Context

**Existing System Integration:**

- Integrates with: Rust actions system (`rust/src/actions/`)
- Technology: Rust + llama-cpp-2 crate
- Follows pattern: Existing action modules (http, file, data)
- Touch points: `rust/Cargo.toml`, `rust/src/actions/`, `rust/src/engine/yaml.rs`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: `llm.call` action generates text using local llama-cpp-2 backend
2. **AC-2**: `llm.embed` action generates embeddings using local model
3. **AC-3**: `llm.stream` action supports streaming generation (callback-based)
4. **AC-4**: Model path configurable via YAML settings or environment variable
5. **AC-5**: Graceful fallback to API-based LLM if local model not found

### Configuration Requirements

6. **AC-6**: Feature flag `--features llm-local` enables local LLM support
7. **AC-7**: YAML settings support `llm.backend: local` configuration
8. **AC-8**: `llm.model_path` setting specifies GGUF file location
9. **AC-9**: Default model search path: `~/.cache/tea/models/`

### Quality Requirements

10. **AC-10**: Unit tests for LlmBackend trait implementation
11. **AC-11**: Integration test with small test model (TinyLlama ~500MB)
12. **AC-12**: Build without `llm-local` feature excludes llama-cpp-2 dependency
13. **AC-13**: No regressions in existing Rust tests

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-2 dependency with feature flag (AC: 6, 12)
  - [ ] Add `llama-cpp-2` to Cargo.toml as optional dependency
  - [ ] Create `llm-local` feature flag
  - [ ] Add conditional compilation attributes
  - [ ] Verify build with and without feature

- [ ] Task 2: Define LlmBackend trait (AC: 1, 2, 3, 5)
  - [ ] Create `rust/src/actions/llm_backend.rs`
  - [ ] Define `LlmBackend` trait with `call`, `embed`, `stream` methods
  - [ ] Implement `ApiLlmBackend` for existing HTTP-based LLM
  - [ ] Implement `LocalLlmBackend` for llama-cpp-2

- [ ] Task 3: Implement LocalLlmBackend (AC: 1, 2, 3, 4)
  - [ ] Create `rust/src/actions/llm_local.rs`
  - [ ] Initialize llama-cpp-2 with model path
  - [ ] Implement `call()` for text generation
  - [ ] Implement `embed()` for embeddings
  - [ ] Implement `stream()` with callback

- [ ] Task 4: Add YAML configuration support (AC: 7, 8, 9)
  - [ ] Add `llm` section to settings schema
  - [ ] Support `backend: local | api` selection
  - [ ] Support `model_path` configuration
  - [ ] Implement model path resolution (env > yaml > default)

- [ ] Task 5: Integrate with action dispatcher (AC: 5)
  - [ ] Update `rust/src/engine/yaml.rs` action routing
  - [ ] Select backend based on settings
  - [ ] Implement fallback from local to API if model missing
  - [ ] Log backend selection for debugging

- [ ] Task 6: Add tests (AC: 10, 11, 13)
  - [ ] Add unit tests for LlmBackend trait
  - [ ] Add integration test with mock model
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

#[derive(Debug, Serialize, Deserialize)]
pub struct LlmCallParams {
    pub prompt: String,
    pub max_tokens: Option<usize>,
    pub temperature: Option<f32>,
    pub stop: Option<Vec<String>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct LlmCallResult {
    pub content: String,
    pub model: String,
    pub tokens_used: Option<usize>,
}

#[async_trait]
pub trait LlmBackend: Send + Sync {
    async fn call(&self, params: LlmCallParams) -> Result<LlmCallResult, LlmError>;
    async fn embed(&self, text: &str) -> Result<Vec<f32>, LlmError>;
    async fn stream(
        &self,
        params: LlmCallParams,
        callback: Box<dyn Fn(&str) + Send>,
    ) -> Result<LlmCallResult, LlmError>;
}
```

### LocalLlmBackend Implementation

```rust
// rust/src/actions/llm_local.rs
#[cfg(feature = "llm-local")]
use llama_cpp_2::model::{LlamaModel, params::LlamaModelParams};
#[cfg(feature = "llm-local")]
use llama_cpp_2::context::params::LlamaContextParams;

#[cfg(feature = "llm-local")]
pub struct LocalLlmBackend {
    model: LlamaModel,
    model_path: PathBuf,
}

#[cfg(feature = "llm-local")]
impl LocalLlmBackend {
    pub fn new(model_path: &Path) -> Result<Self, LlmError> {
        let params = LlamaModelParams::default();
        let model = LlamaModel::load_from_file(model_path, params)?;
        Ok(Self {
            model,
            model_path: model_path.to_path_buf(),
        })
    }
}

#[cfg(feature = "llm-local")]
#[async_trait]
impl LlmBackend for LocalLlmBackend {
    async fn call(&self, params: LlmCallParams) -> Result<LlmCallResult, LlmError> {
        let ctx_params = LlamaContextParams::default();
        let mut ctx = self.model.new_context(&ctx_params)?;

        // Tokenize prompt
        let tokens = ctx.model().str_to_token(&params.prompt, AddBos::Always)?;

        // Generate
        let mut output = String::new();
        let max_tokens = params.max_tokens.unwrap_or(100);

        for _ in 0..max_tokens {
            // ... generation loop
        }

        Ok(LlmCallResult {
            content: output,
            model: self.model_path.file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_default(),
            tokens_used: Some(max_tokens),
        })
    }

    // ... embed and stream implementations
}
```

### YAML Settings Schema

```yaml
# Example workflow with local LLM settings
settings:
  llm:
    backend: local           # 'local' or 'api'
    model_path: ~/.cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf
    # Alternative: use environment variable
    # model_path: ${TEA_MODEL_PATH}

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "{{ state.question }}"
      max_tokens: 100
```

### Model Path Resolution Order

```rust
fn resolve_model_path(settings: &YamlSettings) -> Option<PathBuf> {
    // 1. Explicit environment variable
    if let Ok(path) = std::env::var("TEA_MODEL_PATH") {
        return Some(PathBuf::from(path));
    }

    // 2. YAML settings
    if let Some(path) = settings.llm.as_ref().and_then(|l| l.model_path.as_ref()) {
        return Some(PathBuf::from(path));
    }

    // 3. Default cache location
    if let Some(home) = dirs::home_dir() {
        let default = home.join(".cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf");
        if default.exists() {
            return Some(default);
        }
    }

    None
}
```

### Backend Selection Logic

```rust
fn create_llm_backend(settings: &YamlSettings) -> Box<dyn LlmBackend> {
    let backend_type = settings.llm.as_ref()
        .and_then(|l| l.backend.as_ref())
        .map(|s| s.as_str())
        .unwrap_or("api");

    match backend_type {
        #[cfg(feature = "llm-local")]
        "local" => {
            if let Some(path) = resolve_model_path(settings) {
                match LocalLlmBackend::new(&path) {
                    Ok(backend) => return Box::new(backend),
                    Err(e) => {
                        tracing::warn!("Failed to load local model: {}, falling back to API", e);
                    }
                }
            }
            // Fallback to API
            Box::new(ApiLlmBackend::new(settings))
        }
        _ => Box::new(ApiLlmBackend::new(settings)),
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

- [ ] `LlmBackend` trait defined with `call`, `embed`, `stream` methods
- [ ] `LocalLlmBackend` implements trait using llama-cpp-2
- [ ] Feature flag `llm-local` gates dependency
- [ ] YAML settings support `llm.backend` and `llm.model_path`
- [ ] Fallback to API if local model unavailable
- [ ] Unit tests pass
- [ ] Integration test with test model passes
- [ ] No regressions in existing tests

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
