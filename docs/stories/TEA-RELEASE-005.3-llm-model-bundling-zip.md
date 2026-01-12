# TEA-RELEASE-005.3: LLM Model Bundling via ZIP Append

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.3 |
| **Type** | Story |
| **Priority** | High |
| **Estimated Effort** | 5 points |
| **Status** | Draft |
| **Parent Epic** | TEA-RELEASE-005 |
| **Depends On** | TEA-RELEASE-005.2 (Cosmopolitan build pipeline) |
| **Files to Create** | `scripts/bundle-model.sh`, `rust/src/model_loader.rs` |
| **Files to Modify** | `rust/src/actions/llm.rs`, `.github/workflows/build-ape.yaml` |

## Story

**As a** user,
**I want** a single `tea-llm.com` file with bundled LLM model,
**So that** I can run offline AI inference on any platform without downloading models separately.

## Background

### ZIP Append Technique

Cosmopolitan APE binaries support appending ZIP archives to the end of the executable. At runtime, these files are accessible via a virtual `/zip/` filesystem:

```bash
# Append model to binary
zip -A tea.com models/phi4-mini.gguf

# At runtime, model is available at:
# /zip/models/phi4-mini.gguf
```

This provides:
- **Single file distribution** - binary + model in one file
- **No extraction needed** - model is memory-mapped directly
- **Instant startup** - no temp folder creation

### Model Selection

| Model | Size | Context | Use Case |
|-------|------|---------|----------|
| Phi-4-mini Q3_K_S | ~1.9GB | 128K tokens | Default - fits GitHub 2GB limit |
| Gemma 3 1B Q8_0 | ~1.07GB | 8K tokens | Ultra-lightweight alternative |

## Acceptance Criteria

- [ ] **AC-1**: Model bundling script `scripts/bundle-model.sh` created
- [ ] **AC-2**: Model appended to binary via `zip -A` command
- [ ] **AC-3**: `/zip/` virtual filesystem accessible at runtime
- [ ] **AC-4**: Model loads from `/zip/models/` path without extraction
- [ ] **AC-5**: `llm.call` action detects and uses bundled model automatically
- [ ] **AC-6**: `tea-llm.com` artifact produced (~2GB with Phi-4-mini)
- [ ] **AC-7**: Startup time <3 seconds (no extraction delay)
- [ ] **AC-8**: LLM inference works correctly on Linux, Windows, macOS
- [ ] **AC-9**: Fallback to `~/.cache/tea/models/` if bundled model not found

## Technical Design

### Model Bundling Script

```bash
#!/bin/bash
# scripts/bundle-model.sh

set -e

MODEL_NAME="${1:-microsoft_Phi-4-mini-instruct-Q3_K_S.gguf}"
MODEL_URL="https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/${MODEL_NAME}"
OUTPUT_NAME="${2:-tea-llm.com}"

# Download model if not present
if [ ! -f "models/${MODEL_NAME}" ]; then
    echo "Downloading ${MODEL_NAME}..."
    mkdir -p models
    wget -O "models/${MODEL_NAME}" "${MODEL_URL}"
fi

# Copy base APE binary
cp tea.com "${OUTPUT_NAME}"

# Append model as ZIP
echo "Bundling model into ${OUTPUT_NAME}..."
cd models
zip -A "../${OUTPUT_NAME}" "${MODEL_NAME}"
cd ..

# Create manifest
cat > models/manifest.json << EOF
{
  "model": "${MODEL_NAME}",
  "bundled_at": "$(date -Iseconds)",
  "sha256": "$(sha256sum models/${MODEL_NAME} | cut -d' ' -f1)"
}
EOF
zip -A "${OUTPUT_NAME}" models/manifest.json

echo "Created ${OUTPUT_NAME}"
ls -lh "${OUTPUT_NAME}"
```

### Model Loader Implementation

```rust
// rust/src/model_loader.rs
use std::path::PathBuf;
use std::fs;

/// Locations to search for LLM model (in order of priority)
const MODEL_SEARCH_PATHS: &[&str] = &[
    "/zip/models/",                           // Bundled in APE
    "${TEA_MODEL_PATH}",                      // Environment variable
    "${HOME}/.cache/tea/models/",             // User cache
    "/usr/share/tea/models/",                 // System-wide
];

const DEFAULT_MODEL_NAMES: &[&str] = &[
    "microsoft_Phi-4-mini-instruct-Q3_K_S.gguf",
    "phi4-mini.gguf",
    "gemma-3-1b-it-Q8_0.gguf",
];

pub struct ModelLoader;

impl ModelLoader {
    /// Find the first available model from search paths
    pub fn find_model() -> Option<PathBuf> {
        for base_path in MODEL_SEARCH_PATHS {
            let expanded = Self::expand_path(base_path);

            for model_name in DEFAULT_MODEL_NAMES {
                let full_path = PathBuf::from(&expanded).join(model_name);
                if full_path.exists() || Self::is_zip_path(&full_path) {
                    return Some(full_path);
                }
            }
        }
        None
    }

    /// Check if path is in /zip/ virtual filesystem
    fn is_zip_path(path: &PathBuf) -> bool {
        path.starts_with("/zip/")
    }

    /// Expand environment variables in path
    fn expand_path(path: &str) -> String {
        let mut result = path.to_string();
        if let Ok(home) = std::env::var("HOME") {
            result = result.replace("${HOME}", &home);
        }
        if let Ok(model_path) = std::env::var("TEA_MODEL_PATH") {
            result = result.replace("${TEA_MODEL_PATH}", &model_path);
        }
        result
    }

    /// Load model bytes (handles both file and /zip/ paths)
    pub fn load_model_bytes(path: &PathBuf) -> Result<Vec<u8>, std::io::Error> {
        // Cosmopolitan's libc handles /zip/ paths transparently
        fs::read(path)
    }
}
```

### LLM Action Integration

```rust
// rust/src/actions/llm.rs (modification)
use crate::model_loader::ModelLoader;

impl LlmAction {
    pub fn execute(&self, params: &LlmParams) -> Result<LlmResult, ActionError> {
        // Priority: explicit path > bundled > cache > error
        let model_path = params.model_path
            .clone()
            .or_else(|| ModelLoader::find_model())
            .ok_or_else(|| ActionError::ModelNotFound(
                "No LLM model found. Provide model_path or use tea-llm.com".into()
            ))?;

        // Load model (works for both /zip/ and regular paths)
        let model_bytes = ModelLoader::load_model_bytes(&model_path)?;

        // Initialize llama.cpp with model bytes
        self.backend.load_from_bytes(&model_bytes)?;

        // Execute inference...
    }
}
```

### CI Workflow Addition

```yaml
# Addition to .github/workflows/build-ape.yaml

  build-ape-llm:
    needs: build-ape
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - uses: actions/download-artifact@v4
        with:
          name: tea-ape

      - name: Download Phi-4-mini model
        run: |
          mkdir -p models
          wget -O models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf \
            "https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF/resolve/main/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf"

      - name: Bundle model into APE
        run: |
          chmod +x scripts/bundle-model.sh
          ./scripts/bundle-model.sh

      - name: Verify bundled binary
        run: |
          ./tea-llm.com --version
          # Quick LLM test
          echo '{"prompt": "Hello"}' | ./tea-llm.com run examples/llm/simple-chat.yaml

      - name: Upload artifact
        uses: actions/upload-artifact@v4
        with:
          name: tea-ape-llm
          path: tea-llm.com
```

## Tasks / Subtasks

- [ ] **Task 1: Create bundling script** (AC: 1, 2)
  - [ ] Create `scripts/bundle-model.sh`
  - [ ] Add model download logic
  - [ ] Add ZIP append step
  - [ ] Create manifest.json

- [ ] **Task 2: Implement model loader** (AC: 3, 4, 5)
  - [ ] Create `rust/src/model_loader.rs`
  - [ ] Implement `/zip/` path detection
  - [ ] Add search path priority
  - [ ] Integrate with `llm.call` action

- [ ] **Task 3: Update CI workflow** (AC: 6)
  - [ ] Add `build-ape-llm` job
  - [ ] Download model in CI
  - [ ] Bundle and upload artifact

- [ ] **Task 4: Performance validation** (AC: 7)
  - [ ] Measure startup time
  - [ ] Verify no temp extraction
  - [ ] Document memory mapping behavior

- [ ] **Task 5: Cross-platform testing** (AC: 8)
  - [ ] Test LLM inference on Linux
  - [ ] Test LLM inference on Windows
  - [ ] Test LLM inference on macOS

- [ ] **Task 6: Fallback handling** (AC: 9)
  - [ ] Implement graceful fallback
  - [ ] Add helpful error message
  - [ ] Document model installation for non-bundled binary

## Dev Notes

### ZIP Virtual Filesystem

Cosmopolitan provides transparent access to ZIP-appended files:

```c
// This "just works" in Cosmopolitan
FILE *f = fopen("/zip/models/model.gguf", "rb");
```

The `/zip/` path is handled by Cosmopolitan's libc, which:
1. Detects the path prefix
2. Finds the ZIP central directory at end of executable
3. Memory-maps the file from within the executable
4. Returns a file handle as if it were a regular file

### Memory Considerations

| Scenario | Memory Usage |
|----------|--------------|
| Regular file | ~2GB mmap'd from disk |
| /zip/ bundled | ~2GB mmap'd from executable |
| Extraction | ~4GB (copy in temp + mmap) |

Bundling avoids the extraction overhead entirely.

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Unit | `rust/src/model_loader.rs` | Path resolution tests |
| Integration | CI workflow | Model loading from /zip/ |
| E2E | Manual | Full LLM inference test |

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

