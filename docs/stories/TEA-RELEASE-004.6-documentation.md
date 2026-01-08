# Story TEA-RELEASE-004.6: LLM-Bundled Distributions Documentation

## Status

Draft

## Story

**As a** developer,
**I want** documentation for LLM-bundled distributions,
**So that** I can understand how to use offline LLM capabilities.

## Story Context

**Existing System Integration:**

- Integrates with: Existing documentation (`docs/installation.md`, `examples/`)
- Technology: Markdown documentation
- Follows pattern: Existing README.md and installation docs
- Touch points: `docs/installation.md`, `examples/`, `README.md`

## Acceptance Criteria

### Documentation Requirements

1. **AC-1**: `docs/installation.md` updated with LLM AppImage download and usage instructions
2. **AC-2**: Decision flowchart helps users choose correct distribution variant
3. **AC-3**: `examples/llm/` directory with local LLM workflow examples
4. **AC-4**: WASM deployment guide with COOP/COEP header requirements
5. **AC-5**: Model path configuration documented for all platforms

### Example YAML Workflows

6. **AC-6**: `examples/llm/local-chat.yaml` - Simple chat with local model
7. **AC-7**: `examples/llm/local-embed.yaml` - Embedding generation example
8. **AC-8**: `examples/llm/local-rag.yaml` - RAG workflow with local embeddings
9. **AC-9**: Examples work with bundled AppImage without modification

### Reference Documentation

10. **AC-10**: `llm.call` action reference with local backend parameters
11. **AC-11**: `llm.embed` action reference with local backend parameters
12. **AC-12**: `llm.stream` action reference with callback pattern
13. **AC-13**: YAML settings reference for `llm` section

## Tasks / Subtasks

- [ ] Task 1: Update installation documentation (AC: 1, 2, 5)
  - [ ] Add "LLM-Bundled Distributions" section to installation.md
  - [ ] Add download links for LLM AppImages (Rust, Python)
  - [ ] Add download links for WASM LLM package
  - [ ] Create Mermaid decision flowchart for distribution selection
  - [ ] Document model path configuration options

- [ ] Task 2: Create WASM deployment guide (AC: 4)
  - [ ] Create `docs/wasm/llm-deployment.md`
  - [ ] Document COOP/COEP header requirements
  - [ ] Add nginx/Apache configuration examples
  - [ ] Document IndexedDB caching behavior
  - [ ] Add troubleshooting section

- [ ] Task 3: Create example YAML workflows (AC: 6, 7, 8, 9)
  - [ ] Create `examples/llm/` directory
  - [ ] Create `local-chat.yaml` - simple chat example
  - [ ] Create `local-embed.yaml` - embedding example
  - [ ] Create `local-rag.yaml` - RAG workflow example
  - [ ] Verify examples work with bundled AppImage

- [ ] Task 4: Update actions reference (AC: 10, 11, 12, 13)
  - [ ] Update `docs/python/actions-reference.md` with local LLM actions
  - [ ] Update `docs/rust/actions-reference.md` with local LLM actions
  - [ ] Document `llm` settings section in YAML reference
  - [ ] Add examples for each action

- [ ] Task 5: Update README with quick start (AC: 1)
  - [ ] Add "Offline LLM" section to README
  - [ ] Link to detailed installation docs
  - [ ] Add one-liner example

## Dev Notes

### Installation.md Structure Update

```markdown
## Distribution Selection

### Quick Decision Guide

```mermaid
flowchart TD
    A[Need LLM capabilities?] -->|No| B[Standard AppImage]
    A -->|Yes| C[Need offline/bundled model?]
    C -->|No| D[Standard + API LLM]
    C -->|Yes| E[LLM-Bundled AppImage ~5GB]

    B --> F[tea-{version}-{arch}.AppImage]
    D --> F
    E --> G[tea-rust-llm-{version}-{arch}.AppImage<br/>or tea-python-llm-{version}-{arch}.AppImage]
```

### LLM-Bundled Distributions

These distributions include the Gemma 3n E4B model (~4.5GB) for offline LLM inference.

| Distribution | Size | Use Case |
|-------------|------|----------|
| `tea-rust-llm-{version}-x86_64.AppImage` | ~5GB | Rust CLI with bundled model |
| `tea-rust-llm-{version}-aarch64.AppImage` | ~5GB | Rust CLI ARM64 with bundled model |
| `tea-python-llm-{version}-x86_64.AppImage` | ~5GB | Python CLI with bundled model |
| `tea-python-llm-{version}-aarch64.AppImage` | ~5GB | Python CLI ARM64 with bundled model |

### Download and Run

```bash
# Download (replace version)
wget https://github.com/your-org/the_edge_agent/releases/download/v0.9.4/tea-rust-llm-0.9.4-x86_64.AppImage
chmod +x tea-rust-llm-0.9.4-x86_64.AppImage

# Run LLM workflow
./tea-rust-llm-0.9.4-x86_64.AppImage run examples/llm/local-chat.yaml --input '{"question": "What is 2+2?"}'
```
```

### Example: local-chat.yaml

```yaml
# examples/llm/local-chat.yaml
# Simple chat workflow using bundled local model
name: local-chat
description: Chat with local Gemma 3n E4B model

settings:
  llm:
    backend: local
    # Model path auto-detected in AppImage
    # Override with: model_path: /path/to/model.gguf

state_schema:
  question: str
  answer: str

nodes:
  - name: generate_response
    action: llm.call
    params:
      prompt: |
        You are a helpful assistant.

        User: {{ state.question }}

        Assistant:
      max_tokens: 200
      temperature: 0.7
    output: answer

edges:
  - from: __start__
    to: generate_response
  - from: generate_response
    to: __end__
```

### Example: local-embed.yaml

```yaml
# examples/llm/local-embed.yaml
# Generate embeddings using local model
name: local-embed
description: Generate text embeddings with local model

settings:
  llm:
    backend: local

state_schema:
  text: str
  embedding: list

nodes:
  - name: generate_embedding
    action: llm.embed
    params:
      text: "{{ state.text }}"
    output: embedding

edges:
  - from: __start__
    to: generate_embedding
  - from: generate_embedding
    to: __end__
```

### Example: local-rag.yaml

```yaml
# examples/llm/local-rag.yaml
# RAG workflow with local embeddings and generation
name: local-rag
description: Retrieval-Augmented Generation with local model

settings:
  llm:
    backend: local

state_schema:
  query: str
  context: str
  answer: str

nodes:
  - name: embed_query
    action: llm.embed
    params:
      text: "{{ state.query }}"
    output: query_embedding

  - name: search_context
    action: memory.retrieve
    params:
      embedding: "{{ state.query_embedding }}"
      top_k: 3
    output: context

  - name: generate_answer
    action: llm.call
    params:
      prompt: |
        Use the following context to answer the question.

        Context:
        {{ state.context }}

        Question: {{ state.query }}

        Answer:
      max_tokens: 300
    output: answer

edges:
  - from: __start__
    to: embed_query
  - from: embed_query
    to: search_context
  - from: search_context
    to: generate_answer
  - from: generate_answer
    to: __end__
```

### WASM Deployment Guide Structure

```markdown
# WASM LLM Deployment Guide

## Overview

Deploy TEA with bundled LLM in the browser using WebAssembly.

## Requirements

### HTTP Headers (Required for Multi-threading)

Your web server MUST send these headers for multi-threaded inference:

```
Cross-Origin-Opener-Policy: same-origin
Cross-Origin-Embedder-Policy: require-corp
```

### Nginx Configuration

```nginx
server {
    location / {
        add_header Cross-Origin-Opener-Policy same-origin;
        add_header Cross-Origin-Embedder-Policy require-corp;
        # ... your other config
    }
}
```

### Apache Configuration

```apache
<IfModule mod_headers.c>
    Header set Cross-Origin-Opener-Policy "same-origin"
    Header set Cross-Origin-Embedder-Policy "require-corp"
</IfModule>
```

## Download and Setup

1. Download from GitHub Releases:
   - `tea-wasm-llm-{version}.tar.gz` (WASM package)
   - `gemma-3n-E4B-it-Q4_K_M.chunk-*` (model chunks)
   - `model-manifest.json` (chunk metadata)

2. Extract and serve:
   ```bash
   tar -xzf tea-wasm-llm-0.9.4.tar.gz
   cat gemma-3n-E4B-it-Q4_K_M.chunk-* > models/gemma-3n-E4B-it-Q4_K_M.gguf
   python -m http.server 8080
   ```

## Usage

```html
<script type="module">
import { initTeaLlm, executeLlmYaml } from './tea-wasm-llm/index.js';

await initTeaLlm();
const result = await executeLlmYaml(yaml, { question: "Hello!" });
</script>
```
```

### Actions Reference Addition

```markdown
## llm.call (Local Backend)

Generate text using local llama.cpp model.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `prompt` | string | Yes | - | Text prompt for generation |
| `max_tokens` | int | No | 100 | Maximum tokens to generate |
| `temperature` | float | No | 0.7 | Sampling temperature (0.0-2.0) |
| `stop` | list[str] | No | [] | Stop sequences |

### Settings

Configure in workflow `settings.llm`:

| Setting | Type | Default | Description |
|---------|------|---------|-------------|
| `backend` | string | "api" | "local" or "api" |
| `model_path` | string | auto | Path to GGUF model file |
| `n_ctx` | int | 2048 | Context window size |
| `n_threads` | int | 4 | CPU threads for inference |

### Example

```yaml
settings:
  llm:
    backend: local
    model_path: ~/.cache/tea/models/gemma-3n-E4B-it-Q4_K_M.gguf

nodes:
  - name: generate
    action: llm.call
    params:
      prompt: "Explain quantum computing in simple terms."
      max_tokens: 200
```
```

### README Quick Start Addition

```markdown
## Offline LLM Support

Run LLM workflows without internet using bundled models:

```bash
# Download LLM-bundled AppImage (~5GB)
wget https://github.com/.../tea-rust-llm-0.9.4-x86_64.AppImage
chmod +x tea-rust-llm-0.9.4-x86_64.AppImage

# Run offline chat
./tea-rust-llm-0.9.4-x86_64.AppImage run examples/llm/local-chat.yaml \
  --input '{"question": "What is the meaning of life?"}'
```

See [LLM Installation Guide](docs/installation.md#llm-bundled-distributions) for details.
```

## Definition of Done

- [ ] `docs/installation.md` updated with LLM AppImage section
- [ ] Decision flowchart created (Mermaid)
- [ ] `examples/llm/` directory with 3 examples
- [ ] `docs/wasm/llm-deployment.md` created
- [ ] Actions reference updated for local LLM
- [ ] YAML settings reference updated
- [ ] README updated with quick start
- [ ] All examples tested with bundled AppImage

## Risk and Compatibility Check

**Primary Risk:** Documentation out of sync with implementation

**Mitigation:** Create documentation alongside implementation stories

**Rollback:** Documentation is additive, no rollback needed

## Compatibility Verification

- [x] No breaking changes (documentation only)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: None

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
