# TEA-RELEASE-004: LLM-Bundled Distributions (AppImage + WASM)

## Status

Draft

## Epic Goal

Provide self-contained TEA distributions with bundled Gemma 3n E4B GGUF model for offline LLM inference across Python AppImage, Rust AppImage, and WebAssembly platforms.

## Epic Description

### Existing System Context

- **Current relevant functionality:** TEA-RELEASE-002/003 established AppImage build patterns with Prolog bundling using linuxdeploy
- **Technology stack:** PyInstaller (Python), Cargo (Rust), wasm-pack (WASM), llama.cpp ecosystem
- **Integration points:** GitHub Actions release workflow, existing `tea` CLI, TEA-WASM-001 spike

**IMPORTANT:** Existing AppImages remain unchanged. This epic adds **new LLM-bundled variants** as additional release artifacts.

### Enhancement Details

- **What's being added:** Three new distribution variants with bundled Gemma 3n E4B model:
  1. Python AppImage with llama-cpp-python
  2. Rust AppImage with llama-cpp-2 crate
  3. WASM web package with wllama (model bundled in package)

- **Model specification:**
  - Model: [gemma3n:e4b](https://ollama.com/library/gemma3n) / [GGUF](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF)
  - Ollama size: 7.5GB (full precision)
  - GGUF Q4_K_M: ~4.5GB (recommended quantization)
  - Architecture: Effective 4B parameters with selective parameter activation
  - Context: 32K tokens
  - Performance: Superior to e2b across all benchmarks (78.6% vs 72.2% HellaSwag)

- **How it integrates:** Extends existing release workflow with new build matrix jobs
- **Success criteria:** Users can run LLM-powered YAML agents offline without installing models separately

### Alignment with TEA-WASM-001 Findings

The WASM feasibility spike validated:
- wllama callback bridge pattern works for browser LLM inference
- Model loading from CDN/IndexedDB confirmed
- **This epic extends to bundled model distribution**

## Stories

### Story 1: TEA-RELEASE-004.1 - Rust LLM AppImage (x86_64 + ARM64)

**As a** developer using The Edge Agent,
**I want** a Rust AppImage with bundled Gemma 3n E4B model,
**So that** I can run LLM-powered workflows offline without installing llama.cpp or downloading models.

**Key deliverables:**
- Add `llama-cpp-2` crate dependency with `cuda` and `metal` feature flags
- Build `tea-rust-llm-{version}-{arch}.AppImage` (~4.5GB)
- Bundle `gemma-3n-E4B-it-Q4_K_M.gguf` in AppImage
- Add `llm.call` action routing to local llama.cpp
- GitHub Actions job for x86_64 and ARM64

### Story 2: TEA-RELEASE-004.2 - Python LLM AppImage (x86_64 + ARM64)

**As a** developer using The Edge Agent Python,
**I want** a Python AppImage with bundled Gemma 3n E4B model,
**So that** I can run LLM-powered workflows offline without installing llama-cpp-python or downloading models.

**Key deliverables:**
- Add `llama-cpp-python` to PyInstaller build
- Build `tea-python-llm-{version}-{arch}.AppImage` (~4.5GB)
- Bundle `gemma-3n-E4B-it-Q4_K_M.gguf` in AppImage
- Configure model path via environment variable
- GitHub Actions job for x86_64 and ARM64

### Story 3: TEA-RELEASE-004.3 - WASM Web Package with Bundled Model (SPLIT)

**Split into 3 sub-stories for better manageability:**

#### Story 3a: TEA-RELEASE-004.3a - WASM LLM Core Package
- Create `tea-wasm-llm` Rust crate with wasm-pack
- Port wllama callback bridge from TEA-WASM-001 spike
- TypeScript wrapper with `initTeaLlm()` and `executeLlmYaml()`
- Test with tiny model (~500KB)

#### Story 3b: TEA-RELEASE-004.3b - Model Bundling and Caching
- Model chunking script for GitHub 2GB limit
- Model loader with chunk reassembly
- IndexedDB caching for persistent storage
- Cache invalidation on version change

#### Story 3c: TEA-RELEASE-004.3c - Release and Testing
- GitHub Actions workflow for release
- Playwright browser tests
- Package README and COOP/COEP documentation
- Troubleshooting guide

### Story 4: TEA-RELEASE-004.4 - LLM Actions Integration (Rust Native)

**As a** developer using Rust TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

**Key deliverables:**
- Implement `LlmBackend` trait with llama-cpp-2 backend
- Support `llm.call`, `llm.stream`, `llm.embed` actions
- Model path configuration via settings or environment
- Feature flag: `--features llm-local`

### Story 5: TEA-RELEASE-004.5 - LLM Actions Integration (Python Native)

**As a** developer using Python TEA,
**I want** `llm.call` and `llm.embed` actions to work with local llama.cpp,
**So that** YAML workflows can use LLM capabilities without external API calls.

**Key deliverables:**
- Implement local LLM backend using llama-cpp-python
- Support `llm.call`, `llm.stream`, `llm.embed` actions
- Model path configuration via settings YAML
- Graceful fallback to API-based LLM if model not found

### Story 6: TEA-RELEASE-004.6 - Documentation and Examples

**As a** developer,
**I want** documentation for LLM-bundled distributions,
**So that** I can understand how to use offline LLM capabilities.

**Key deliverables:**
- Update installation.md with LLM AppImage instructions
- Add YAML examples using local LLM (`examples/llm/local-chat.yaml`)
- Document WASM deployment with bundled model
- Add decision flowchart for LLM distribution selection

## Compatibility Requirements

- [x] Existing APIs remain unchanged (new `llm.call` action, backward compatible)
- [x] No database schema changes
- [x] UI changes: N/A (CLI tool)
- [x] Performance impact: Large file sizes (~4.5GB), acceptable for bundled distributions

## Risk Mitigation

- **Primary Risk:** AppImage size (4.5GB) may be too large for some distribution channels
- **Mitigation:**
  - Offer both bundled and non-bundled variants
  - Use GitHub Releases (2GB limit per file) - may need to split or use external hosting
  - Document minimum disk space requirements
- **Rollback Plan:** Delete LLM-bundled artifacts; non-LLM AppImages remain available

## Artifact Naming Convention

### New LLM-Bundled Artifacts (This Epic)

| Artifact | Size (est.) | Description |
|----------|-------------|-------------|
| `tea-rust-llm-{version}-x86_64.AppImage` | ~5GB | Rust + llama-cpp-2 + gemma3n:e4b Q4_K_M |
| `tea-rust-llm-{version}-aarch64.AppImage` | ~5GB | Rust ARM64 + llama-cpp-2 + gemma3n:e4b Q4_K_M |
| `tea-python-llm-{version}-x86_64.AppImage` | ~5GB | Python + llama-cpp-python + gemma3n:e4b Q4_K_M |
| `tea-python-llm-{version}-aarch64.AppImage` | ~5GB | Python ARM64 + llama-cpp-python + gemma3n:e4b Q4_K_M |
| `tea-wasm-llm-{version}.tar.gz` + model chunks (GitHub) | ~4.5GB | WASM + wllama + gemma3n:e4b Q4_K_M bundled |

### Existing Artifacts (Unchanged)

| Artifact | Size | Description |
|----------|------|-------------|
| `tea-{version}-x86_64.AppImage` | ~50MB | Rust + Prolog (from TEA-RELEASE-002) |
| `tea-{version}-aarch64.AppImage` | ~50MB | Rust ARM64 + Prolog |
| `tea-python-{version}-x86_64.AppImage` | ~150MB | Python full + Prolog (from TEA-RELEASE-003) |
| `tea-python-{version}-aarch64.AppImage` | ~150MB | Python ARM64 full + Prolog |

## Technical Constraints

### Model Distribution Limits

| Platform | Limit | Strategy |
|----------|-------|----------|
| GitHub Releases | 2GB per file | Split model or use external hosting |
| npm | 500MB per package | Split into multiple packages or external CDN |
| Docker Hub | No strict limit | Alternative distribution method |

### Recommended Quantization

| Quant | Size | Quality | Recommendation |
|-------|------|---------|----------------|
| Q4_K_M | 4.54GB | Good | **Default** - best balance |
| Q4_K_S | 4.40GB | Good | Alternative if size critical |
| Q3_K_M | 3.69GB | Acceptable | Minimum viable for edge |
| Q6_K | 6.27GB | Better | If quality is priority |

### LLM Backend Libraries

| Platform | Library | Notes |
|----------|---------|-------|
| Rust Native | [llama-cpp-2](https://crates.io/crates/llama-cpp-2) | Bindgen-based, most maintained |
| Python Native | [llama-cpp-python](https://github.com/abetlen/llama-cpp-python) | OpenAI-compatible API |
| WASM Browser | [wllama](https://github.com/ngxson/wllama) | Multi-threading requires COOP/COEP |

## Dependencies

- TEA-RELEASE-002 (Done) - Rust AppImage build patterns
- TEA-RELEASE-003 (Done) - Python AppImage build patterns
- TEA-WASM-001 (Done) - WASM feasibility spike with wllama integration
- [ggml-org/gemma-3n-E4B-it-GGUF](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF) - Model source

## Definition of Done

- [ ] All 8 stories completed with acceptance criteria met (6 original + 2 from Story 3 split)
- [ ] Rust LLM AppImage builds for x86_64 and ARM64
- [ ] Python LLM AppImage builds for x86_64 and ARM64
- [ ] WASM package with bundled model published
- [ ] `llm.call` action works offline in all three platforms
- [ ] Documentation updated with usage instructions
- [ ] No regression in existing non-LLM builds
- [ ] Example YAML workflows demonstrate offline LLM usage

## Out of Scope

- GPU acceleration in WASM (WebGPU support not yet stable)
- macOS/Windows LLM bundles (future story)
- Model fine-tuning or training capabilities
- Multiple model support (future enhancement)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial epic draft | Sarah (PO Agent) |
| 2026-01-08 | 0.2 | Split Story 3 into 3a/3b/3c for manageability | Bob (SM Agent) |

---

## References

- [Gemma 3n on Ollama](https://ollama.com/library/gemma3n) - Official model page
- [Gemma 3n E4B GGUF on HuggingFace](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF) - GGUF quantized versions
- [llama-cpp-2 Rust crate](https://crates.io/crates/llama-cpp-2)
- [llama-cpp-python](https://github.com/abetlen/llama-cpp-python)
- [wllama - llama.cpp in browser](https://github.com/ngxson/wllama)
- [TEA-WASM-001 Feasibility Report](../rust/wasm-feasibility.md)
