# TEA-RELEASE-004: LLM-Bundled Distributions (AppImage + WASM)

## Status

**✅ Implemented**

_Status updated: 2026-01-13 - Implementation verified in codebase._
- `.github/workflows/docker-build.yaml` - Docker image build pipeline
- `docker/Dockerfile.gemma3-1b` - Gemma 3 1B variant
- `docker/Dockerfile.gemma3-4b` - Gemma 3 4B variant
- `docker/Dockerfile.gemma3n-e4b` - Gemma 3n E4B variant
- `docker/Dockerfile.phi4-mini` - Phi-4 mini variant
- `.github/workflows/build-python-llm.yaml` - Python LLM distribution
- `.github/workflows/build-rust-llm.yaml` - Rust LLM distribution
- `.github/workflows/build-python-appimage.yaml` - Python AppImage
- `.github/workflows/build-rust-appimage.yaml` - Rust AppImage

**Updated:** 2026-01-09
**Notes:** Epic validated with comprehensive QA test design (127 test scenarios). Quality gate passed with concerns noted. All 9 stories defined with acceptance criteria. Triple-model strategy: Gemma 3n E4B (best quality, ~5GB), Phi-4-mini (128K context, ~2GB), Gemma 3 1B (ultra-lightweight, ~1GB).

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

- **Model specifications:**

  **Primary (AppImage - Best Quality):** Gemma 3n E4B
  - Source: [GGUF](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF)
  - GGUF Q4_K_M: ~4.54GB
  - Context: 32K tokens
  - Use case: Maximum quality, offline desktop deployment

  **Lightweight (AppImage + WASM):** Phi-4-mini
  - Source: [GGUF](https://huggingface.co/bartowski/microsoft_Phi-4-mini-instruct-GGUF)
  - GGUF Q3_K_S: ~1.90GB (fits GitHub 2GB limit - no chunking needed)
  - Context: 128K tokens (4x Gemma)
  - Use case: Browser/WASM deployment, smaller downloads, extended context tasks

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

### Story 7: TEA-RELEASE-004.7 - Gemma 3 1B Variant (Ultra-Lightweight)

**As a** developer using The Edge Agent,
**I want** AppImage and WASM distributions with bundled Gemma 3 1B model (Q8_0, ~1.07GB),
**So that** I can run LLM-powered workflows on resource-constrained devices with minimal download size.

**Key deliverables:**
- Rust AppImage variants: `tea-rust-llm-gemma3-1b-{version}-{arch}.AppImage` (~1.5GB)
- Python AppImage variants: `tea-python-llm-gemma3-1b-{version}-{arch}.AppImage` (~1.5GB)
- WASM support for Gemma 3 1B model loading
- Bundle `gemma-3-1b-it-Q8_0.gguf` from [unsloth/gemma-3-1b-it-GGUF](https://huggingface.co/unsloth/gemma-3-1b-it-GGUF)
- GitHub Actions jobs for x86_64 and ARM64

**Model specifications:**
- Source: `unsloth/gemma-3-1b-it-GGUF`
- Size: 1.07GB (Q8_0 quantization)
- Context: 8K tokens
- Use case: Edge devices, fast prototyping, minimal footprint

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

**Gemma 3n E4B Variants (Best Quality - ~5GB)**

| Artifact | Size (est.) | Description |
|----------|-------------|-------------|
| `tea-rust-llm-gemma-{version}-x86_64.AppImage` | ~5GB | Rust + Gemma 3n E4B Q4_K_M (best quality) |
| `tea-rust-llm-gemma-{version}-aarch64.AppImage` | ~5GB | Rust ARM64 + Gemma 3n E4B Q4_K_M |
| `tea-python-llm-gemma-{version}-x86_64.AppImage` | ~5GB | Python + Gemma 3n E4B Q4_K_M (best quality) |
| `tea-python-llm-gemma-{version}-aarch64.AppImage` | ~5GB | Python ARM64 + Gemma 3n E4B Q4_K_M |

**Phi-4-mini Variants (Smaller Download - ~2.5GB)**

| Artifact | Size (est.) | Description |
|----------|-------------|-------------|
| `tea-rust-llm-phi4-{version}-x86_64.AppImage` | ~2.5GB | Rust + Phi-4-mini Q3_K_S (128K context) |
| `tea-rust-llm-phi4-{version}-aarch64.AppImage` | ~2.5GB | Rust ARM64 + Phi-4-mini Q3_K_S |
| `tea-python-llm-phi4-{version}-x86_64.AppImage` | ~2.5GB | Python + Phi-4-mini Q3_K_S (128K context) |
| `tea-python-llm-phi4-{version}-aarch64.AppImage` | ~2.5GB | Python ARM64 + Phi-4-mini Q3_K_S |

**Gemma 3 1B Variants (Ultra-Lightweight - ~1.5GB)**

| Artifact | Size (est.) | Description |
|----------|-------------|-------------|
| `tea-rust-llm-gemma3-1b-{version}-x86_64.AppImage` | ~1.5GB | Rust + Gemma 3 1B Q8_0 (edge devices) |
| `tea-rust-llm-gemma3-1b-{version}-aarch64.AppImage` | ~1.5GB | Rust ARM64 + Gemma 3 1B Q8_0 |
| `tea-python-llm-gemma3-1b-{version}-x86_64.AppImage` | ~1.5GB | Python + Gemma 3 1B Q8_0 (edge devices) |
| `tea-python-llm-gemma3-1b-{version}-aarch64.AppImage` | ~1.5GB | Python ARM64 + Gemma 3 1B Q8_0 |

**WASM Browser Package (Phi-4-mini + Gemma 3 1B)**

| Artifact | Size (est.) | Description |
|----------|-------------|-------------|
| `tea-wasm-llm-{version}.tar.gz` | ~50MB | WASM + wllama package |
| `microsoft_Phi-4-mini-instruct-Q3_K_S.gguf` | ~1.9GB | Phi-4-mini model (128K context) |
| `gemma-3-1b-it-Q8_0.gguf` | ~1.07GB | Gemma 3 1B model (smallest, 8K context) |

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
- [ggml-org/gemma-3n-E4B-it-GGUF](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF) - Gemma 3n E4B model source
- [unsloth/gemma-3-1b-it-GGUF](https://huggingface.co/unsloth/gemma-3-1b-it-GGUF) - Gemma 3 1B model source

## Definition of Done

- [ ] All 9 stories completed with acceptance criteria met (6 original + 2 from Story 3 split + Story 7)
- [ ] Rust LLM AppImage builds for x86_64 and ARM64 (Gemma, Phi-4-mini, Gemma 3 1B variants)
- [ ] Python LLM AppImage builds for x86_64 and ARM64 (Gemma, Phi-4-mini, Gemma 3 1B variants)
- [ ] WASM package with bundled model published (Phi-4-mini + Gemma 3 1B support)
- [ ] `llm.call` action works offline in all three platforms
- [ ] Documentation updated with usage instructions
- [ ] No regression in existing non-LLM builds
- [ ] Example YAML workflows demonstrate offline LLM usage
- [ ] Gemma 3 1B ultra-lightweight variants available for edge deployment

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
| 2026-01-08 | 0.3 | Dual-model strategy: Gemma for AppImage (quality), Phi-4-mini for WASM/smaller (1.9GB, no chunking) | Sarah (PO Agent) |
| 2026-01-09 | 0.4 | Added Story 7: Gemma 3 1B ultra-lightweight variant (~1.07GB) for edge devices | Sarah (PO Agent) |

---

## QA Notes

**Assessment Date:** 2026-01-08
**Test Architect:** Quinn (QA Agent)
**Test Design Reference:** `docs/qa/assessments/TEA-RELEASE-004-test-design-20260108.md`

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total Test Scenarios** | 127 |
| **Unit Tests** | 52 (41%) |
| **Integration Tests** | 48 (38%) |
| **E2E Tests** | 27 (21%) |

**Priority Distribution:**
- **P0 (Critical):** 34 tests - Must pass before release
- **P1 (High):** 51 tests - Should pass
- **P2 (Medium):** 32 tests - Full regression
- **P3 (Low):** 10 tests - Release candidates only

**Coverage by Story:**

| Story | Unit | Int | E2E | Total |
|-------|------|-----|-----|-------|
| 004.1 - Rust LLM AppImage | 8 | 9 | 4 | 21 |
| 004.2 - Python LLM AppImage | 8 | 9 | 4 | 21 |
| 004.3a - WASM LLM Core | 7 | 6 | 3 | 16 |
| 004.3b - Model Loading/Caching | 8 | 7 | 4 | 19 |
| 004.3c - WASM Release/Testing | 3 | 4 | 5 | 12 |
| 004.4 - Rust LLM Actions | 9 | 6 | 2 | 17 |
| 004.5 - Python LLM Actions | 9 | 6 | 2 | 17 |
| 004.6 - Documentation | 0 | 1 | 3 | 4 |

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **AppImage size exceeds GitHub 2GB limit** | Medium | High | Phi-4-mini Q3_K_S variant (~1.9GB) fits within limit; Gemma variants (~5GB) require external hosting |
| **llama-cpp-2 ARM64 compilation failures** | Medium | High | Native ARM64 runner tests (004.1-INT-002/004, 004.2-INT-002/004) |
| **IndexedDB storage limits in Safari** | Medium | Medium | Safari 1GB limit test (004.3b-E2E-004), graceful fallback |
| **Model load failure at runtime** | Low | High | API fallback tests (004.4-UNIT-008, 004.5-UNIT-008) |
| **COOP/COEP header misconfiguration** | Medium | High | Header validation tests (004.3a-INT-001/002, 004.3c-UNIT-002) |
| **Regression in non-LLM builds** | Low | Critical | Feature isolation tests (004.1-UNIT-009, 004.2-UNIT-009, 004.4-INT-006, 004.5-INT-006) |

### Recommended Test Scenarios

**Critical Path (P0 - Must Execute):**

1. **Build Validation:**
   - All 8 AppImage variants build successfully (Gemma x86_64/ARM64, Phi-4-mini x86_64/ARM64 for both Rust and Python)
   - WASM package builds with wasm-pack
   - GitHub Actions release workflow triggers correctly

2. **Self-Contained Execution:**
   - AppImages run on clean Ubuntu 22.04/24.04 containers with no dependencies installed
   - WASM package loads in headless Chrome with COOP/COEP headers
   - Model auto-detection from $APPDIR/usr/share/models/ works

3. **LLM Actions:**
   - `llm.call`, `llm.chat`, `llm.embed` actions work with bundled models
   - Chat format templates (ChatML for Phi-4-mini, Gemma format for Gemma) apply correctly
   - Model context windows respected (128K for Phi-4-mini, 32K for Gemma)

4. **Regression Prevention:**
   - `cargo build` without `llm-local` feature succeeds
   - `pip install` without `[llm-local]` extras succeeds
   - Existing Prolog AppImages remain unchanged and functional

**Secondary Path (P1 - Should Execute):**

5. **Model Loading & Caching:**
   - IndexedDB caching verified (second page load uses cache)
   - Progress callbacks fire during model download
   - SHA256 checksum verification works

6. **Configuration Flexibility:**
   - TEA_MODEL_PATH environment variable override works
   - YAML settings model_path respected
   - API fallback triggers when local model unavailable

### Concerns & Blockers

| Type | Description | Severity | Status |
|------|-------------|----------|--------|
| **CONCERN** | Gemma ~5GB variants exceed GitHub Releases 2GB limit - requires external hosting strategy | Medium | Needs decision on hosting (e.g., HuggingFace, separate CDN) |
| **CONCERN** | Safari 1GB IndexedDB limit may prevent WASM model caching on iOS Safari | Low | Documented in troubleshooting guide, fallback to streaming |
| **CONCERN** | ARM64 GitHub Actions runners may have limited availability | Medium | Self-hosted runner or emulation fallback needed |
| **BLOCKER** | None identified at test design phase | - | - |

### Test Environment Requirements

| Environment | Purpose |
|-------------|---------|
| Ubuntu 22.04 container (clean) | AppImage E2E validation |
| Ubuntu 24.04 container (clean) | AppImage E2E validation |
| Fedora latest container | Cross-distro AppImage validation |
| Chrome headless + COOP/COEP | WASM browser testing |
| Firefox headless | WASM browser compatibility |
| Safari (manual) | IndexedDB limit validation |

### Quality Gate Recommendation

**Status:** PASS (with concerns noted)

The epic has comprehensive test coverage across all 8 stories. All acceptance criteria have mapped test scenarios. Risk mitigations are in place for identified concerns. The dual-model strategy (Gemma for quality, Phi-4-mini for size constraints) addresses the primary GitHub 2GB limit risk effectively.

**Pre-Release Checklist:**
- [ ] All P0 tests pass
- [ ] All P1 tests pass (or have documented waivers)
- [ ] External hosting solution confirmed for >2GB artifacts
- [ ] ARM64 build strategy confirmed
- [ ] Documentation reviewed and examples validated

---

## References

- [Gemma 3n on Ollama](https://ollama.com/library/gemma3n) - Official model page
- [Gemma 3n E4B GGUF on HuggingFace](https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF) - GGUF quantized versions
- [llama-cpp-2 Rust crate](https://crates.io/crates/llama-cpp-2)
- [llama-cpp-python](https://github.com/abetlen/llama-cpp-python)
- [wllama - llama.cpp in browser](https://github.com/ngxson/wllama)
- [TEA-WASM-001 Feasibility Report](../rust/wasm-feasibility.md)
