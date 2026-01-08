# Story TEA-RELEASE-004.1: Rust LLM AppImage with Bundled Gemma 3n E4B

## Status

Draft

## Story

**As a** developer using The Edge Agent,
**I want** a Rust AppImage with bundled Gemma 3n E4B model,
**So that** I can run LLM-powered workflows offline without installing llama.cpp or downloading models.

## Story Context

**Existing System Integration:**

- Integrates with: TEA-RELEASE-002 Rust AppImage build infrastructure
- Technology: Rust + llama-cpp-2 crate + linuxdeploy + GGUF
- Follows pattern: Existing AppImage build jobs in `.github/workflows/release.yaml`
- Touch points: `rust/Cargo.toml`, `rust/src/actions/`, `.github/workflows/release.yaml`

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: Release includes `tea-rust-llm-{version}-x86_64.AppImage` with bundled Gemma 3n E4B Q4_K_M model
2. **AC-2**: Release includes `tea-rust-llm-{version}-aarch64.AppImage` with bundled Gemma 3n E4B Q4_K_M model
3. **AC-3**: `llm.call` action routes to local llama-cpp-2 backend when model is present
4. **AC-4**: AppImage runs on systems without any LLM dependencies installed (fully self-contained)
5. **AC-5**: Model path auto-detected relative to AppImage extraction directory

### Build Requirements

6. **AC-6**: Add `llama-cpp-2` crate dependency with feature flags (`cuda`, `metal` optional)
7. **AC-7**: GitHub Actions job `build-rust-llm-appimage-x86_64` builds on `ubuntu-latest`
8. **AC-8**: GitHub Actions job `build-rust-llm-appimage-aarch64` builds on `ubuntu-24.04-arm`
9. **AC-9**: Model file downloaded from HuggingFace during build
10. **AC-10**: AppImage bundled with linuxdeploy including model file

### Quality Requirements

11. **AC-11**: Smoke test: `--version` and `--impl` pass
12. **AC-12**: Functional test: Run `examples/llm/local-chat.yaml` with local model
13. **AC-13**: Native Rust build without `llm-local` feature continues to work
14. **AC-14**: Existing Prolog AppImage (TEA-RELEASE-002) unchanged

## Tasks / Subtasks

- [ ] Task 1: Add llama-cpp-2 dependency to Rust crate (AC: 6, 13)
  - [ ] Add `llama-cpp-2` to Cargo.toml with `llm-local` feature flag
  - [ ] Configure CUDA/Metal as optional features
  - [ ] Verify native build compiles with and without feature
  - [ ] Run `cargo test` to confirm no regressions

- [ ] Task 2: Implement local LLM backend in Rust (AC: 3, 5)
  - [ ] Create `rust/src/actions/llm_local.rs` module
  - [ ] Implement `LlmBackend` trait for llama-cpp-2
  - [ ] Support `llm.call` action with local model
  - [ ] Auto-detect model path from `$APPDIR/usr/share/models/`
  - [ ] Fallback to `TEA_MODEL_PATH` environment variable

- [ ] Task 3: Add x86_64 LLM AppImage build job (AC: 1, 7, 9, 10)
  - [ ] Create `build-rust-llm-appimage-x86_64` job in release.yaml
  - [ ] Download `gemma-3n-E4B-it-Q4_K_M.gguf` from HuggingFace
  - [ ] Bundle model into AppDir at `usr/share/models/`
  - [ ] Use linuxdeploy to create AppImage
  - [ ] Add custom AppRun setting `TEA_MODEL_PATH`

- [ ] Task 4: Add ARM64 LLM AppImage build job (AC: 2, 8, 9, 10)
  - [ ] Create `build-rust-llm-appimage-aarch64` job in release.yaml
  - [ ] Use native ARM64 runner for glibc compatibility
  - [ ] Same model bundling approach as x86_64

- [ ] Task 5: Add smoke and functional tests (AC: 11, 12)
  - [ ] Add `--version` and `--impl` smoke tests in workflow
  - [ ] Create `examples/llm/local-chat.yaml` example
  - [ ] Run example in workflow using bundled model
  - [ ] Test AppImage on clean Ubuntu container

- [ ] Task 6: Update release job artifacts (AC: 14)
  - [ ] Add LLM AppImages to artifact collection
  - [ ] Update SHA256SUMS generation
  - [ ] Ensure existing Prolog AppImages still build

## Dev Notes

### llama-cpp-2 Cargo Configuration

```toml
[dependencies.llama-cpp-2]
version = "0.1"
optional = true

[features]
llm-local = ["llama-cpp-2"]
llm-local-cuda = ["llm-local", "llama-cpp-2/cuda"]
llm-local-metal = ["llm-local", "llama-cpp-2/metal"]
```

### Model Download in CI

```yaml
- name: Download Gemma 3n E4B model
  run: |
    mkdir -p models
    wget -q https://huggingface.co/ggml-org/gemma-3n-E4B-it-GGUF/resolve/main/gemma-3n-E4B-it-Q4_K_M.gguf \
      -O models/gemma-3n-E4B-it-Q4_K_M.gguf
```

### AppDir Structure

```
tea-rust-llm.AppDir/
├── AppRun (custom script)
├── tea.desktop
├── usr/
│   ├── bin/
│   │   └── tea (the binary with llm-local feature)
│   ├── lib/
│   │   ├── libswipl.so.9 (if Prolog also included)
│   │   └── ... (dependencies)
│   └── share/
│       └── models/
│           └── gemma-3n-E4B-it-Q4_K_M.gguf (~4.5GB)
```

### Custom AppRun Script

```bash
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
export TEA_MODEL_PATH="${HERE}/usr/share/models/gemma-3n-E4B-it-Q4_K_M.gguf"
export SWI_HOME_DIR="${HERE}/usr/lib/swipl"  # If Prolog included
exec "${HERE}/usr/bin/tea" "$@"
```

### Relevant Source Files

```
rust/
├── Cargo.toml              # Add llama-cpp-2 dependency
├── src/
│   ├── actions/
│   │   ├── mod.rs          # Register llm_local action
│   │   └── llm_local.rs    # NEW: Local LLM backend
│   └── engine/
│       └── yaml.rs         # Action routing logic
```

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | In workflow | `--version` and `--impl --features` |
| Functional test | In workflow | Run local-chat.yaml with bundled model |
| Container test | In workflow | Run on clean Ubuntu container |

### GitHub Release Size Constraint

GitHub Releases has a **2GB per file limit**. Since the AppImage will be ~5GB:

**Options:**
1. Use external hosting (S3, GCS, R2) with GitHub Release linking
2. Split model and AppImage into separate files
3. Use GitHub LFS (Large File Storage)

**Recommended:** External hosting with download script in release notes.

## Definition of Done

- [ ] Rust crate builds with `--features llm-local`
- [ ] `llm.call` action works with local llama-cpp-2 backend
- [ ] x86_64 LLM AppImage builds successfully
- [ ] aarch64 LLM AppImage builds successfully
- [ ] Smoke tests pass for both architectures
- [ ] Functional test runs local-chat.yaml successfully
- [ ] Existing Prolog AppImages unchanged
- [ ] Model auto-detected from AppImage bundle

## Risk and Compatibility Check

**Primary Risk:** llama-cpp-2 compilation issues on ARM64

**Mitigation:** Use native ARM64 runner, test compilation early

**Rollback:** Feature-flagged, can be excluded from release

## Compatibility Verification

- [x] No breaking changes to existing APIs (additive feature)
- [x] Database changes: None
- [x] UI changes: None
- [x] Performance impact: Large file download in CI, acceptable

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-08 | 0.1 | Initial story creation | Sarah (PO Agent) |
