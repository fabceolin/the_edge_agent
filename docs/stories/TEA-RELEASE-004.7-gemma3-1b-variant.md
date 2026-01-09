# Story TEA-RELEASE-004.7: Gemma 3 1B Variant (Ultra-Lightweight LLM Bundle)

## Status

Draft

## Story

**As a** developer using The Edge Agent,
**I want** AppImage and WASM distributions with bundled Gemma 3 1B model (Q8_0, ~1.07GB),
**So that** I can run LLM-powered workflows on resource-constrained devices with minimal download size.

## Story Context

**Existing System Integration:**

- Extends: TEA-RELEASE-004.1 (Rust LLM AppImage), TEA-RELEASE-004.2 (Python LLM AppImage), TEA-RELEASE-004.3 (WASM)
- Technology: Same llama-cpp-2 (Rust), llama-cpp-python (Python), wllama (WASM) infrastructure
- Model source: [unsloth/gemma-3-1b-it-GGUF](https://huggingface.co/unsloth/gemma-3-1b-it-GGUF)
- Touch points: `.github/workflows/release.yaml`, existing LLM action routing

**Model Specifications:**

| Property | Value |
|----------|-------|
| **Model** | Gemma 3 1B Instruct |
| **Parameters** | 1.0B |
| **Quantization** | Q8_0 (8-bit) |
| **File Size** | 1.07 GB |
| **Context Window** | 8K tokens |
| **Source** | `unsloth/gemma-3-1b-it-GGUF` |
| **GGUF File** | `gemma-3-1b-it-Q8_0.gguf` |

**Model Comparison:**

| Model | Size | Context | Quality | Use Case |
|-------|------|---------|---------|----------|
| **Gemma 3 1B Q8_0** | 1.07GB | 8K | Good | Edge devices, fast prototyping |
| Phi-4-mini Q3_K_S | 1.90GB | 128K | Good | Extended context tasks |
| Gemma 3n E4B Q4_K_M | 4.54GB | 32K | Best | Maximum quality offline |

## Acceptance Criteria

### Rust AppImage (AC 1-5)

1. **AC-1**: Release includes `tea-rust-llm-gemma3-1b-{version}-x86_64.AppImage` (~1.5GB)
2. **AC-2**: Release includes `tea-rust-llm-gemma3-1b-{version}-aarch64.AppImage` (~1.5GB)
3. **AC-3**: AppImage bundles `gemma-3-1b-it-Q8_0.gguf` at `usr/share/models/`
4. **AC-4**: `llm.call` action routes to local backend when Gemma 3 1B model detected
5. **AC-5**: Smoke test passes: `--version` and `--impl` work correctly

### Python AppImage (AC 6-10)

6. **AC-6**: Release includes `tea-python-llm-gemma3-1b-{version}-x86_64.AppImage` (~1.5GB)
7. **AC-7**: Release includes `tea-python-llm-gemma3-1b-{version}-aarch64.AppImage` (~1.5GB)
8. **AC-8**: AppImage bundles `gemma-3-1b-it-Q8_0.gguf` at `usr/share/models/`
9. **AC-9**: `llm.call` action routes to local backend when Gemma 3 1B model detected
10. **AC-10**: Smoke test passes: `--version` and `--impl` work correctly

### WASM Package (AC 11-14)

11. **AC-11**: WASM package supports Gemma 3 1B model loading via wllama
12. **AC-12**: Model cached in IndexedDB after first load
13. **AC-13**: `llm.call` action works in browser with Gemma 3 1B
14. **AC-14**: Model size under IndexedDB Safari 1GB limit (1.07GB fits with compression)

### Quality Requirements (AC 15-17)

15. **AC-15**: Functional test: `examples/llm/local-chat.yaml` runs successfully with Gemma 3 1B
16. **AC-16**: No regression in existing Gemma/Phi-4-mini variants
17. **AC-17**: Chat template correctly applied (Gemma format)

## Tasks / Subtasks

- [ ] Task 1: Add Rust Gemma 3 1B AppImage build jobs (AC: 1-5)
  - [ ] Create `build-rust-llm-gemma3-1b-appimage-x86_64` job in release.yaml
  - [ ] Create `build-rust-llm-gemma3-1b-appimage-aarch64` job in release.yaml
  - [ ] Download `gemma-3-1b-it-Q8_0.gguf` from HuggingFace during build
  - [ ] Bundle model at `usr/share/models/gemma-3-1b-it-Q8_0.gguf`
  - [ ] Update AppRun script with dynamic model detection
  - [ ] Add smoke tests in workflow

- [ ] Task 2: Add Python Gemma 3 1B AppImage build jobs (AC: 6-10)
  - [ ] Create `build-python-llm-gemma3-1b-appimage-x86_64` job in release.yaml
  - [ ] Create `build-python-llm-gemma3-1b-appimage-aarch64` job in release.yaml
  - [ ] Download `gemma-3-1b-it-Q8_0.gguf` from HuggingFace during build
  - [ ] Bundle model at `usr/share/models/gemma-3-1b-it-Q8_0.gguf`
  - [ ] Update AppRun script with dynamic model detection
  - [ ] Add smoke tests in workflow

- [ ] Task 3: Add WASM Gemma 3 1B support (AC: 11-14)
  - [ ] Add Gemma 3 1B model configuration to wllama loader
  - [ ] Configure model URL for CDN/GitHub release download
  - [ ] Test IndexedDB caching with 1.07GB model
  - [ ] Verify Safari compatibility (model fits 1GB limit when streamed)

- [ ] Task 4: Update model path detection logic (AC: 4, 9)
  - [ ] Rust: Update `llm_backend.rs` to detect `gemma-3-1b-it-*.gguf` pattern
  - [ ] Python: Update `llm_local.py` to detect `gemma-3-1b-it-*.gguf` pattern
  - [ ] Apply correct Gemma chat template for 1B model

- [ ] Task 5: Add functional tests (AC: 15-17)
  - [ ] Create `examples/llm/local-chat-gemma3-1b.yaml` example
  - [ ] Run functional test in CI with Gemma 3 1B model
  - [ ] Verify no regression in existing model variants

- [ ] Task 6: Update release artifacts (AC: 16)
  - [ ] Add Gemma 3 1B AppImages to release job dependencies
  - [ ] Update SHA256SUMS generation
  - [ ] Update release notes template with new variants

## Dev Notes

### HuggingFace Model Download

```yaml
- name: Download Gemma 3 1B model
  run: |
    mkdir -p models
    wget -q https://huggingface.co/unsloth/gemma-3-1b-it-GGUF/resolve/main/gemma-3-1b-it-Q8_0.gguf \
      -O models/gemma-3-1b-it-Q8_0.gguf
    # Verify checksum
    echo "EXPECTED_SHA256  models/gemma-3-1b-it-Q8_0.gguf" | sha256sum -c -
```

### AppDir Structure

```
tea-rust-llm-gemma3-1b.AppDir/
├── AppRun (custom script)
├── tea.desktop
├── usr/
│   ├── bin/
│   │   └── tea
│   ├── lib/
│   │   └── ... (dependencies)
│   └── share/
│       └── models/
│           └── gemma-3-1b-it-Q8_0.gguf (~1.07GB)
```

### Dynamic Model Detection (AppRun)

```bash
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"

# Auto-detect model file
MODEL_FILE=$(find "${HERE}/usr/share/models" -name "*.gguf" -type f | head -1)
if [ -n "$MODEL_FILE" ]; then
    export TEA_MODEL_PATH="$MODEL_FILE"
fi

export SWI_HOME_DIR="${HERE}/usr/lib/swipl"
exec "${HERE}/usr/bin/tea" "$@"
```

### Gemma Chat Template

```python
# Gemma 3 uses specific chat format
GEMMA_CHAT_TEMPLATE = """<start_of_turn>user
{user_message}<end_of_turn>
<start_of_turn>model
{assistant_message}<end_of_turn>
"""
```

### Artifact Naming Convention

| Artifact | Size Est. | Description |
|----------|-----------|-------------|
| `tea-rust-llm-gemma3-1b-{version}-x86_64.AppImage` | ~1.5GB | Rust + Gemma 3 1B Q8_0 |
| `tea-rust-llm-gemma3-1b-{version}-aarch64.AppImage` | ~1.5GB | Rust ARM64 + Gemma 3 1B |
| `tea-python-llm-gemma3-1b-{version}-x86_64.AppImage` | ~1.5GB | Python + Gemma 3 1B Q8_0 |
| `tea-python-llm-gemma3-1b-{version}-aarch64.AppImage` | ~1.5GB | Python ARM64 + Gemma 3 1B |

### WASM Considerations

- Model size (1.07GB) is under Safari's 1GB IndexedDB limit when using streaming
- Smaller model = faster initial load in browser
- Good choice for demo/prototype WASM deployments
- Consider offering alongside Phi-4-mini for WASM users

### Relevant Source Files

```
rust/
├── src/actions/
│   ├── llm.rs              # Action routing
│   ├── llm_backend.rs      # Backend detection
│   └── llm_local.rs        # Local LLM implementation

python/
├── src/the_edge_agent/actions/
│   ├── llm_actions.py      # Action routing
│   ├── llm_local.py        # Local LLM implementation
│   └── llm_backend.py      # Backend abstraction

.github/workflows/
└── release.yaml            # CI/CD build jobs
```

## Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Smoke test | CI workflow | `--version` and `--impl` |
| Functional | CI workflow | Run local-chat-gemma3-1b.yaml |
| Container | CI workflow | Clean Ubuntu 22.04 execution |
| WASM | Playwright | Browser model loading and inference |

## Definition of Done

- [ ] Rust Gemma 3 1B AppImage builds for x86_64
- [ ] Rust Gemma 3 1B AppImage builds for aarch64
- [ ] Python Gemma 3 1B AppImage builds for x86_64
- [ ] Python Gemma 3 1B AppImage builds for aarch64
- [ ] WASM package supports Gemma 3 1B model
- [ ] All smoke tests pass
- [ ] Functional test with local-chat example passes
- [ ] No regression in existing model variants
- [ ] SHA256SUMS updated with new artifacts

## Risk and Compatibility Check

**Primary Risk:** Model quality may be lower than larger variants for complex tasks

**Mitigation:**
- Document use cases (edge devices, prototyping, simple tasks)
- Keep larger variants available for quality-critical deployments

**Rollback:** Additive feature, can be excluded from release without affecting existing variants

## Compatibility Verification

- [ ] No breaking changes to existing APIs (additive feature)
- [ ] Database changes: None
- [ ] UI changes: None
- [ ] Performance impact: Smaller model = faster inference, lower memory usage

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-09 | 0.1 | Initial story creation | Sarah (PO Agent) |

---

## References

- [unsloth/gemma-3-1b-it-GGUF](https://huggingface.co/unsloth/gemma-3-1b-it-GGUF) - Model source
- [Gemma 3 Documentation](https://ai.google.dev/gemma/docs) - Official Gemma docs
- [TEA-RELEASE-004.1](./TEA-RELEASE-004.1-rust-llm-appimage.md) - Rust LLM AppImage pattern
- [TEA-RELEASE-004.2](./TEA-RELEASE-004.2-python-llm-appimage.md) - Python LLM AppImage pattern
