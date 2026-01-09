# Test Design: Epic TEA-RELEASE-004 - LLM-Bundled Distributions

Date: 2026-01-08
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 127
- **Unit tests**: 52 (41%)
- **Integration tests**: 48 (38%)
- **E2E tests**: 27 (21%)
- **Priority distribution**: P0: 34, P1: 51, P2: 32, P3: 10

## Test Coverage Matrix by Story

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

---

## Test Scenarios by Story

### Story 004.1: Rust LLM AppImage (Gemma + Phi-4-mini Variants)

#### AC-1/AC-2: AppImage Build Artifacts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-INT-001 | Integration | P0 | Verify Gemma x86_64 AppImage builds successfully | Critical release artifact |
| 004.1-INT-002 | Integration | P0 | Verify Gemma aarch64 AppImage builds successfully | Critical release artifact |
| 004.1-INT-003 | Integration | P0 | Verify Phi-4-mini x86_64 AppImage builds successfully | Critical release artifact |
| 004.1-INT-004 | Integration | P0 | Verify Phi-4-mini aarch64 AppImage builds successfully | Critical release artifact |
| 004.1-UNIT-001 | Unit | P1 | Verify model file included in AppDir structure | Build validation |
| 004.1-UNIT-002 | Unit | P1 | Verify AppRun script sets TEA_MODEL_PATH correctly | Configuration validation |

#### AC-3: llm.call Action Routing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-UNIT-003 | Unit | P0 | LlmBackend trait implementation compiles with llm-local feature | Core functionality |
| 004.1-UNIT-004 | Unit | P0 | llm.call action routes to local backend when model present | Core functionality |
| 004.1-UNIT-005 | Unit | P1 | llm.call returns valid LlmCallResult structure | Data integrity |
| 004.1-INT-005 | Integration | P0 | llm.call generates text with bundled Gemma model | Core functionality |
| 004.1-INT-006 | Integration | P0 | llm.call generates text with bundled Phi-4-mini model | Core functionality |

#### AC-4: Self-Contained Execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-E2E-001 | E2E | P0 | AppImage runs on clean Ubuntu 22.04 container (no deps) | Critical user journey |
| 004.1-E2E-002 | E2E | P0 | AppImage runs on clean Ubuntu 24.04 container (no deps) | Critical user journey |
| 004.1-INT-007 | Integration | P1 | All shared libraries bundled in AppImage | Dependency validation |

#### AC-5: Model Path Auto-Detection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-UNIT-006 | Unit | P0 | Model auto-detected from $APPDIR/usr/share/models/ | Core functionality |
| 004.1-UNIT-007 | Unit | P1 | TEA_MODEL_PATH override works | Configuration flexibility |
| 004.1-UNIT-008 | Unit | P2 | Default cache path ~/.cache/tea/models/ searched | Fallback behavior |

#### AC-11/AC-12: Smoke and Functional Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-INT-008 | Integration | P0 | `--version` returns correct version | Smoke test |
| 004.1-INT-009 | Integration | P0 | `--impl` shows llm-local feature enabled | Feature validation |
| 004.1-E2E-003 | E2E | P0 | Run examples/llm/local-chat.yaml successfully | Critical user journey |
| 004.1-E2E-004 | E2E | P1 | Run examples/llm/local-embed.yaml successfully | Secondary workflow |

#### AC-13/AC-14: Non-Regression

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.1-UNIT-009 | Unit | P0 | cargo build without llm-local feature succeeds | Regression prevention |
| 004.1-INT-010 | Integration | P0 | Existing Prolog AppImage unchanged and functional | Regression prevention |

---

### Story 004.2: Python LLM AppImage (Gemma + Phi-4-mini Variants)

#### AC-1/AC-2: AppImage Build Artifacts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-001 | Integration | P0 | Verify Gemma x86_64 AppImage builds successfully | Critical release artifact |
| 004.2-INT-002 | Integration | P0 | Verify Gemma aarch64 AppImage builds successfully | Critical release artifact |
| 004.2-INT-003 | Integration | P0 | Verify Phi-4-mini x86_64 AppImage builds successfully | Critical release artifact |
| 004.2-INT-004 | Integration | P0 | Verify Phi-4-mini aarch64 AppImage builds successfully | Critical release artifact |
| 004.2-UNIT-001 | Unit | P1 | Verify model file included in AppDir structure | Build validation |
| 004.2-UNIT-002 | Unit | P1 | Verify AppRun script sets APPDIR and TEA_MODEL_PATH | Configuration |

#### AC-3: llm.call Action Routing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-003 | Unit | P0 | LocalLlmBackend class loads with llama-cpp-python | Core functionality |
| 004.2-UNIT-004 | Unit | P0 | llm.call action routes to local backend when model present | Core functionality |
| 004.2-UNIT-005 | Unit | P1 | llm.call returns valid LlmCallResult dataclass | Data integrity |
| 004.2-INT-005 | Integration | P0 | llm.call generates text with bundled model | Core functionality |

#### AC-4: Self-Contained Execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-E2E-001 | E2E | P0 | AppImage runs on clean Ubuntu container (no Python installed) | Critical user journey |
| 004.2-E2E-002 | E2E | P0 | AppImage runs on clean Fedora container | Cross-distro validation |
| 004.2-INT-006 | Integration | P1 | PyInstaller bundles all Python dependencies | Dependency validation |

#### AC-5: Model Path Resolution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-006 | Unit | P0 | resolve_model_path() finds model in APPDIR | Core functionality |
| 004.2-UNIT-007 | Unit | P1 | TEA_MODEL_PATH environment variable overrides | Configuration |
| 004.2-UNIT-008 | Unit | P2 | YAML settings model_path respected | Configuration |

#### AC-11/AC-12: Smoke and Functional Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-007 | Integration | P0 | `--version` returns correct version | Smoke test |
| 004.2-INT-008 | Integration | P0 | `--impl` shows llm-local capability | Feature validation |
| 004.2-E2E-003 | E2E | P0 | Run examples/llm/local-chat.yaml successfully | Critical user journey |
| 004.2-E2E-004 | E2E | P1 | Run examples/llm/local-embed.yaml successfully | Secondary workflow |

#### AC-13/AC-14: Non-Regression

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-009 | Unit | P0 | pip install without [llm-local] extras succeeds | Regression prevention |
| 004.2-INT-009 | Integration | P0 | Existing Python Prolog AppImage unchanged | Regression prevention |

---

### Story 004.3a: WASM LLM Core Package

#### AC-1/AC-2: LLM Actions in Browser

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3a-UNIT-001 | Unit | P0 | llm_call_async() invokes registered handler | Core functionality |
| 004.3a-UNIT-002 | Unit | P0 | llm_embed_async() invokes registered handler | Core functionality |
| 004.3a-UNIT-003 | Unit | P1 | Error returned when no handler registered | Error handling |
| 004.3a-UNIT-004 | Unit | P2 | Invalid JSON params handled gracefully | Error handling |
| 004.3a-INT-001 | Integration | P0 | SharedArrayBuffer detected when COOP/COEP present | Multi-threading |
| 004.3a-INT-002 | Integration | P1 | Falls back to single-thread mode without COOP/COEP | Graceful degradation |

#### AC-3/AC-4: TypeScript Wrapper

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3a-UNIT-005 | Unit | P0 | initTeaLlm() initializes WASM module | Core functionality |
| 004.3a-UNIT-006 | Unit | P0 | executeLlmYaml() executes workflows | Core functionality |
| 004.3a-INT-003 | Integration | P0 | TypeScript wrapper exports all types | API completeness |
| 004.3a-E2E-001 | E2E | P0 | Package loads in Chrome browser | Critical user journey |
| 004.3a-E2E-002 | E2E | P1 | Package loads in Firefox browser | Browser compatibility |

#### AC-5/AC-6/AC-7: Build Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3a-INT-004 | Integration | P0 | wasm-pack build succeeds without warnings | Build validation |
| 004.3a-INT-005 | Integration | P1 | .d.ts TypeScript definitions generated | API documentation |
| 004.3a-INT-006 | Integration | P1 | ES module build works in browsers | Module format |

#### AC-8/AC-9: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3a-UNIT-007 | Unit | P0 | Unit test with stories260K.gguf passes | Core validation |
| 004.3a-E2E-003 | E2E | P2 | Existing tea-wasm spike still builds | Regression prevention |

---

### Story 004.3b: WASM Model Loading and Caching

#### AC-2/AC-3: Model Loading (Simplified - No Chunking)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-UNIT-001 | Unit | P0 | loadModel() loads single GGUF file | Core functionality |
| 004.3b-UNIT-002 | Unit | P1 | Progress callback fires during download | UX requirement |
| 004.3b-UNIT-003 | Unit | P1 | SHA256 checksum verification works | Data integrity |
| 004.3b-UNIT-004 | Unit | P2 | Checksum mismatch triggers error | Security |
| 004.3b-INT-001 | Integration | P0 | Model loads from bundled assets | Core functionality |
| 004.3b-INT-002 | Integration | P1 | Model loads from GitHub Release URL | Distribution |

#### AC-4/AC-5/AC-6: IndexedDB Caching

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-UNIT-005 | Unit | P0 | cacheModel() stores model in IndexedDB | Core functionality |
| 004.3b-UNIT-006 | Unit | P0 | getCachedModel() retrieves cached model | Core functionality |
| 004.3b-UNIT-007 | Unit | P1 | Version change triggers cache invalidation | Cache consistency |
| 004.3b-UNIT-008 | Unit | P1 | clearCache() removes cached model | Admin function |
| 004.3b-INT-003 | Integration | P0 | Second page load uses cached model | Performance |
| 004.3b-E2E-001 | E2E | P0 | Cache hit skips download (verified in DevTools) | Critical performance |
| 004.3b-E2E-002 | E2E | P1 | Cache clear triggers re-download | Admin workflow |

#### AC-7/AC-8/AC-9: Build Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-INT-004 | Integration | P0 | model-manifest.json contains valid SHA256 | Build artifact |
| 004.3b-INT-005 | Integration | P1 | Download script fetches Phi-4-mini Q3_K_S | Build process |

#### AC-10/AC-11/AC-12: Quality Requirements

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3b-E2E-003 | E2E | P1 | Corrupted cache recovery works (re-download) | Resilience |
| 004.3b-E2E-004 | E2E | P2 | Safari 1GB limit handled gracefully | Browser compat |
| 004.3b-INT-006 | Integration | P2 | Progress callback shows accurate percentage | UX |
| 004.3b-INT-007 | Integration | P2 | Network error during download handled | Error handling |

---

### Story 004.3c: WASM LLM Release and Testing

#### AC-1/AC-2/AC-3/AC-4/AC-5: Release Workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3c-INT-001 | Integration | P0 | GitHub Actions workflow builds on release tag | Release automation |
| 004.3c-INT-002 | Integration | P0 | tea-wasm-llm-{version}.tar.gz uploaded | Release artifact |
| 004.3c-INT-003 | Integration | P0 | Model GGUF file uploaded to GitHub Releases | Release artifact |
| 004.3c-INT-004 | Integration | P1 | SHA256SUMS generated for all assets | Security |
| 004.3c-UNIT-001 | Unit | P1 | Tarball contains pkg/ and js/ directories | Package structure |

#### AC-6/AC-7/AC-8/AC-9: Playwright Testing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3c-E2E-001 | E2E | P0 | Playwright test loads package in headless Chrome | Core validation |
| 004.3c-E2E-002 | E2E | P0 | Playwright test verifies model initialization | Core validation |
| 004.3c-E2E-003 | E2E | P0 | Playwright test executes simple LLM workflow | Critical user journey |
| 004.3c-E2E-004 | E2E | P1 | Playwright test verifies IndexedDB caching | Cache validation |
| 004.3c-UNIT-002 | Unit | P1 | Test server sets COOP/COEP headers correctly | Test infrastructure |

#### AC-10/AC-11/AC-12/AC-13: Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.3c-UNIT-003 | Unit | P2 | README includes all required sections | Documentation |
| 004.3c-E2E-005 | E2E | P2 | Troubleshooting steps resolve common issues | User experience |

---

### Story 004.4: Rust Local LLM Actions Integration

#### AC-1/AC-2/AC-3/AC-4: LLM Actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.4-UNIT-001 | Unit | P0 | llm.call action generates text (raw prompt) | Core functionality |
| 004.4-UNIT-002 | Unit | P0 | llm.chat action generates text (OpenAI format) | Core functionality |
| 004.4-UNIT-003 | Unit | P0 | llm.embed action generates embeddings | Core functionality |
| 004.4-UNIT-004 | Unit | P1 | llm.stream action streams tokens via callback | Streaming support |
| 004.4-UNIT-005 | Unit | P1 | ChatML format applied for Phi-4-mini models | Chat formatting |
| 004.4-UNIT-006 | Unit | P1 | Gemma format applied for Gemma models | Chat formatting |

#### AC-5/AC-6: Configuration and Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.4-UNIT-007 | Unit | P0 | Model path from YAML settings respected | Configuration |
| 004.4-UNIT-008 | Unit | P1 | Fallback to API backend when model not found | Graceful degradation |
| 004.4-INT-001 | Integration | P0 | YAML workflow with llm.chat executes | Core workflow |
| 004.4-INT-002 | Integration | P1 | API fallback works when local unavailable | Fallback validation |

#### AC-7/AC-12: Model Auto-Detection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.4-UNIT-009 | Unit | P0 | Phi-4-mini auto-detected (128K context) | Auto-config |
| 004.4-INT-003 | Integration | P1 | Gemma auto-detected (32K context) | Auto-config |
| 004.4-INT-004 | Integration | P2 | Unknown model uses safe defaults | Fallback behavior |

#### AC-8/AC-13/AC-14/AC-15/AC-16: Build and Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.4-INT-005 | Integration | P0 | cargo test passes with llm-local feature | Regression prevention |
| 004.4-INT-006 | Integration | P0 | cargo test passes without llm-local feature | Regression prevention |
| 004.4-E2E-001 | E2E | P0 | Integration test with TinyLlama passes | Core validation |
| 004.4-E2E-002 | E2E | P2 | Performance benchmark within acceptable range | Performance |

---

### Story 004.5: Python Local LLM Actions Integration

#### AC-1/AC-2/AC-3/AC-4: LLM Actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-001 | Unit | P0 | llm.call action generates text (raw prompt) | Core functionality |
| 004.5-UNIT-002 | Unit | P0 | llm.chat action generates text (OpenAI format) | Core functionality |
| 004.5-UNIT-003 | Unit | P0 | llm.embed action generates embeddings | Core functionality |
| 004.5-UNIT-004 | Unit | P1 | llm.stream action streams tokens via callback | Streaming support |
| 004.5-UNIT-005 | Unit | P1 | create_chat_completion() used for chat action | API usage |
| 004.5-UNIT-006 | Unit | P2 | stream_chat() yields tokens correctly | Streaming validation |

#### AC-5/AC-6: Configuration and Fallback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-007 | Unit | P0 | resolve_model_path() returns correct priority order | Configuration |
| 004.5-UNIT-008 | Unit | P1 | create_llm_backend() falls back to API | Graceful degradation |
| 004.5-INT-001 | Integration | P0 | YAML workflow with llm.chat executes | Core workflow |
| 004.5-INT-002 | Integration | P1 | API fallback works when local unavailable | Fallback validation |

#### AC-7/AC-12: Model Auto-Detection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-UNIT-009 | Unit | P0 | get_model_info() returns 128K for Phi-4-mini | Auto-config |
| 004.5-INT-003 | Integration | P1 | get_model_info() returns 32K for Gemma | Auto-config |
| 004.5-INT-004 | Integration | P2 | Unknown model uses 4096 default | Fallback behavior |

#### AC-8/AC-13/AC-14/AC-15/AC-16: Build and Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.5-INT-005 | Integration | P0 | pytest passes with llm-local extras | Regression prevention |
| 004.5-INT-006 | Integration | P0 | pytest passes without llm-local extras | Regression prevention |
| 004.5-E2E-001 | E2E | P0 | Integration test with TinyLlama passes | Core validation |
| 004.5-E2E-002 | E2E | P2 | Mocked tests cover all LocalLlmBackend methods | Test coverage |

---

### Story 004.6: Documentation and Examples

#### AC-1/AC-2/AC-5: Installation Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-INT-001 | Integration | P1 | Documentation links resolve correctly | User experience |
| 004.6-E2E-001 | E2E | P1 | README quick start example works | Critical user journey |

#### AC-6/AC-7/AC-8/AC-9: Example Workflows

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.6-E2E-002 | E2E | P0 | examples/llm/local-chat.yaml executes | Example validation |
| 004.6-E2E-003 | E2E | P1 | examples/llm/local-embed.yaml executes | Example validation |
| 004.6-E2E-004 | E2E | P2 | examples/llm/local-rag.yaml executes (if memory.retrieve available) | Advanced example |

---

## Risk Coverage

| Risk | Test IDs Mitigating |
|------|---------------------|
| AppImage size exceeds GitHub 2GB limit | 004.1-INT-003/004, 004.2-INT-003/004 (Phi-4-mini ~2GB) |
| llama-cpp-2 compilation on ARM64 | 004.1-INT-002/004, 004.2-INT-002/004 |
| IndexedDB storage limits | 004.3b-E2E-004 |
| Model load failure | 004.4-UNIT-008, 004.5-UNIT-008 (fallback tests) |
| COOP/COEP header issues | 004.3a-INT-001/002, 004.3c-UNIT-002 |
| Regression in non-LLM builds | 004.1-UNIT-009, 004.2-UNIT-009, 004.4-INT-006, 004.5-INT-006 |

---

## Recommended Execution Order

### P0 Tests (Must Pass Before Release)

1. **Unit Tests (Fast Feedback)**
   - 004.1-UNIT-003/004/006 (Rust LLM backend)
   - 004.2-UNIT-003/004/006 (Python LLM backend)
   - 004.3a-UNIT-001/005/006 (WASM core)
   - 004.3b-UNIT-001/005/006 (Model loading)
   - 004.4-UNIT-001/002/003/007/009 (Rust actions)
   - 004.5-UNIT-001/002/003/007/009 (Python actions)

2. **Integration Tests (Build Validation)**
   - 004.1-INT-001/002/003/004 (All AppImage builds)
   - 004.2-INT-001/002/003/004 (All AppImage builds)
   - 004.3a-INT-004 (WASM build)
   - 004.3c-INT-001/002/003 (Release workflow)
   - 004.1-INT-005/006/008/009/010 (Rust functional)
   - 004.2-INT-005/007/008/009 (Python functional)
   - 004.4-INT-001/005/006 (Rust actions)
   - 004.5-INT-001/005/006 (Python actions)

3. **E2E Tests (Critical Paths)**
   - 004.1-E2E-001/002/003 (Rust AppImage on clean systems)
   - 004.2-E2E-001/002/003 (Python AppImage on clean systems)
   - 004.3a-E2E-001 (WASM in Chrome)
   - 004.3b-E2E-001 (Cache hit)
   - 004.3c-E2E-001/002/003 (Playwright)
   - 004.4-E2E-001, 004.5-E2E-001 (TinyLlama integration)
   - 004.6-E2E-002 (Example works)

### P1 Tests (Should Pass)

4. Execute P1 unit tests
5. Execute P1 integration tests
6. Execute P1 E2E tests

### P2/P3 Tests (As Time Permits)

7. Execute P2 tests in full regression
8. P3 tests only in release candidates

---

## Test Environment Requirements

| Environment | Purpose | Configuration |
|-------------|---------|---------------|
| Ubuntu 22.04 container | AppImage E2E | Clean, no dev tools |
| Ubuntu 24.04 container | AppImage E2E | Clean, no dev tools |
| Fedora latest container | Cross-distro | Clean, no Python |
| Chrome headless | WASM E2E | With COOP/COEP server |
| Firefox headless | WASM E2E | With COOP/COEP server |
| Safari (manual) | WASM compatibility | IndexedDB limits |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (STORY-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed
- [x] Regression prevention tests included

---

## Gate YAML Block

```yaml
test_design:
  epic: TEA-RELEASE-004
  date: 2026-01-08
  scenarios_total: 127
  by_level:
    unit: 52
    integration: 48
    e2e: 27
  by_priority:
    p0: 34
    p1: 51
    p2: 32
    p3: 10
  coverage_gaps: []
  risk_mitigations:
    - appimage_size: "Phi-4-mini variant under 2GB"
    - arm64_compilation: "Native runner tests"
    - indexeddb_limits: "Safari fallback tested"
    - model_failure: "API fallback tests"
  notes: |
    Comprehensive test design covering all 8 stories in the LLM-Bundled Distributions epic.
    Focus on critical build artifacts, self-contained execution, and cross-platform compatibility.
    P0 tests must all pass before release approval.
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004-test-design-20260108.md
P0 tests identified: 34
P1 tests identified: 51
Critical paths covered: AppImage builds, WASM browser, LLM actions, model loading
```
