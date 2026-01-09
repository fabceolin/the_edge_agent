# Test Design: Story TEA-RELEASE-004.2

**Story:** Python LLM AppImage (Gemma + Phi-4-mini Variants)
**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 14 (33%)
- **Integration tests:** 16 (38%)
- **E2E tests:** 12 (29%)
- **Priority distribution:** P0: 18, P1: 15, P2: 7, P3: 2

### Risk-Adjusted Focus Areas

| Area | Risk Level | Test Focus |
|------|------------|------------|
| Model Path Resolution | High | Comprehensive path fallback testing |
| llama-cpp-python Integration | High | Backend initialization, inference |
| AppImage Self-Containment | Critical | Clean environment testing |
| Graceful Import Handling | Medium | Fallback behavior when lib missing |
| CI/CD Build Pipeline | High | Cross-arch compilation, model bundling |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Release includes Gemma variants

**Requirement:** `tea-python-llm-gemma-{version}-{arch}.AppImage` (~5GB, best quality)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-E2E-001 | E2E | P0 | Verify Gemma x86_64 AppImage exists in release artifacts | Critical release deliverable |
| 004.2-E2E-002 | E2E | P0 | Verify Gemma aarch64 AppImage exists in release artifacts | Critical release deliverable |
| 004.2-INT-001 | Integration | P1 | Verify Gemma AppImage size is within expected range (4.5-5.5GB) | Size validation for user expectations |
| 004.2-INT-002 | Integration | P1 | Verify Gemma model file bundled at `usr/share/models/` | Correct bundling structure |

---

### AC-2: Release includes Phi-4-mini variants

**Requirement:** `tea-python-llm-phi4-{version}-{arch}.AppImage` (~2.5GB, 128K context)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-E2E-003 | E2E | P0 | Verify Phi-4-mini x86_64 AppImage exists in release artifacts | Critical release deliverable |
| 004.2-E2E-004 | E2E | P0 | Verify Phi-4-mini aarch64 AppImage exists in release artifacts | Critical release deliverable |
| 004.2-INT-003 | Integration | P1 | Verify Phi-4-mini AppImage size is within expected range (2-3GB) | Size validation for user expectations |
| 004.2-INT-004 | Integration | P1 | Verify Phi-4-mini model file bundled at `usr/share/models/` | Correct bundling structure |

---

### AC-3: `llm.call` action routes to local llama-cpp-python backend

**Requirement:** Local model inference via llama-cpp-python when model present

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-001 | Unit | P0 | `LlmLocalBackend` initializes with valid model path | Core backend functionality |
| 004.2-UNIT-002 | Unit | P0 | `LlmLocalBackend` raises clear error for invalid model path | Error handling for user debugging |
| 004.2-UNIT-003 | Unit | P0 | `llm.call` returns properly formatted response | Output format validation |
| 004.2-UNIT-004 | Unit | P1 | `LlmLocalBackend` handles empty prompt gracefully | Edge case handling |
| 004.2-UNIT-005 | Unit | P1 | `LlmLocalBackend` respects max_tokens parameter | Parameter passthrough |
| 004.2-UNIT-006 | Unit | P1 | `LlmLocalBackend` respects temperature parameter | Parameter passthrough |
| 004.2-INT-005 | Integration | P0 | `llm.call` action in YAML routes to local backend when model exists | Action routing logic |
| 004.2-INT-006 | Integration | P0 | `llm.call` produces coherent text output from Gemma model | End-to-end inference |
| 004.2-INT-007 | Integration | P0 | `llm.call` produces coherent text output from Phi-4-mini model | End-to-end inference |
| 004.2-INT-008 | Integration | P1 | Backend selection falls back to remote when local unavailable | Fallback behavior |

---

### AC-4: AppImage runs on systems without dependencies

**Requirement:** Fully self-contained, no Python/LLM dependencies required on host

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-E2E-005 | E2E | P0 | Gemma AppImage runs on clean Ubuntu 22.04 container | Self-containment validation |
| 004.2-E2E-006 | E2E | P0 | Phi-4-mini AppImage runs on clean Ubuntu 22.04 container | Self-containment validation |
| 004.2-E2E-007 | E2E | P1 | AppImage runs on system without Python installed | No Python dependency |
| 004.2-E2E-008 | E2E | P1 | AppImage runs on system without llama-cpp-python installed | No llama-cpp dependency |
| 004.2-INT-009 | Integration | P0 | All required .so libraries bundled in AppImage | Library completeness |

---

### AC-5: Model path auto-detected from AppImage or TEA_MODEL_PATH

**Requirement:** Model resolution: TEA_MODEL_PATH > APPDIR > default cache

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-007 | Unit | P0 | `get_model_path()` returns TEA_MODEL_PATH when set | Highest priority path |
| 004.2-UNIT-008 | Unit | P0 | `get_model_path()` returns APPDIR path when TEA_MODEL_PATH unset | AppImage path detection |
| 004.2-UNIT-009 | Unit | P1 | `get_model_path()` returns cache path when both env vars unset | Default fallback |
| 004.2-UNIT-010 | Unit | P1 | `get_model_path()` validates file existence for APPDIR candidate | Existence check |
| 004.2-INT-010 | Integration | P0 | AppImage auto-detects bundled model without env vars | Real AppImage path resolution |

---

### AC-6: Add `llama-cpp-python` to PyInstaller build

**Requirement:** PyInstaller collects llama-cpp-python correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-011 | Integration | P0 | PyInstaller binary includes llama_cpp module | Build completeness |
| 004.2-INT-012 | Integration | P1 | PyInstaller binary includes native .so files from llama-cpp | Native lib bundling |

---

### AC-7 & AC-8: GitHub Actions jobs for both architectures

**Requirement:** Build jobs for Gemma and Phi-4-mini on x86_64 and aarch64

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-E2E-009 | E2E | P0 | `build-python-llm-gemma-x86_64` job completes successfully | CI pipeline validation |
| 004.2-E2E-010 | E2E | P0 | `build-python-llm-gemma-aarch64` job completes successfully | CI pipeline validation |
| 004.2-E2E-011 | E2E | P0 | `build-python-llm-phi4-x86_64` job completes successfully | CI pipeline validation |
| 004.2-E2E-012 | E2E | P0 | `build-python-llm-phi4-aarch64` job completes successfully | CI pipeline validation |

---

### AC-9 & AC-10: Model download and bundling

**Requirement:** HuggingFace download + linuxdeploy bundling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-013 | Integration | P1 | Gemma model downloads successfully from HuggingFace | Model acquisition |
| 004.2-INT-014 | Integration | P1 | Phi-4-mini model downloads successfully from HuggingFace | Model acquisition |
| 004.2-INT-015 | Integration | P2 | Model checksum validates after download | Integrity verification |

---

### AC-11: Smoke test `--version` and `--impl`

**Requirement:** Basic CLI functionality works

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-011 | Unit | P0 | `--version` outputs version string | CLI smoke test |
| 004.2-UNIT-012 | Unit | P0 | `--impl` outputs "python" | Implementation identifier |

---

### AC-12: Functional test with `local-chat.yaml`

**Requirement:** Run example YAML agent with bundled model

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-016 | Integration | P0 | `examples/llm/local-chat.yaml` executes successfully | Example validation |

---

### AC-13: Graceful import handling when llama-cpp-python missing

**Requirement:** Clear error message, no crashes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-UNIT-013 | Unit | P0 | `LLAMA_CPP_AVAILABLE` is False when import fails | Feature detection |
| 004.2-UNIT-014 | Unit | P0 | `check_llm_local_available()` raises ImportError with install hint | User guidance |

---

### AC-14: Existing Python Prolog AppImage unchanged

**Requirement:** No regression to TEA-RELEASE-003 artifacts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 004.2-INT-017 | Integration | P2 | Python Prolog AppImage still builds successfully | Regression prevention |
| 004.2-INT-018 | Integration | P2 | Python Prolog AppImage smoke test passes | Regression prevention |

---

## Test Implementation Details

### Unit Test Mocking Strategy

```python
# Mock llama_cpp.Llama for unit tests
@pytest.fixture
def mock_llama():
    with patch('the_edge_agent.actions.llm_local.Llama') as mock:
        mock.return_value.create_completion.return_value = {
            'choices': [{'text': 'Mocked response'}]
        }
        yield mock

# Mock environment for path tests
@pytest.fixture
def clean_env():
    with patch.dict(os.environ, {}, clear=True):
        yield
```

### Integration Test Environment

```yaml
# docker-compose.test.yml for clean container testing
services:
  clean-ubuntu:
    image: ubuntu:22.04
    volumes:
      - ./dist:/app
    command: /app/tea-python-llm-gemma-*.AppImage --version
```

### E2E Test in CI Workflow

```yaml
- name: E2E Test - Clean Container
  run: |
    docker run --rm -v $(pwd)/dist:/dist ubuntu:22.04 \
      /dist/tea-python-llm-gemma-${{ env.VERSION }}-x86_64.AppImage --version
```

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Test IDs |
|------|-------------|--------|----------|
| llama-cpp-python ARM64 compilation fails | Medium | High | 004.2-E2E-010, 004.2-E2E-012 |
| Model path resolution fails in AppImage | Medium | High | 004.2-UNIT-007 to 010, 004.2-INT-010 |
| AppImage missing native .so dependencies | Low | Critical | 004.2-INT-009, 004.2-E2E-005/006 |
| Graceful fallback doesn't work | Low | Medium | 004.2-UNIT-013/014, 004.2-INT-008 |
| HuggingFace download rate-limited | Medium | Medium | 004.2-INT-013/014 |
| Regression to Prolog AppImage | Low | High | 004.2-INT-017/018 |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (P0 Unit)
1. 004.2-UNIT-001 to 004.2-UNIT-003 (Backend core)
2. 004.2-UNIT-007, 004.2-UNIT-008 (Path resolution)
3. 004.2-UNIT-011, 004.2-UNIT-012 (CLI smoke)
4. 004.2-UNIT-013, 004.2-UNIT-014 (Graceful import)

### Phase 2: Integration Validation (P0 Integration)
1. 004.2-INT-005 to 004.2-INT-007 (Action routing)
2. 004.2-INT-009 (Library bundling)
3. 004.2-INT-010 (AppImage path detection)
4. 004.2-INT-011 (PyInstaller validation)
5. 004.2-INT-016 (Example execution)

### Phase 3: E2E Pipeline (P0 E2E)
1. 004.2-E2E-009 to 004.2-E2E-012 (CI job validation)
2. 004.2-E2E-001 to 004.2-E2E-004 (Artifact existence)
3. 004.2-E2E-005, 004.2-E2E-006 (Clean container)

### Phase 4: P1+ Coverage
1. All P1 tests in order by ID
2. P2 regression tests
3. P3 edge cases if time permits

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (004.2-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 14
    integration: 18
    e2e: 12
  by_priority:
    p0: 18
    p1: 15
    p2: 7
    p3: 2
  coverage_gaps: []
  high_risk_areas:
    - model_path_resolution
    - llama_cpp_arm64_compilation
    - appimage_self_containment
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.2-test-design-20260108.md
P0 tests identified: 18
Story ID: TEA-RELEASE-004.2
Epic: TEA-RELEASE-004 (LLM Bundled Distributions)
```
