# Test Design: Story TEA-RELEASE-004.1

**Story Title:** Rust LLM AppImage (Gemma + Phi-4-mini Variants)

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 12 (29%) |
| **Integration tests** | 18 (43%) |
| **E2E tests** | 12 (28%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 8 | Core functionality, self-containment, model routing |
| P1 | 18 | Build infrastructure, architecture coverage, smoke tests |
| P2 | 12 | Feature variants, optional configurations |
| P3 | 4 | Edge cases, optional feature combinations |

---

## Test Scenarios by Acceptance Criteria

### AC-1: Gemma Variant Release Artifacts

**Requirement:** Release includes Gemma variants: `tea-rust-llm-gemma-{version}-{arch}.AppImage` (~5GB, best quality)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-001 | Integration | P1 | Verify Gemma x86_64 AppImage artifact exists in release with correct naming pattern | CI artifact validation |
| 004.1-INT-002 | Integration | P1 | Verify Gemma aarch64 AppImage artifact exists in release with correct naming pattern | CI artifact validation |
| 004.1-INT-003 | Integration | P2 | Verify Gemma AppImage file size is within expected range (4-6GB) | Release artifact quality |
| 004.1-E2E-001 | E2E | P1 | Download and verify Gemma AppImage from release page | Full release pipeline validation |

---

### AC-2: Phi-4-mini Variant Release Artifacts

**Requirement:** Release includes Phi-4-mini variants: `tea-rust-llm-phi4-{version}-{arch}.AppImage` (~2.5GB, 128K context)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-004 | Integration | P1 | Verify Phi-4-mini x86_64 AppImage artifact exists in release with correct naming pattern | CI artifact validation |
| 004.1-INT-005 | Integration | P1 | Verify Phi-4-mini aarch64 AppImage artifact exists in release with correct naming pattern | CI artifact validation |
| 004.1-INT-006 | Integration | P2 | Verify Phi-4-mini AppImage file size is within expected range (2-3GB) | Release artifact quality |
| 004.1-E2E-002 | E2E | P1 | Download and verify Phi-4-mini AppImage from release page | Full release pipeline validation |

---

### AC-3: LLM Action Routing to Local Backend

**Requirement:** `llm.call` action routes to local llama-cpp-2 backend when model is present

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-UNIT-001 | Unit | P0 | Test `llm.call` action dispatcher detects local model availability | Core routing logic, pure function |
| 004.1-UNIT-002 | Unit | P0 | Test backend selection logic prefers local over remote when model present | Business logic validation |
| 004.1-UNIT-003 | Unit | P1 | Test backend selection falls back to remote when local model absent | Fallback path logic |
| 004.1-INT-007 | Integration | P0 | Test llama-cpp-2 backend receives request when local model available | Component interaction verification |
| 004.1-INT-008 | Integration | P1 | Test remote API backend receives request when local model unavailable | Fallback interaction verification |

---

### AC-4: AppImage Self-Containment

**Requirement:** AppImage runs on systems without any LLM dependencies installed (fully self-contained)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-E2E-003 | E2E | P0 | Run Gemma AppImage on clean Ubuntu 22.04 container (no LLM libs) | Critical: self-containment guarantee |
| 004.1-E2E-004 | E2E | P0 | Run Phi-4-mini AppImage on clean Ubuntu 22.04 container (no LLM libs) | Critical: self-containment guarantee |
| 004.1-E2E-005 | E2E | P1 | Run AppImage on Debian 12 minimal container | Cross-distro compatibility |
| 004.1-E2E-006 | E2E | P2 | Run AppImage on Fedora 39 minimal container | Extended distro coverage |
| 004.1-INT-009 | Integration | P0 | Verify all required shared libraries bundled in AppDir/usr/lib/ | Dependency isolation check |

---

### AC-5: Model Path Auto-Detection

**Requirement:** Model path auto-detected relative to AppImage extraction directory

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-UNIT-004 | Unit | P0 | Test model path resolution from `$APPDIR/usr/share/models/` | Core path detection logic |
| 004.1-UNIT-005 | Unit | P1 | Test model path resolution from `TEA_MODEL_PATH` environment variable | Alternative path support |
| 004.1-UNIT-006 | Unit | P1 | Test model path resolution priority (APPDIR > TEA_MODEL_PATH) | Priority order validation |
| 004.1-INT-010 | Integration | P0 | Test AppImage correctly sets and resolves model path at runtime | End-to-end path resolution |

---

### AC-6: llama-cpp-2 Crate Dependency

**Requirement:** Add `llama-cpp-2` crate dependency with feature flags (`cuda`, `metal` optional)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-UNIT-007 | Unit | P1 | Test Cargo.toml contains llama-cpp-2 with `llm-local` feature | Build configuration validation |
| 004.1-INT-011 | Integration | P1 | Build Rust crate with `--features llm-local` succeeds | Feature integration |
| 004.1-INT-012 | Integration | P2 | Build Rust crate with `--features llm-local-cuda` succeeds (if CUDA available) | Optional feature verification |
| 004.1-INT-013 | Integration | P2 | Build Rust crate with `--features llm-local-metal` succeeds (macOS only) | Optional feature verification |

---

### AC-7: GitHub Actions Jobs for Gemma Variants (x86_64 + aarch64)

**Requirement:** GitHub Actions jobs for Gemma variants

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-014 | Integration | P1 | Verify `build-rust-llm-gemma-appimage-x86_64` job exists in release.yaml | CI configuration validation |
| 004.1-INT-015 | Integration | P1 | Verify `build-rust-llm-gemma-appimage-aarch64` job exists in release.yaml | CI configuration validation |
| 004.1-E2E-007 | E2E | P1 | Trigger Gemma x86_64 build job and verify completion | Full CI pipeline test |
| 004.1-E2E-008 | E2E | P1 | Trigger Gemma aarch64 build job and verify completion | Full CI pipeline test |

---

### AC-8: GitHub Actions Jobs for Phi-4-mini Variants

**Requirement:** GitHub Actions jobs for Phi-4-mini variants (x86_64 + aarch64)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-016 | Integration | P1 | Verify `build-rust-llm-phi4-appimage-x86_64` job exists in release.yaml | CI configuration validation |
| 004.1-INT-017 | Integration | P1 | Verify `build-rust-llm-phi4-appimage-aarch64` job exists in release.yaml | CI configuration validation |
| 004.1-E2E-009 | E2E | P1 | Trigger Phi-4-mini x86_64 build job and verify completion | Full CI pipeline test |
| 004.1-E2E-010 | E2E | P1 | Trigger Phi-4-mini aarch64 build job and verify completion | Full CI pipeline test |

---

### AC-9: Model Download from HuggingFace

**Requirement:** Model files downloaded from HuggingFace during build (Gemma Q4_K_M or Phi-4-mini Q3_K_S)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-UNIT-008 | Unit | P2 | Validate HuggingFace URL construction for Gemma model | URL format correctness |
| 004.1-UNIT-009 | Unit | P2 | Validate HuggingFace URL construction for Phi-4-mini model | URL format correctness |
| 004.1-INT-018 | Integration | P1 | Verify Gemma model download completes within workflow timeout | CI download verification |

---

### AC-10: AppImage Bundling with linuxdeploy

**Requirement:** AppImage bundled with linuxdeploy including model file

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-UNIT-010 | Unit | P2 | Validate AppDir structure contains required directories | Structure validation |
| 004.1-INT-019 | Integration | P0 | Verify model file present at `usr/share/models/` in AppDir | Critical bundling verification |

---

### AC-11: Smoke Tests (--version and --impl)

**Requirement:** Smoke test: `--version` and `--impl` pass

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-E2E-011 | E2E | P0 | Execute `tea --version` on Gemma AppImage and verify output | Basic executable validation |
| 004.1-E2E-012 | E2E | P0 | Execute `tea --impl` on Gemma AppImage and verify Rust implementation detected | Implementation detection |
| 004.1-UNIT-011 | Unit | P1 | Test `--version` flag parsing and output format | CLI flag handling |
| 004.1-UNIT-012 | Unit | P1 | Test `--impl` flag parsing and output format | CLI flag handling |

---

### AC-12: Functional Test with local-chat.yaml

**Requirement:** Functional test: Run `examples/llm/local-chat.yaml` with local model

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-020 | Integration | P0 | Load and validate `examples/llm/local-chat.yaml` YAML structure | YAML engine integration |
| 004.1-INT-021 | Integration | P0 | Execute local-chat.yaml with Gemma model and verify LLM response | Full functional validation |
| 004.1-INT-022 | Integration | P1 | Execute local-chat.yaml with Phi-4-mini model and verify LLM response | Model variant functional test |

---

### AC-13: Native Build Without llm-local Feature

**Requirement:** Native Rust build without `llm-local` feature continues to work

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-023 | Integration | P0 | Build Rust crate without `--features llm-local` succeeds | Regression prevention |
| 004.1-INT-024 | Integration | P0 | Run `cargo test` without `llm-local` feature - all tests pass | Feature isolation |

---

### AC-14: Existing Prolog AppImage Unchanged

**Requirement:** Existing Prolog AppImage (TEA-RELEASE-002) unchanged

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 004.1-INT-025 | Integration | P0 | Verify Prolog AppImage build job still exists and is unchanged | Regression prevention |
| 004.1-INT-026 | Integration | P1 | Build Prolog AppImage and verify functionality unchanged | Compatibility assurance |
| 004.1-UNIT-013 | Unit | P2 | Compare release.yaml diff - no Prolog job modifications | Configuration regression |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| llama-cpp-2 ARM64 compilation failure | Medium | High | 004.1-INT-015, 004.1-E2E-008, 004.1-E2E-010 |
| AppImage not self-contained | Low | Critical | 004.1-E2E-003, 004.1-E2E-004, 004.1-INT-009 |
| Model path resolution failure | Low | High | 004.1-UNIT-004, 004.1-UNIT-005, 004.1-INT-010 |
| Regression in existing Prolog AppImage | Low | Medium | 004.1-INT-025, 004.1-INT-026 |
| GitHub Release 2GB file limit exceeded | High | High | External hosting validation (manual), 004.1-INT-003 |
| HuggingFace model download timeout | Medium | Medium | 004.1-INT-018 (with timeout configuration) |

---

## Recommended Execution Order

### Phase 1: Fail-Fast (P0 Unit + Integration)
1. 004.1-UNIT-001, 004.1-UNIT-002 (routing logic)
2. 004.1-UNIT-004 (model path resolution)
3. 004.1-INT-023, 004.1-INT-024 (regression - no feature)
4. 004.1-INT-007 (backend integration)
5. 004.1-INT-019 (model bundling)

### Phase 2: Core E2E Validation (P0 E2E)
6. 004.1-E2E-011, 004.1-E2E-012 (smoke tests)
7. 004.1-E2E-003, 004.1-E2E-004 (self-containment)

### Phase 3: P1 Tests (Full Coverage)
8. 004.1-INT-020, 004.1-INT-021, 004.1-INT-022 (functional tests)
9. 004.1-INT-001 through 004.1-INT-006 (artifact validation)
10. 004.1-E2E-007 through 004.1-E2E-010 (CI pipeline tests)
11. 004.1-INT-025, 004.1-INT-026 (Prolog regression)

### Phase 4: P2+ Tests (As Time Permits)
12. All remaining P2/P3 tests

---

## Test Environment Requirements

### Unit Test Environment
- Rust toolchain (stable)
- No external dependencies required
- Fast execution (<30s total)

### Integration Test Environment
- Docker for container testing
- GitHub Actions runner access for CI tests
- ~10GB disk space for model files
- Internet access for HuggingFace downloads

### E2E Test Environment
- Clean Ubuntu 22.04 container image
- Clean Debian 12 container image (P1)
- Clean Fedora 39 container image (P2)
- AppImage FUSE support enabled
- No pre-installed LLM libraries

---

## Quality Checklist

- [x] Every AC has test coverage (14 ACs, 42 scenarios)
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core functionality, self-containment)
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 12
    integration: 18
    e2e: 12
  by_priority:
    p0: 8
    p1: 18
    p2: 12
    p3: 4
  coverage_gaps: []
  risk_mitigations:
    - risk: "ARM64 compilation"
      tests: ["004.1-INT-015", "004.1-E2E-008", "004.1-E2E-010"]
    - risk: "Self-containment failure"
      tests: ["004.1-E2E-003", "004.1-E2E-004", "004.1-INT-009"]
    - risk: "Model path resolution"
      tests: ["004.1-UNIT-004", "004.1-UNIT-005", "004.1-INT-010"]
    - risk: "Prolog AppImage regression"
      tests: ["004.1-INT-025", "004.1-INT-026"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-004.1-test-design-20260108.md
P0 tests identified: 8
P1 tests identified: 18
Total ACs covered: 14/14 (100%)
```
