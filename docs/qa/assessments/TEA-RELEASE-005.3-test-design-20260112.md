# Test Design: Story TEA-RELEASE-005.3

**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)
**Story:** LLM Model Bundling via ZIP Append

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 10 (42%) |
| **Integration tests** | 9 (37%) |
| **E2E tests** | 5 (21%) |
| **Priority distribution** | P0: 8, P1: 10, P2: 4, P3: 2 |

### Risk Assessment Summary

This feature involves:
- **Single-file distribution** - Critical for user experience
- **Cross-platform compatibility** - Must work on Linux, Windows, macOS
- **Model loading from virtual filesystem** - Cosmopolitan-specific behavior
- **Graceful fallback** - Must handle missing bundled models
- **Performance requirements** - <3 second startup time

---

## Test Scenarios by Acceptance Criteria

### AC-1: Model bundling script `scripts/bundle-model.sh` created

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-001 | Unit | P1 | Script creates output file when base binary exists | Pure file operation logic |
| 005.3-UNIT-002 | Unit | P1 | Script downloads model when not present locally | Download logic validation |
| 005.3-UNIT-003 | Unit | P2 | Script accepts custom model name parameter | Parameter parsing validation |
| 005.3-UNIT-004 | Unit | P2 | Script accepts custom output name parameter | Parameter parsing validation |
| 005.3-INT-001 | Integration | P0 | Script produces valid ZIP-appended binary | Critical bundling validation |

---

### AC-2: Model appended to binary via `zip -A` command

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-005 | Unit | P1 | ZIP central directory exists at end of bundled binary | ZIP structure validation |
| 005.3-INT-002 | Integration | P0 | Bundled model accessible via `unzip -l` listing | Verifies ZIP append correctness |
| 005.3-INT-003 | Integration | P1 | manifest.json appended alongside model | Manifest bundling verification |

---

### AC-3: `/zip/` virtual filesystem accessible at runtime

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-INT-004 | Integration | P0 | `/zip/models/` path resolves to bundled content | Core Cosmopolitan feature test |
| 005.3-INT-005 | Integration | P0 | `fs::read("/zip/models/*.gguf")` returns model bytes | File system abstraction test |

---

### AC-4: Model loads from `/zip/models/` path without extraction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-006 | Unit | P0 | `ModelLoader::is_zip_path()` correctly identifies /zip/ paths | Pure logic validation |
| 005.3-UNIT-007 | Unit | P1 | `ModelLoader::find_model()` prioritizes /zip/ path first | Search order logic |
| 005.3-INT-006 | Integration | P0 | Model bytes loaded match original model file checksum | Data integrity validation |

---

### AC-5: `llm.call` action detects and uses bundled model automatically

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-008 | Unit | P1 | LlmAction uses ModelLoader::find_model() when no explicit path | Integration point validation |
| 005.3-INT-007 | Integration | P1 | llm.call succeeds without explicit model_path on bundled binary | Auto-detection integration |
| 005.3-E2E-001 | E2E | P0 | Complete LLM inference works on bundled tea-llm.com | Critical user journey |

---

### AC-6: `tea-llm.com` artifact produced (~2GB with Phi-4-mini)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-INT-008 | Integration | P1 | CI workflow produces tea-llm.com artifact | Build pipeline validation |
| 005.3-E2E-002 | E2E | P1 | Downloaded artifact size within expected range (1.8-2.2GB) | Artifact integrity check |

---

### AC-7: Startup time <3 seconds (no extraction delay)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-009 | Unit | P1 | No temp directory creation during model load | Extraction avoidance validation |
| 005.3-E2E-003 | E2E | P0 | Bundled binary startup time <3 seconds | Performance requirement |
| 005.3-E2E-004 | E2E | P3 | Memory usage during load matches mmap behavior (~RSS stable) | Memory mapping verification |

---

### AC-8: LLM inference works correctly on Linux, Windows, macOS

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-E2E-005 | E2E | P0 | tea-llm.com produces valid LLM output on Linux | Cross-platform critical path |
| 005.3-E2E-006 | E2E | P0 | tea-llm.com produces valid LLM output on Windows | Cross-platform critical path |
| 005.3-E2E-007 | E2E | P0 | tea-llm.com produces valid LLM output on macOS | Cross-platform critical path |

**Note:** E2E-005, 006, 007 are logically grouped as AC-8 coverage but tracked as separate platform tests.

---

### AC-9: Fallback to `~/.cache/tea/models/` if bundled model not found

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-010 | Unit | P1 | `ModelLoader::expand_path()` expands `${HOME}` and `${TEA_MODEL_PATH}` | Path expansion logic |
| 005.3-INT-009 | Integration | P1 | Model loads from cache when /zip/ path not available | Fallback mechanism test |
| 005.3-UNIT-011 | Unit | P3 | Helpful error message when no model found anywhere | Error UX validation |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Tests Mitigating |
|------|-------------|--------|------------------|
| ZIP append corrupts binary | Low | Critical | 005.3-INT-001, 005.3-INT-002 |
| /zip/ filesystem unavailable | Low | Critical | 005.3-INT-004, 005.3-INT-005 |
| Model checksum mismatch | Medium | High | 005.3-INT-006 |
| Startup performance regression | Medium | High | 005.3-E2E-003 |
| Cross-platform incompatibility | Medium | Critical | 005.3-E2E-005/006/007 |
| Fallback path broken | Low | Medium | 005.3-INT-009, 005.3-UNIT-010 |
| CI artifact corruption | Low | High | 005.3-INT-008, 005.3-E2E-002 |

---

## Test Implementation Locations

| Test Level | Location | Framework |
|------------|----------|-----------|
| Unit (Rust) | `rust/tests/test_model_loader.rs` | `#[test]` / cargo test |
| Integration (Rust) | `rust/tests/integration/test_model_bundling.rs` | cargo test with fixtures |
| Integration (Shell) | `scripts/test-bundle.sh` | Shell assertions |
| E2E | CI workflow / manual | GitHub Actions matrix |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on logic errors)
   - 005.3-UNIT-006: is_zip_path detection

2. **P0 Integration tests** (validate core bundling)
   - 005.3-INT-001: ZIP-appended binary valid
   - 005.3-INT-002: ZIP listing verification
   - 005.3-INT-004: /zip/ path resolution
   - 005.3-INT-005: fs::read from /zip/
   - 005.3-INT-006: Checksum integrity

3. **P0 E2E tests** (critical user paths)
   - 005.3-E2E-001: LLM inference on bundled binary
   - 005.3-E2E-003: Startup time <3s
   - 005.3-E2E-005/006/007: Cross-platform validation

4. **P1 tests in order** (core functionality)

5. **P2+ tests as time permits**

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 10
    integration: 9
    e2e: 5
  by_priority:
    p0: 8
    p1: 10
    p2: 4
    p3: 2
  coverage_gaps: []
  critical_paths:
    - "ZIP append integrity"
    - "/zip/ filesystem access"
    - "Cross-platform LLM inference"
    - "Startup performance"
  recommendations:
    - "Cross-platform E2E tests should run in CI matrix (Linux, Windows, macOS)"
    - "Performance test (E2E-003) should have baseline established before implementation"
    - "Consider adding model corruption test (truncated GGUF file handling)"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Cross-platform requirements addressed
- [x] Performance requirements testable

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-005.3-test-design-20260112.md
P0 tests identified: 8
Cross-platform E2E tests: 3 (Linux, Windows, macOS)
Performance tests: 2 (startup time, memory behavior)
```
