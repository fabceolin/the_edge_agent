# Test Design: Story TEA-RELEASE-005.2

**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)
**Story:** Cosmopolitan Build Pipeline

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 28 |
| **Unit tests** | 4 (14%) |
| **Integration tests** | 10 (36%) |
| **E2E tests** | 14 (50%) |
| **Priority distribution** | P0: 8, P1: 12, P2: 6, P3: 2 |

### Strategy Rationale

This story focuses on **build infrastructure and cross-platform binary distribution** - inherently integration and E2E heavy. The primary risk is not logic errors but **platform compatibility, toolchain integration, and runtime environment issues**. Therefore:

- **Unit tests** are limited to pure validation logic (version parsing, config validation)
- **Integration tests** validate toolchain interactions, build steps, and artifact generation
- **E2E tests** dominate because the core value proposition is "binary works on all platforms"

---

## Test Scenarios by Acceptance Criteria

### AC-1: GitHub Actions workflow `build-ape.yaml` created

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-UNIT-001 | Unit | P1 | Validate workflow YAML schema | Catch syntax errors before CI runs |
| TEA-RELEASE-005.2-INT-001 | Integration | P0 | Workflow triggers on version tag push | Core release mechanism must work |
| TEA-RELEASE-005.2-INT-002 | Integration | P1 | Workflow triggers on workflow_dispatch | Manual release capability required |

### AC-2: Cosmopolitan toolchain (`cosmocc`) installed in CI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-INT-003 | Integration | P0 | Cosmocc downloads and extracts successfully | Build cannot proceed without toolchain |
| TEA-RELEASE-005.2-INT-004 | Integration | P1 | Cosmocc version matches COSMO_VERSION env | Version pinning prevents drift |
| TEA-RELEASE-005.2-INT-005 | Integration | P2 | Cosmocc cached between runs (if implemented) | Performance optimization validation |

### AC-3: TEA Rust compiles with Cosmopolitan target

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-UNIT-002 | Unit | P1 | `cosmo-config.toml` parses correctly | Config errors fail silently |
| TEA-RELEASE-005.2-INT-006 | Integration | P0 | `cargo build --release` with cosmocc succeeds | Core build step - non-negotiable |
| TEA-RELEASE-005.2-INT-007 | Integration | P1 | Build produces valid APE binary format | Output format verification |

### AC-4: `tea.com` includes Scryer Prolog support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-INT-008 | Integration | P0 | `--features scryer` compiles with cosmocc | Critical feature must compile |
| TEA-RELEASE-005.2-E2E-001 | E2E | P0 | Execute Prolog query on Linux | Scryer runtime works in APE |
| TEA-RELEASE-005.2-E2E-002 | E2E | P0 | Execute Prolog query on Windows | Cross-platform Scryer validation |
| TEA-RELEASE-005.2-E2E-003 | E2E | P0 | Execute Prolog query on macOS | Cross-platform Scryer validation |

### AC-5: `tea.com` includes mlua (Lua) support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-INT-009 | Integration | P1 | mlua feature compiles with cosmocc | Lua support included |
| TEA-RELEASE-005.2-E2E-004 | E2E | P1 | Execute Lua script on Linux | Lua runtime works in APE |
| TEA-RELEASE-005.2-E2E-005 | E2E | P1 | Execute Lua script on Windows | Cross-platform Lua validation |
| TEA-RELEASE-005.2-E2E-006 | E2E | P1 | Execute Lua script on macOS | Cross-platform Lua validation |

### AC-6: Binary runs on Ubuntu 22.04 without dependencies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-E2E-007 | E2E | P0 | Run `tea.com` on clean Ubuntu 22.04 container | Primary Linux target validation |
| TEA-RELEASE-005.2-E2E-008 | E2E | P2 | Run on Ubuntu 20.04 (older glibc) | Backward compatibility check |

### AC-7: Binary runs on Windows 11 without dependencies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-E2E-009 | E2E | P0 | Run `tea.com` on clean Windows 11 runner | Primary Windows target validation |
| TEA-RELEASE-005.2-E2E-010 | E2E | P3 | Run on Windows 10 (older OS) | Backward compatibility - low priority |

### AC-8: Binary runs on macOS 13+ without dependencies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-E2E-011 | E2E | P0 | Run `tea.com` on macOS 13 runner | Primary macOS target validation |
| TEA-RELEASE-005.2-E2E-012 | E2E | P2 | Run on macOS 14 (arm64) | ARM64 architecture validation |

### AC-9: `tea.com --version` and `tea.com --impl` work on all platforms

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-UNIT-003 | Unit | P1 | Version string format validation | Consistent version output |
| TEA-RELEASE-005.2-E2E-013 | E2E | P1 | `--version` outputs expected format on all platforms | User-facing command works |
| TEA-RELEASE-005.2-E2E-014 | E2E | P1 | `--impl` outputs Rust/APE identifier | Implementation detection works |

### AC-10: Binary size <25MB (without model)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-UNIT-004 | Unit | P2 | Artifact size assertion in CI | Automated size guard |
| TEA-RELEASE-005.2-INT-010 | Integration | P2 | LTO and strip produce smaller binary | Optimization validation |

### AC-11: Release workflow updated to include APE artifact

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RELEASE-005.2-E2E-015 | E2E | P1 | Release includes `tea-{version}.com` artifact | Release artifact present |
| TEA-RELEASE-005.2-E2E-016 | E2E | P3 | Downloaded artifact matches build artifact checksum | Integrity verification |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| RISK-001 | Cosmopolitan toolchain unavailable | TEA-RELEASE-005.2-INT-003 |
| RISK-002 | Rust crates incompatible with cosmocc | TEA-RELEASE-005.2-INT-006, INT-008 |
| RISK-003 | Binary fails on specific platform | TEA-RELEASE-005.2-E2E-007 through E2E-012 |
| RISK-004 | Scryer/Lua runtime broken in APE | TEA-RELEASE-005.2-E2E-001 through E2E-006 |
| RISK-005 | Binary size exceeds distribution limits | TEA-RELEASE-005.2-UNIT-004, INT-010 |
| RISK-006 | Release workflow doesn't include APE | TEA-RELEASE-005.2-E2E-015 |

---

## Test Implementation Details

### Unit Test Implementations

```yaml
TEA-RELEASE-005.2-UNIT-001:
  file: tests/ci/test_workflow_schema.py
  framework: pytest + yamllint
  mock_requirements: None
  estimated_runtime: <1s

TEA-RELEASE-005.2-UNIT-002:
  file: rust/tests/config_validation.rs
  framework: cargo test
  mock_requirements: None
  estimated_runtime: <1s

TEA-RELEASE-005.2-UNIT-003:
  file: rust/tests/version_format.rs
  framework: cargo test
  mock_requirements: None
  estimated_runtime: <1s

TEA-RELEASE-005.2-UNIT-004:
  file: .github/workflows/build-ape.yaml (inline assertion)
  framework: bash assertion
  mock_requirements: None
  estimated_runtime: <1s
```

### Integration Test Implementations

```yaml
TEA-RELEASE-005.2-INT-001:
  location: GitHub Actions (triggered by tag)
  validation: Workflow run success status

TEA-RELEASE-005.2-INT-003:
  location: .github/workflows/build-ape.yaml
  command: |
    cosmocc --version || exit 1
  expected: Exit code 0, version string output

TEA-RELEASE-005.2-INT-006:
  location: .github/workflows/build-ape.yaml
  command: |
    cd rust && cargo build --release --features scryer
  expected: Exit code 0, binary at target/release/tea

TEA-RELEASE-005.2-INT-007:
  location: .github/workflows/build-ape.yaml
  command: |
    file tea.com | grep -q "executable"
  expected: APE format detected
```

### E2E Test Implementations

```yaml
TEA-RELEASE-005.2-E2E-001:
  platform: ubuntu-latest
  command: |
    ./tea.com --eval "X = 1 + 1."
  expected: X = 2 output

TEA-RELEASE-005.2-E2E-002:
  platform: windows-latest
  command: |
    .\tea.com --eval "X = 1 + 1."
  expected: X = 2 output

TEA-RELEASE-005.2-E2E-007:
  platform: ubuntu-22.04 (clean container)
  command: |
    docker run --rm -v $(pwd):/app ubuntu:22.04 /app/tea.com --version
  expected: Version string output, exit 0

TEA-RELEASE-005.2-E2E-009:
  platform: windows-latest
  command: |
    .\tea.com --version
  expected: Version string output, exit 0
```

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Critical)

1. TEA-RELEASE-005.2-INT-003 - Cosmocc installs
2. TEA-RELEASE-005.2-INT-006 - Build succeeds
3. TEA-RELEASE-005.2-INT-008 - Scryer compiles
4. TEA-RELEASE-005.2-INT-001 - Workflow triggers
5. TEA-RELEASE-005.2-E2E-007 - Linux runs
6. TEA-RELEASE-005.2-E2E-009 - Windows runs
7. TEA-RELEASE-005.2-E2E-011 - macOS runs
8. TEA-RELEASE-005.2-E2E-001/002/003 - Scryer cross-platform

### Phase 2: Core Validation (P1 High)

9. TEA-RELEASE-005.2-UNIT-001/002/003 - Config and format validation
10. TEA-RELEASE-005.2-INT-002/004/009 - Workflow and Lua compile
11. TEA-RELEASE-005.2-E2E-004/005/006 - Lua cross-platform
12. TEA-RELEASE-005.2-E2E-013/014/015 - CLI commands and release

### Phase 3: Completeness (P2 Medium)

13. TEA-RELEASE-005.2-INT-005/010 - Cache and optimization
14. TEA-RELEASE-005.2-UNIT-004 - Size check
15. TEA-RELEASE-005.2-E2E-008/012 - Backward compatibility

### Phase 4: Nice-to-Have (P3 Low)

16. TEA-RELEASE-005.2-E2E-010 - Windows 10
17. TEA-RELEASE-005.2-E2E-016 - Checksum verification

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels appropriate for test type (heavy E2E for cross-platform)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (cross-platform = P0)
- [x] Test IDs follow `{epic}.{story}-{LEVEL}-{SEQ}` convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to tests

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 4
    integration: 10
    e2e: 14
  by_priority:
    p0: 8
    p1: 12
    p2: 6
    p3: 2
  coverage_gaps: []
  special_considerations:
    - Cross-platform testing requires multi-runner CI matrix
    - Scryer Prolog tests require valid Prolog queries
    - Binary size monitoring should be automated
    - Clean container tests validate no hidden dependencies
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-005.2-test-design-20260112.md
P0 tests identified: 8
Critical path: Toolchain install -> Build -> Platform smoke tests
```
