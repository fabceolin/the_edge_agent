# Test Design: Story TEA-RELEASE-005.4

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 34
- **Unit tests:** 6 (18%)
- **Integration tests:** 16 (47%)
- **E2E tests:** 12 (35%)
- **Priority distribution:** P0: 14, P1: 12, P2: 6, P3: 2

### Strategy Rationale

This story focuses on cross-platform testing validation for Actually Portable Executables (APE). The testing strategy emphasizes:

1. **Platform Consistency**: Ensuring identical behavior across Linux, Windows, and macOS
2. **Security Software Compatibility**: Validating behavior with Windows Defender and macOS Gatekeeper
3. **Deterministic Output Verification**: Confirming Prolog, Lua, and LLM outputs match across platforms
4. **CI/CD Integration**: Validating the test matrix workflow executes correctly

Given the nature of cross-platform binary testing, integration tests dominate (47%) as they validate real system interactions, while E2E tests (35%) cover critical user journeys across the full matrix.

---

## Test Scenarios by Acceptance Criteria

### AC-1: Test matrix covers Ubuntu 20.04, 22.04, 24.04

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-001 | Integration | P0 | TEA binary executes `--version` on Ubuntu 20.04 | Validates binary compatibility with oldest supported Ubuntu |
| 005.4-INT-002 | Integration | P0 | TEA binary executes `--version` on Ubuntu 22.04 | Validates binary compatibility with LTS release |
| 005.4-INT-003 | Integration | P0 | TEA binary executes `--version` on Ubuntu 24.04 | Validates binary compatibility with latest LTS |
| 005.4-INT-004 | Integration | P1 | TEA binary runs Prolog test agent on all Ubuntu versions | Multi-version consistency check |

### AC-2: Test matrix covers Windows 10, Windows 11

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-005 | Integration | P0 | TEA binary executes `--version` on Windows 10 (2019 runner) | Validates compatibility with Windows 10 |
| 005.4-INT-006 | Integration | P0 | TEA binary executes `--version` on Windows 11 (latest runner) | Validates compatibility with Windows 11 |
| 005.4-INT-007 | Integration | P1 | TEA binary runs PowerShell commands successfully | Windows-specific shell execution |

### AC-3: Test matrix covers macOS 12, 13, 14 (Intel and ARM)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-008 | Integration | P0 | TEA binary executes `--version` on macOS 12 (Monterey) | Intel architecture validation |
| 005.4-INT-009 | Integration | P0 | TEA binary executes `--version` on macOS 13 (Ventura) | Transition architecture validation |
| 005.4-INT-010 | Integration | P0 | TEA binary executes `--version` on macOS 14 (Sonoma) | ARM64 architecture validation |
| 005.4-INT-011 | Integration | P1 | Binary is executable after chmod on all macOS versions | Unix permission handling |

### AC-4: Prolog queries produce identical results across platforms

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-UNIT-001 | Unit | P0 | Prolog factorial(5) returns 120 on Linux | Baseline calculation verification |
| 005.4-UNIT-002 | Unit | P0 | Prolog factorial(5) returns 120 on Windows | Cross-platform parity |
| 005.4-UNIT-003 | Unit | P0 | Prolog factorial(5) returns 120 on macOS | Cross-platform parity |
| 005.4-E2E-001 | E2E | P0 | Full Prolog test agent produces identical output hash across all platforms | Critical cross-platform consistency |

### AC-5: Lua scripts produce identical results across platforms

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-UNIT-004 | Unit | P0 | Lua fibonacci(10) returns 55 on Linux | Baseline calculation verification |
| 005.4-UNIT-005 | Unit | P0 | Lua fibonacci(10) returns 55 on Windows | Cross-platform parity |
| 005.4-UNIT-006 | Unit | P0 | Lua fibonacci(10) returns 55 on macOS | Cross-platform parity |
| 005.4-E2E-002 | E2E | P0 | Full Lua test agent produces identical output hash across all platforms | Critical cross-platform consistency |

### AC-6: LLM inference produces consistent results (deterministic seed)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-012 | Integration | P1 | LLM with temperature=0, seed=42 produces non-empty response | Core LLM functionality |
| 005.4-INT-013 | Integration | P1 | Same prompt+seed produces same response on repeated runs | Determinism verification |
| 005.4-E2E-003 | E2E | P1 | LLM test agent completes without error on Linux | Full agent workflow |
| 005.4-E2E-004 | E2E | P2 | LLM response content is consistent across multiple runs | Deep determinism check |

### AC-7: No Windows Defender blocking (or documented workaround)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-E2E-005 | E2E | P0 | TEA binary runs on fresh Windows VM without manual intervention | Critical deployment path |
| 005.4-E2E-006 | E2E | P1 | Documentation for Defender exception process exists and is accessible | User experience |
| 005.4-INT-014 | Integration | P2 | Workflow logs capture any Defender-related warnings | Observability |

### AC-8: No macOS Gatekeeper blocking (or documented workaround)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-E2E-007 | E2E | P0 | TEA binary runs after `xattr -d com.apple.quarantine` | Critical deployment path |
| 005.4-E2E-008 | E2E | P1 | Workflow includes quarantine removal step | Automation coverage |
| 005.4-INT-015 | Integration | P2 | `xattr` command succeeds or gracefully fails on clean binary | Idempotent operation |

### AC-9: Platform-specific quirks documented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-E2E-009 | E2E | P1 | `docs/shared/ape-platform-notes.md` exists | Documentation completeness |
| 005.4-E2E-010 | E2E | P2 | Documentation covers Windows quirks section | Content verification |
| 005.4-E2E-011 | E2E | P2 | Documentation covers macOS quirks section | Content verification |
| 005.4-E2E-012 | E2E | P3 | Documentation covers Linux distro variations | Content verification |

### AC-10: CI workflow runs full test matrix on release

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-016 | Integration | P0 | Workflow file `.github/workflows/test-ape-matrix.yaml` exists and is valid YAML | CI infrastructure |
| 005.4-E2E-013 | E2E | P0 | Workflow_call successfully triggers with artifact input | Reusable workflow validation |
| 005.4-E2E-014 | E2E | P1 | All matrix jobs complete successfully in dry-run | Full matrix coverage |
| 005.4-E2E-015 | E2E | P3 | Workflow integrates with build-ape.yaml properly | CI pipeline integration |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Binary incompatible with older Ubuntu | Medium | High | 005.4-INT-001, 005.4-INT-002, 005.4-INT-003 |
| Windows Defender blocks execution | High | High | 005.4-E2E-005, 005.4-E2E-006, 005.4-INT-014 |
| macOS Gatekeeper quarantine | High | High | 005.4-E2E-007, 005.4-E2E-008, 005.4-INT-015 |
| Prolog output varies by platform | Low | Critical | 005.4-UNIT-001 through 003, 005.4-E2E-001 |
| Lua output varies by platform | Low | Critical | 005.4-UNIT-004 through 006, 005.4-E2E-002 |
| LLM non-deterministic despite seed | Medium | Medium | 005.4-INT-012, 005.4-INT-013, 005.4-E2E-003 |
| CI workflow misconfigured | Medium | High | 005.4-INT-016, 005.4-E2E-013, 005.4-E2E-014 |

---

## Test Implementation Notes

### Prolog Test Agent (`tests/ape/prolog-test.yaml`)

```yaml
# Key verification: factorial(5) = 120
# Platform detection via Python platform.system()
# Expected result format: "PASS: factorial(5) = 120 on {platform}"
```

### Lua Test Agent (`tests/ape/lua-test.yaml`)

```yaml
# Key verification: fibonacci(10) = 55
# Pure Lua execution without Python interop
# Expected result format: "PASS: fibonacci(10) = 55"
```

### LLM Test Agent (`tests/ape/llm-test.yaml`)

```yaml
# Settings: temperature=0, seed=42
# Verify non-empty response
# Max tokens: 50 (short for consistency)
```

### Cross-Platform Hash Verification

For E2E tests 005.4-E2E-001 and 005.4-E2E-002, compute SHA-256 of final state output and compare across platforms:

```bash
# Linux
./tea.com run tests/ape/prolog-test.yaml | sha256sum

# Windows (PowerShell)
.\tea.com run tests/ape/prolog-test.yaml | Get-FileHash -Algorithm SHA256

# macOS
./tea.com run tests/ape/prolog-test.yaml | shasum -a 256
```

---

## Recommended Execution Order

1. **P0 Unit tests** (005.4-UNIT-001 through 006) - Fail fast on calculation errors
2. **P0 Integration tests** (005.4-INT-001 through 011, 016) - Validate platform compatibility
3. **P0 E2E tests** (005.4-E2E-001, 002, 005, 007, 013) - Critical user journeys
4. **P1 tests** in order - Core functionality validation
5. **P2+ tests** as time permits - Documentation and edge cases

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 6
    integration: 16
    e2e: 12
  by_priority:
    p0: 14
    p1: 12
    p2: 6
    p3: 2
  coverage_gaps: []
  design_date: "2026-01-12"
  designer: "Quinn (Test Architect)"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-005.4-test-design-20260112.md
P0 tests identified: 14
P1 tests identified: 12
```
