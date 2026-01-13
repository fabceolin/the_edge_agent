# Test Design: TEA-RELEASE-005 (APE Cosmopolitan Distribution Epic)

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 14 (30%)
- **Integration tests:** 18 (38%)
- **E2E tests:** 15 (32%)
- **Priority distribution:** P0: 18, P1: 16, P2: 10, P3: 3

## Risk Assessment Summary

| Risk ID | Risk | Test Coverage |
|---------|------|---------------|
| RISK-001 | Scryer Prolog incompatible with SWI syntax | INT-003, INT-004, E2E-003 |
| RISK-002 | Cosmopolitan Rust target experimental | INT-007, INT-008, E2E-006 |
| RISK-003 | Windows Defender false positive | E2E-009, E2E-010 |
| RISK-004 | macOS Gatekeeper blocking | E2E-011, E2E-012 |
| RISK-005 | WASM Scryer spike fails | INT-017, INT-018 |

---

## Test Scenarios by Story

### Story 005.1: Scryer Prolog Integration Spike

#### AC: Add `scryer-prolog` crate as optional dependency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-001 | Unit | P0 | Verify `--features scryer` compiles successfully | Build validation - pure compile logic |
| 005.1-UNIT-002 | Unit | P1 | Verify feature flag isolation (scryer vs default) | Conditional compilation correctness |
| 005.1-INT-001 | Integration | P0 | Verify scryer-prolog crate loads at runtime | Crate integration point |

#### AC: Implement `PrologBackend` trait for Scryer

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-UNIT-003 | Unit | P0 | Test PrologBackend trait method signatures | API contract validation |
| 005.1-UNIT-004 | Unit | P0 | Test Scryer query execution (simple fact) | Core logic validation |
| 005.1-UNIT-005 | Unit | P1 | Test Scryer query execution (rule with variables) | Complex Prolog logic |
| 005.1-INT-002 | Integration | P0 | Test PrologBackend polymorphism (swap implementations) | Trait-based dispatch |
| 005.1-INT-003 | Integration | P0 | Test SWI-to-Scryer syntax differences handling | Risk mitigation RISK-001 |

#### AC: Port simple-prolog-agent.yaml to Scryer syntax

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-INT-004 | Integration | P0 | Test simple-prolog-agent.yaml with Scryer backend | Full agent execution |
| 005.1-E2E-001 | E2E | P1 | Run ported agent end-to-end with Scryer | User journey validation |

#### AC: Benchmark startup time and query performance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.1-INT-005 | Integration | P1 | Measure Scryer startup time (<2s threshold) | Performance requirement |
| 005.1-INT-006 | Integration | P2 | Compare Scryer vs SWI query performance | Informational benchmark |

---

### Story 005.2: Cosmopolitan Build Pipeline

#### AC: Install Cosmopolitan toolchain in CI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-INT-007 | Integration | P0 | Verify cosmocc installs correctly in GitHub Actions | CI pipeline dependency |
| 005.2-UNIT-006 | Unit | P2 | Validate cosmocc version detection script | Utility script |

#### AC: Configure Rust to target `x86_64-unknown-cosmo`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-INT-008 | Integration | P0 | Verify Rust cross-compilation to Cosmo target | Risk mitigation RISK-002 |
| 005.2-UNIT-007 | Unit | P1 | Test Cargo.toml target configuration parsing | Build config validation |

#### AC: Build `tea.com` with Scryer Prolog + Lua

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-INT-009 | Integration | P0 | Verify tea.com binary is generated | Core deliverable |
| 005.2-INT-010 | Integration | P0 | Verify tea.com file size within 10% of native | Success criteria |
| 005.2-E2E-002 | E2E | P0 | Execute tea.com --version on Linux | Basic smoke test |

#### AC: Smoke test on Linux, Windows, macOS runners

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.2-E2E-003 | E2E | P0 | Run tea.com simple Prolog query on Linux | Cross-platform validation |
| 005.2-E2E-004 | E2E | P0 | Run tea.com simple Prolog query on Windows | Cross-platform validation |
| 005.2-E2E-005 | E2E | P0 | Run tea.com simple Prolog query on macOS | Cross-platform validation |
| 005.2-E2E-006 | E2E | P0 | Run tea.com Lua script on all platforms | Multi-runtime validation |

---

### Story 005.3: LLM Model Bundling via ZIP Append

#### AC: Implement `/zip/` virtual filesystem access

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-008 | Unit | P0 | Test /zip/ path resolution logic | File system abstraction |
| 005.3-UNIT-009 | Unit | P1 | Test /zip/ read for model bytes | Data access layer |
| 005.3-INT-011 | Integration | P0 | Verify model file accessible via /zip/ | Virtual FS integration |

#### AC: Create build script to append Phi-4-mini via ZIP

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-INT-012 | Integration | P0 | Verify ZIP append produces valid archive | Build artifact integrity |
| 005.3-INT-013 | Integration | P1 | Verify tea-llm.com size ~2GB | Size expectation |
| 005.3-UNIT-010 | Unit | P2 | Test ZIP append script with mock model | Script logic validation |

#### AC: Modify `llm.call` action to detect bundled model

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.3-UNIT-011 | Unit | P0 | Test bundled model detection logic | Core feature logic |
| 005.3-INT-014 | Integration | P0 | Test llm.call with bundled model path | Action integration |
| 005.3-E2E-007 | E2E | P0 | Run LLM inference with tea-llm.com on Linux | Critical user journey |
| 005.3-E2E-008 | E2E | P1 | Run LLM inference with tea-llm.com on Windows | Cross-platform validation |

---

### Story 005.4: Platform Testing & Validation

#### AC: Matrix test on Ubuntu, Windows, macOS variants

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-E2E-009 | E2E | P0 | tea.com on Ubuntu 20.04 | Minimum supported version |
| 005.4-E2E-010 | E2E | P1 | tea.com on Ubuntu 22.04/24.04 | Current LTS versions |
| 005.4-E2E-011 | E2E | P0 | tea.com on Windows 10 (with Defender check) | Risk mitigation RISK-003 |
| 005.4-E2E-012 | E2E | P1 | tea.com on Windows 11 | Current Windows version |
| 005.4-E2E-013 | E2E | P0 | tea.com on macOS 12 (with Gatekeeper check) | Risk mitigation RISK-004 |
| 005.4-E2E-014 | E2E | P1 | tea.com on macOS 13/14 | Current macOS versions |

#### AC: Document platform-specific quirks

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.4-INT-015 | Integration | P2 | Capture and log platform-specific errors | Observability |
| 005.4-UNIT-012 | Unit | P3 | Validate quirks documentation format | Documentation validation |

---

### Story 005.5: Documentation & Migration Guide

#### AC: Update installation docs with APE instructions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-UNIT-013 | Unit | P2 | Validate docs/installation.md contains APE section | Documentation completeness |
| 005.5-INT-016 | Integration | P2 | Follow APE installation steps and verify success | User journey validation |

#### AC: Create Scryer migration guide

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.5-UNIT-014 | Unit | P2 | Validate scryer-migration.md covers key syntax differences | Documentation completeness |
| 005.5-E2E-015 | E2E | P2 | User follows migration guide and converts SWI agent | User journey validation |

---

### Story 005.6: Scryer Prolog WASM Spike

#### AC: Attempt WASM compilation of Scryer

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 005.6-INT-017 | Integration | P2 | Compile scryer-prolog to wasm32-unknown-unknown | Spike validation |
| 005.6-INT-018 | Integration | P3 | Run minimal Prolog query in WASM (if compiles) | Spike success path |
| 005.6-UNIT-015 | Unit | P3 | Document blocking issues from compilation output | Spike documentation |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Test Scenarios | Mitigation Confidence |
|---------|------------------|----------------|----------------------|
| RISK-001 | Scryer Prolog incompatibility | 005.1-INT-003, 005.1-INT-004, 005.5-E2E-015 | High |
| RISK-002 | Cosmopolitan Rust experimental | 005.2-INT-007, 005.2-INT-008, 005.2-E2E-002 | Medium |
| RISK-003 | Windows Defender false positive | 005.4-E2E-011 | Medium |
| RISK-004 | macOS Gatekeeper blocking | 005.4-E2E-013 | Medium |
| RISK-005 | WASM Scryer spike fails | 005.6-INT-017, 005.6-INT-018 | Low (informational) |

---

## Recommended Execution Order

### Phase 1: Fail-Fast (P0 Unit + Integration)
1. 005.1-UNIT-001 through 005.1-UNIT-005 (Scryer basics)
2. 005.3-UNIT-008, 005.3-UNIT-011 (/zip/ and bundled model detection)
3. 005.1-INT-001 through 005.1-INT-004 (Prolog integration)
4. 005.2-INT-007 through 005.2-INT-010 (Cosmo build)
5. 005.3-INT-011 through 005.3-INT-014 (LLM bundling)

### Phase 2: Cross-Platform E2E (P0 E2E)
1. 005.2-E2E-002 through 005.2-E2E-006 (Platform smoke tests)
2. 005.3-E2E-007 (LLM inference Linux)
3. 005.4-E2E-009, 005.4-E2E-011, 005.4-E2E-013 (Min versions)

### Phase 3: Comprehensive (P1)
1. All remaining P1 tests in order
2. Additional platform matrix tests

### Phase 4: Nice-to-Have (P2+)
1. P2 documentation and benchmark tests
2. P3 spike and documentation validation

---

## Test Environment Requirements

| Environment | Purpose | Configuration |
|-------------|---------|---------------|
| Linux Runner (Ubuntu 20.04) | CI smoke tests | GitHub Actions ubuntu-20.04 |
| Linux Runner (Ubuntu 22.04) | CI validation | GitHub Actions ubuntu-22.04 |
| Windows Runner (2019) | Windows 10 equivalent | GitHub Actions windows-2019 |
| Windows Runner (2022) | Windows 11 equivalent | GitHub Actions windows-2022 |
| macOS Runner (macos-12) | macOS Monterey | GitHub Actions macos-12 |
| macOS Runner (macos-13) | macOS Ventura | GitHub Actions macos-13 |
| Local Dev | Manual testing | Developer machine with all OSes or VMs |

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 14
    integration: 18
    e2e: 15
  by_priority:
    p0: 18
    p1: 16
    p2: 10
    p3: 3
  coverage_gaps: []
  risk_coverage:
    - risk: RISK-001
      tests: [005.1-INT-003, 005.1-INT-004, 005.5-E2E-015]
    - risk: RISK-002
      tests: [005.2-INT-007, 005.2-INT-008, 005.2-E2E-002]
    - risk: RISK-003
      tests: [005.4-E2E-011]
    - risk: RISK-004
      tests: [005.4-E2E-013]
    - risk: RISK-005
      tests: [005.6-INT-017, 005.6-INT-018]
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed
- [x] Cross-platform coverage for APE deliverables

---

## Key Considerations

### Shift-Left Strategy
- Unit tests cover pure logic (PrologBackend trait, /zip/ resolution, model detection)
- Integration tests handle component boundaries (Cosmo toolchain, Scryer crate, ZIP append)
- E2E reserved for critical cross-platform validation (actual binary execution)

### Defense in Depth for P0 Paths
- LLM inference: Unit (detection) + Integration (action call) + E2E (full inference)
- Cross-platform execution: Integration (build) + E2E (each platform)
- Prolog execution: Unit (query) + Integration (agent) + E2E (user journey)

### Spike Tests (Story 005.6)
- Intentionally P2/P3 as WASM spike is informational
- Failure does not block APE delivery
- Success enables future architecture unification

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RELEASE-005-test-design-20260112.md
P0 tests identified: 18
Epic stories covered: 6
Total scenarios: 47
```
