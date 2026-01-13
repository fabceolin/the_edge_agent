# Test Design: Story TEA-REPORT-001e

**Date:** 2026-01-11
**Designer:** Quinn (Test Architect)
**Story:** Cross-Runtime Parity Tests
**Parent Epic:** TEA-REPORT-001 (Automatic Bug Reporting System)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 24 |
| **Unit tests** | 8 (33%) |
| **Integration tests** | 10 (42%) |
| **E2E tests** | 6 (25%) |
| **P0 (Critical)** | 10 |
| **P1 (High)** | 9 |
| **P2 (Medium)** | 5 |

### Strategy Rationale

This story is fundamentally about **cross-runtime parity** - ensuring Rust and Python produce byte-identical output. The primary risk is undetected parity drift causing the web viewer to fail for one runtime's URLs. Test strategy emphasizes:

1. **Layered encoding verification** - Test each encoding stage (VLQ, compression, Base64url) independently before full URL tests
2. **Round-trip validation** - Verify encoded URLs decode correctly via JavaScript decoder
3. **CI integration** - Automated parity checks on every PR to prevent regression
4. **Fixture-driven testing** - Deterministic inputs for reproducible results

---

## Test Scenarios by Acceptance Criteria

### AC-25: Rust and Python produce identical URLs for equivalent errors

**Requirement:** Given the same ErrorReport JSON input, both Rust and Python encoders must produce byte-identical URLs.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001e-UNIT-001 | Unit | P0 | VLQ encoding produces identical bytes for boundary values (0, 127, 128, 255, 16383, 16384) | VLQ is foundational - mismatch here cascades to all downstream encoding |
| 001e-UNIT-002 | Unit | P0 | VLQ encoding handles large integers (2^20, 2^30) identically | Edge case: Large stack addresses use high integers |
| 001e-UNIT-003 | Unit | P0 | Base64url encoding uses identical alphabet and padding | Final encoding step - must be identical for URL validity |
| 001e-UNIT-004 | Unit | P1 | JSON serialization produces identical key ordering | Dict ordering affects compression output |
| 001e-INT-001 | Integration | P0 | Deflate compression (level 9) produces identical bytes | Same JSON → same compressed bytes is critical for parity |
| 001e-INT-002 | Integration | P0 | Full URL generation matches for `panic_simple.json` fixture | Core parity test with minimal input |
| 001e-INT-003 | Integration | P0 | Full URL generation matches for `panic_with_stack.json` fixture | Tests multi-frame stack serialization parity |
| 001e-INT-004 | Integration | P1 | Full URL generation matches for `yaml_error.json` fixture | Tests YAML-specific error type handling |
| 001e-INT-005 | Integration | P1 | Full URL generation matches for `executor_error.json` fixture | Tests executor error context parity |
| 001e-INT-006 | Integration | P1 | Full URL generation matches for `extended_context.json` fixture | Tests complex nested context serialization |

### AC-26: Same error types have same capture structure

**Requirement:** ErrorReport structure (fields, serialization order) must be identical across runtimes.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001e-UNIT-005 | Unit | P1 | ErrorReport schema has identical required fields in Rust and Python | Schema divergence causes decode failures |
| 001e-UNIT-006 | Unit | P1 | Stack frame structure matches (addr, symbol, file, line) | Frame serialization must be byte-identical |
| 001e-INT-007 | Integration | P1 | ErrorReport from Panic error has same JSON structure in both runtimes | Validates Panic error type consistency |
| 001e-INT-008 | Integration | P2 | ErrorReport from YamlError has same JSON structure | Validates YAML error type consistency |
| 001e-INT-009 | Integration | P2 | ErrorReport from ExecutorError has same JSON structure | Validates executor error type consistency |

### AC-27: Integration tests verify cross-runtime parity

**Requirement:** End-to-end tests verify that URLs from either runtime decode correctly via the JavaScript decoder.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001e-E2E-001 | E2E | P0 | Rust URL decodes correctly via JavaScript decoder | Critical: Production viewer uses JS decoder |
| 001e-E2E-002 | E2E | P0 | Python URL decodes correctly via JavaScript decoder | Critical: Production viewer uses JS decoder |
| 001e-E2E-003 | E2E | P0 | Round-trip: Rust encode → JS decode → verify fields match original | Full chain validation |
| 001e-E2E-004 | E2E | P0 | Round-trip: Python encode → JS decode → verify fields match original | Full chain validation |
| 001e-E2E-005 | E2E | P1 | CI workflow runs parity tests on every PR | Prevents parity regression |
| 001e-E2E-006 | E2E | P2 | Pre-commit hook validates parity (optional) | Developer convenience |

---

## Additional Test Scenarios (Derived from Tasks)

### Task Coverage: Test Infrastructure (Tasks 1.1-1.3)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001e-UNIT-007 | Unit | P2 | Helper script `rust_encoder.sh` returns valid URL for valid input | Validates test infrastructure |
| 001e-UNIT-008 | Unit | P2 | Helper script `python_encoder.sh` returns valid URL for valid input | Validates test infrastructure |
| 001e-INT-010 | Integration | P2 | Helper script `js_decoder.mjs` parses all fixture URLs | Validates JS decoder tool |

---

## Test Scenario Details

### 001e-UNIT-001: VLQ Boundary Value Encoding

```yaml
test_scenario:
  id: '001e-UNIT-001'
  requirement: 'AC-25'
  priority: P0
  level: unit
  description: 'VLQ encoding produces identical bytes for boundary values'
  justification: 'VLQ is the lowest-level encoding; any mismatch cascades to all higher levels'
  test_values: [0, 1, 127, 128, 255, 256, 16383, 16384]
  expected_behavior: 'Rust vlq_encode(n) == Python vlq_encode(n) for all test values'
  mitigates_risks: ['Parity break undetected']
```

### 001e-INT-002: Full URL Parity (panic_simple)

```yaml
test_scenario:
  id: '001e-INT-002'
  requirement: 'AC-25'
  priority: P0
  level: integration
  description: 'Full URL generation matches for panic_simple.json fixture'
  justification: 'Core parity test with minimal ErrorReport - catches fundamental encoding issues'
  fixture: 'fixtures/panic_simple.json'
  expected_behavior: 'get_rust_url(fixture) == get_python_url(fixture)'
  mitigates_risks: ['Parity break undetected', 'CI environment differences']
```

### 001e-E2E-003: Rust Round-Trip Validation

```yaml
test_scenario:
  id: '001e-E2E-003'
  requirement: 'AC-27'
  priority: P0
  level: e2e
  description: 'Round-trip: Rust encode → JS decode → verify fields match original'
  justification: 'Validates the full production path: Rust CLI → web viewer'
  steps:
    - Load fixture JSON
    - Generate URL via Rust encoder
    - Decode URL via JavaScript decoder
    - Compare decoded object to original fixture
  expected_behavior: 'All fields in decoded object match original fixture exactly'
  mitigates_risks: ['Parity break undetected']
```

---

## Risk Coverage

| Risk (from story) | Impact | Test Coverage |
|-------------------|--------|---------------|
| CI environment differences | Medium | 001e-E2E-005 (workflow), 001e-INT-* (deterministic fixtures) |
| Test flakiness | Low | All tests use deterministic fixtures, no random data |
| Parity break undetected | High | 001e-INT-002 through 001e-INT-006, 001e-E2E-001 through 001e-E2E-004 |

---

## Recommended Execution Order

### Fast Feedback (Fail Fast)

1. **P0 Unit tests** - VLQ and Base64url encoding (< 1s)
   - 001e-UNIT-001, 001e-UNIT-002, 001e-UNIT-003

2. **P0 Integration tests** - Compression and URL parity (< 5s)
   - 001e-INT-001, 001e-INT-002, 001e-INT-003

3. **P0 E2E tests** - JavaScript decoder round-trip (< 10s)
   - 001e-E2E-001, 001e-E2E-002, 001e-E2E-003, 001e-E2E-004

### Standard Coverage

4. **P1 tests** - Structure parity, additional fixtures
   - 001e-UNIT-004, 001e-UNIT-005, 001e-UNIT-006
   - 001e-INT-004, 001e-INT-005, 001e-INT-006, 001e-INT-007
   - 001e-E2E-005

### Extended Coverage

5. **P2 tests** - Infrastructure validation, optional hooks
   - 001e-UNIT-007, 001e-UNIT-008
   - 001e-INT-008, 001e-INT-009, 001e-INT-010
   - 001e-E2E-006

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-REPORT-001e
  date: '2026-01-11'
  scenarios_total: 24
  by_level:
    unit: 8
    integration: 10
    e2e: 6
  by_priority:
    p0: 10
    p1: 9
    p2: 5
  coverage_gaps: []
  ac_coverage:
    AC-25: 10 scenarios
    AC-26: 5 scenarios
    AC-27: 6 scenarios
  risks_mitigated:
    - 'Parity break undetected'
    - 'CI environment differences'
    - 'Test flakiness'
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-REPORT-001e-test-design-20260111.md
P0 tests identified: 10
Acceptance criteria coverage: 3/3 (100%)
Risk mitigation coverage: 3/3 (100%)
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention ({epic}.{story}-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Execution order optimizes for fast feedback

---

## Implementation Notes

### Key Dependencies Between Runtimes

The parity tests require **both** runtimes to be built and available:

```bash
# Required before running tests
cd rust && cargo build
cd python && pip install -e .
```

### Fixture Design Principles

1. **Deterministic** - No timestamps, random IDs, or variable content
2. **Representative** - Cover all error types from dependent stories
3. **Minimal** - Start simple (panic_simple), add complexity incrementally
4. **Documented** - Each fixture documents expected encoding behavior

### Debugging Parity Failures

The story includes a debug script pattern. Recommended debugging order:

1. Compare VLQ encoding for each integer in the input
2. Compare JSON serialization (key order matters for compression)
3. Compare compressed byte lengths
4. Find first byte difference using provided debug function

---

**Document Version:** 1.0
**Generated by:** Quinn (Test Architect)
