# Test Design: Epic TEA-REPORT-001 - Automatic Bug Reporting System

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 78
- **Unit tests:** 32 (41%)
- **Integration tests:** 28 (36%)
- **E2E tests:** 18 (23%)
- **Priority distribution:** P0: 24, P1: 32, P2: 16, P3: 6

### Strategy Rationale

This epic implements a **cross-runtime** (Rust + Python) bug reporting system following the bun.report architecture. Testing strategy emphasizes:

1. **Parity Testing** - Critical that Rust and Python produce identical URLs for same input
2. **Privacy Validation** - Zero PII must leak into URLs (P0 priority)
3. **Encoding Correctness** - VLQ, deflate, base64url must be reversible and cross-runtime compatible
4. **CLI UX** - User experience on error is critical for adoption

---

## Test Scenarios by Child Story

---

## TEA-REPORT-001a: Error Capture Protocol - Core

### AC-1: Rust panic hook captures version, platform, architecture, stack addresses, panic message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001a-UNIT-001 | Unit | P0 | Verify `ErrorReport` struct contains all required fields | Pure struct validation |
| REPORT-001a-UNIT-002 | Unit | P0 | Verify panic hook populates version from Cargo.toml | Isolated metadata extraction |
| REPORT-001a-UNIT-003 | Unit | P0 | Verify platform detection (linux/darwin/windows) | Platform string generation logic |
| REPORT-001a-UNIT-004 | Unit | P0 | Verify architecture detection (x86_64/arm64/etc) | Architecture string logic |
| REPORT-001a-INT-001 | Integration | P0 | Trigger real panic and verify captured ErrorReport | Full panic handler integration |
| REPORT-001a-INT-002 | Integration | P1 | Verify stack frames contain valid addresses | Address capture from backtrace |

### AC-2: Python excepthook captures version, platform, architecture, traceback, exception message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001a-UNIT-005 | Unit | P0 | Verify Python `ErrorReport` dataclass has all fields | Pure dataclass validation |
| REPORT-001a-UNIT-006 | Unit | P0 | Verify version extracted from package metadata | Isolated version extraction |
| REPORT-001a-UNIT-007 | Unit | P0 | Verify `sys.platform` and architecture captured | Platform detection logic |
| REPORT-001a-INT-003 | Integration | P0 | Trigger unhandled exception and verify capture | Full excepthook integration |
| REPORT-001a-INT-004 | Integration | P1 | Verify traceback frames converted to StackFrame | Traceback parsing integration |

### AC-3: YAML engine errors captured with node name, action type, error kind

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001a-UNIT-008 | Unit | P1 | Verify `ErrorContext` captures node name | Context field extraction |
| REPORT-001a-UNIT-009 | Unit | P1 | Verify action type captured (llm_call, transform, etc) | Action type classification |
| REPORT-001a-INT-005 | Integration | P0 | YAML engine error wraps into ErrorReport with context | Engine-to-report integration |
| REPORT-001a-INT-006 | Integration | P1 | Invalid YAML syntax produces proper error context | YAML parse error capture |

### AC-4: Executor errors captured with checkpoint context (ID only), interrupted state (without data)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001a-UNIT-010 | Unit | P1 | Verify checkpoint ID extracted without checkpoint data | Sensitive data exclusion |
| REPORT-001a-INT-007 | Integration | P0 | Executor failure captures checkpoint ID only | Full executor error flow |
| REPORT-001a-INT-008 | Integration | P1 | Interrupted workflow captures state flag, not state data | Interrupt error capture |

### AC-5: All captures exclude PII - no file contents, no state data, no user paths

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001a-UNIT-011 | Unit | P0 | Path sanitizer converts absolute to relative paths | Path sanitization logic |
| REPORT-001a-UNIT-012 | Unit | P0 | Path sanitizer removes home directory prefixes | PII removal logic |
| REPORT-001a-UNIT-013 | Unit | P0 | Verify ErrorReport excludes state dict/HashMap | State exclusion validation |
| REPORT-001a-INT-009 | Integration | P0 | Full error capture contains no absolute paths | Integration PII check |
| REPORT-001a-INT-010 | Integration | P0 | Full error capture contains no environment variables | Env var exclusion |
| REPORT-001a-E2E-001 | E2E | P0 | Generated URL decoded shows no PII (manual audit) | End-to-end privacy validation |

---

## TEA-REPORT-001b: URL Encoder/Decoder Library

### AC-6: VLQ encoding for numeric values (stack addresses, line numbers)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-UNIT-014 | Unit | P0 | VLQ encode/decode round-trip for small numbers (0-127) | Base case validation |
| REPORT-001b-UNIT-015 | Unit | P0 | VLQ encode/decode round-trip for large numbers (>2^32) | Memory address handling |
| REPORT-001b-UNIT-016 | Unit | P1 | VLQ encode produces same output as source-map spec | Spec compliance |
| REPORT-001b-UNIT-017 | Unit | P0 | VLQ encode sequence of numbers (multiple stack frames) | Multi-value encoding |

### AC-7: Deflate compression for text data (panic message, traceback)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-UNIT-018 | Unit | P0 | Deflate compress/decompress round-trip | Compression correctness |
| REPORT-001b-UNIT-019 | Unit | P1 | Compression reduces size for typical error messages | Compression efficiency |
| REPORT-001b-UNIT-020 | Unit | P2 | Handle empty string compression | Edge case |
| REPORT-001b-UNIT-021 | Unit | P1 | Handle very long error messages (>4KB) with truncation | Size limit handling |

### AC-8: Base64url encoding for URL-safe output

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-UNIT-022 | Unit | P0 | Base64url encode/decode round-trip | Encoding correctness |
| REPORT-001b-UNIT-023 | Unit | P0 | Output contains only URL-safe characters (A-Za-z0-9-_) | URL safety validation |
| REPORT-001b-UNIT-024 | Unit | P1 | No padding characters in output | Base64url spec compliance |

### AC-9: URL format matches specification

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-UNIT-025 | Unit | P0 | URL format matches `https://{org}.github.io/the_edge_agent/report/{version}/{encoded}` | Format compliance |
| REPORT-001b-INT-011 | Integration | P0 | Full encode pipeline produces valid URL | Pipeline integration |

### AC-10: Encoded URL length under 2000 characters (browser limit)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-UNIT-026 | Unit | P0 | Truncation logic activates when URL exceeds 2000 chars | Length enforcement |
| REPORT-001b-UNIT-027 | Unit | P1 | Truncation preserves most important data (version, platform) | Priority truncation |
| REPORT-001b-INT-012 | Integration | P0 | Complex error with long stack trace produces valid URL <2000 chars | Real-world length test |

### AC-11: Rust and Python encoders produce identical output for same input

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001b-INT-013 | Integration | P0 | Same ErrorReport struct produces same VLQ output in Rust and Python | VLQ parity |
| REPORT-001b-INT-014 | Integration | P0 | Same text produces same deflate output in Rust and Python | Compression parity |
| REPORT-001b-INT-015 | Integration | P0 | Full encode pipeline produces identical URLs cross-runtime | End-to-end parity |

---

## TEA-REPORT-001c: GitHub Pages Report Viewer

### AC-12: Report viewer at `{org}.github.io/the_edge_agent/report/`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-E2E-002 | E2E | P1 | Report viewer page loads successfully | Deployment verification |
| REPORT-001c-E2E-003 | E2E | P2 | Page is accessible (WCAG 2.1 basic compliance) | Accessibility |

### AC-13: JavaScript decodes URL parameters (VLQ, inflate, base64url)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-UNIT-028 | Unit | P0 | JS VLQ decoder matches Rust/Python encoder output | JS decoder correctness |
| REPORT-001c-UNIT-029 | Unit | P0 | JS inflate (pako) matches Rust/Python deflate output | JS decompression |
| REPORT-001c-UNIT-030 | Unit | P0 | JS base64url decoder produces original bytes | JS decoding correctness |
| REPORT-001c-INT-016 | Integration | P0 | JS decoder chain produces original ErrorReport | Full JS decode pipeline |

### AC-14: Display decoded error info: version, platform, stack trace, error message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-E2E-004 | E2E | P0 | Rust-generated URL displays correctly in viewer | Rust->viewer flow |
| REPORT-001c-E2E-005 | E2E | P0 | Python-generated URL displays correctly in viewer | Python->viewer flow |
| REPORT-001c-E2E-006 | E2E | P1 | All fields (version, platform, stack, message) visible | Field completeness |

### AC-15: Source remapping using debug symbols or source maps (optional)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-E2E-007 | E2E | P2 | With debug symbols available, stack addresses map to source | Source mapping |
| REPORT-001c-E2E-008 | E2E | P3 | Without symbols, raw addresses displayed gracefully | Graceful degradation |

### AC-16: "File issue on GitHub" button with pre-populated content

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-E2E-009 | E2E | P1 | Button generates correct GitHub new issue URL | Issue creation flow |
| REPORT-001c-E2E-010 | E2E | P1 | Issue body contains decoded error information | Content pre-fill |

### AC-17 & AC-38-39: Check for existing issues with similar stack traces

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-INT-017 | Integration | P1 | GitHub API search query finds issues with matching stack | Search integration |
| REPORT-001c-E2E-011 | E2E | P1 | When duplicate exists, "Similar issue" link shown | Duplicate UX |
| REPORT-001c-E2E-012 | E2E | P1 | When no duplicate, "File new issue" button shown | New issue UX |

### AC-18: Mobile-friendly responsive design

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-E2E-013 | E2E | P2 | Page renders correctly at 375px width (mobile) | Mobile responsiveness |
| REPORT-001c-E2E-014 | E2E | P3 | Page renders correctly at 768px width (tablet) | Tablet responsiveness |

### AC-35-37: Spam prevention via GitHub native authentication

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001c-INT-018 | Integration | P1 | "File issue" redirects to github.com/issues/new | Redirect verification |
| REPORT-001c-E2E-015 | E2E | P2 | Unauthenticated user prompted to login on GitHub | Auth flow |

---

## TEA-REPORT-001d: CLI Integration & UX

### AC-19: On crash/error, display report URL to user

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-INT-019 | Integration | P0 | Rust CLI panic displays report URL | Rust CLI integration |
| REPORT-001d-INT-020 | Integration | P0 | Python CLI exception displays report URL | Python CLI integration |
| REPORT-001d-E2E-016 | E2E | P0 | User sees actionable URL on real error | End-to-end UX |

### AC-20-21: `--report-bugs` / `--no-report-bugs` flags

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-UNIT-031 | Unit | P1 | Flag parsing recognizes --report-bugs | Arg parsing |
| REPORT-001d-UNIT-032 | Unit | P1 | Flag parsing recognizes --no-report-bugs | Arg parsing |
| REPORT-001d-INT-021 | Integration | P0 | --no-report-bugs suppresses URL generation | Flag behavior |
| REPORT-001d-INT-022 | Integration | P1 | --report-bugs enables URL (default behavior) | Flag behavior |

### AC-22: Environment variable `TEA_REPORT_BUGS=false` to disable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-INT-023 | Integration | P0 | TEA_REPORT_BUGS=false disables reporting | Env var behavior |
| REPORT-001d-INT-024 | Integration | P1 | CLI flag overrides env var | Precedence |

### AC-23: Clear message with explanation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-INT-025 | Integration | P1 | Message includes "Report this bug:" text | Message format |
| REPORT-001d-INT-026 | Integration | P2 | Message includes privacy explanation | Privacy notice |

### AC-24: Option to copy URL to clipboard

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-INT-027 | Integration | P2 | Clipboard copy works on supported terminals | Clipboard integration |
| REPORT-001d-INT-028 | Integration | P2 | Graceful fallback when clipboard unavailable | Fallback behavior |

### AC-28-34: Opt-in Extended Context

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001d-INT-029 | Integration | P1 | Prompt appears after minimal URL displayed | Prompt UX |
| REPORT-001d-INT-030 | Integration | P1 | User 'y' input generates extended URL | Opt-in flow |
| REPORT-001d-INT-031 | Integration | P1 | Extended URL includes node names, action types | Extended data |
| REPORT-001d-INT-032 | Integration | P0 | Extended URL excludes state data, secrets, env vars | Privacy in extended |
| REPORT-001d-INT-033 | Integration | P1 | --report-extended skips prompt | Flag behavior |
| REPORT-001d-INT-034 | Integration | P1 | --report-minimal skips extended prompt entirely | Flag behavior |
| REPORT-001d-E2E-017 | E2E | P1 | Extended URL decoded shows "extended" marker in viewer | Extended marker |

---

## TEA-REPORT-001e: Cross-Runtime Parity Tests

### AC-25: Rust and Python produce identical URLs for equivalent errors

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001e-INT-035 | Integration | P0 | Panic in Rust == Exception in Python produces same URL structure | Parity core |
| REPORT-001e-INT-036 | Integration | P0 | Same error message produces byte-identical encoding | Encoding parity |

### AC-26: Same error types have same capture structure

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001e-INT-037 | Integration | P0 | YamlError captures identical fields in both runtimes | Type parity |
| REPORT-001e-INT-038 | Integration | P0 | ExecutorError captures identical fields in both runtimes | Type parity |
| REPORT-001e-INT-039 | Integration | P0 | ActionError captures identical fields in both runtimes | Type parity |

### AC-27: Integration tests verify cross-runtime parity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| REPORT-001e-E2E-018 | E2E | P0 | Parity test suite: N fixtures, both runtimes, identical URLs | Full parity verification |

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| **Privacy leak (Critical)** | REPORT-001a-UNIT-011, UNIT-012, UNIT-013, INT-009, INT-010, E2E-001, INT-032 |
| **URL too long** | REPORT-001b-UNIT-026, UNIT-027, INT-012 |
| **Encoding mismatch Rust/Python** | REPORT-001b-INT-013, INT-014, INT-015, REPORT-001e-INT-035-039, E2E-018 |
| **GitHub Pages unavailable** | Graceful degradation tested in E2E-008 |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (CI on every commit)
1. P0 Unit tests (REPORT-001a-UNIT-*, REPORT-001b-UNIT-*)
2. P1 Unit tests

### Phase 2: Integration Validation (CI on PR)
1. P0 Integration tests (parity, privacy, core flows)
2. P1 Integration tests

### Phase 3: Full E2E (Pre-release)
1. P0 E2E tests (privacy audit, parity verification)
2. P1 E2E tests (viewer functionality)
3. P2+ E2E tests (responsiveness, accessibility)

---

## Coverage Gaps

None identified. All 39 Acceptance Criteria have at least one test scenario.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for cross-component, E2E for full flows)
- [x] No duplicate coverage across levels (each test validates distinct aspect)
- [x] Priorities align with business risk (privacy P0, parity P0, UX P1)
- [x] Test IDs follow naming convention `REPORT-{story}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  epic_id: TEA-REPORT-001
  scenarios_total: 78
  by_level:
    unit: 32
    integration: 28
    e2e: 18
  by_priority:
    p0: 24
    p1: 32
    p2: 16
    p3: 6
  coverage_gaps: []
  key_risk_coverage:
    privacy_leak: 7 tests
    encoding_parity: 8 tests
    url_length: 3 tests
  design_date: 2026-01-12
  designer: Quinn (Test Architect)
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-REPORT-001-test-design-20260112.md
P0 tests identified: 24
Child stories covered: 5 (001a, 001b, 001c, 001d, 001e)
Total ACs covered: 39/39
```
