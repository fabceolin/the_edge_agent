# Test Design: Story TEA-REPORT-001d

**Date:** 2026-01-11
**Designer:** Quinn (Test Architect)
**Story:** CLI Integration & UX
**Epic:** TEA-REPORT-001 (Automatic Bug Reporting System)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 42 | 100% |
| **Unit tests** | 18 | 43% |
| **Integration tests** | 16 | 38% |
| **E2E tests** | 8 | 19% |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 12 | Critical functionality |
| **P1** | 18 | Core user journeys |
| **P2** | 10 | Secondary features |
| **P3** | 2 | Nice-to-have |

---

## Test Scenarios by Acceptance Criteria

### AC-19: On crash/error, display report URL to user

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-001 | Unit | P0 | `display_report_url` formats URL correctly | Pure formatting logic |
| 001d-UNIT-002 | Unit | P1 | Report URL is displayed on separate line for easy copy | Output format validation |
| 001d-INT-001 | Integration | P0 | Panic hook triggers URL generation (Rust) | Multi-component: panic hook + capture + encode |
| 001d-INT-002 | Integration | P0 | Excepthook triggers URL generation (Python) | Multi-component: excepthook + capture + encode |
| 001d-INT-003 | Integration | P0 | YAML engine errors generate report URL | Error flow integration |
| 001d-INT-004 | Integration | P0 | Executor errors generate report URL | Error flow integration |
| 001d-E2E-001 | E2E | P0 | User sees report URL when YAML file not found | Critical user-facing error path |
| 001d-E2E-002 | E2E | P0 | User sees report URL on YAML parse error | Critical user-facing error path |

### AC-20: `--report-bugs` flag to enable automatic URL generation (default: enabled)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-003 | Unit | P0 | `--report-bugs` flag defaults to true | Pure flag parsing logic |
| 001d-UNIT-004 | Unit | P1 | Flag is correctly parsed from CLI args | Input parsing validation |
| 001d-INT-005 | Integration | P1 | Default behavior shows URL on error | Component interaction |
| 001d-E2E-003 | E2E | P1 | `tea run broken.yaml` shows report URL with default settings | User journey validation |

### AC-21: `--no-report-bugs` flag to disable (for privacy-conscious users)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-005 | Unit | P0 | `--no-report-bugs` flag is parsed correctly | Pure flag parsing logic |
| 001d-UNIT-006 | Unit | P1 | `--no-report-bugs` conflicts with `--report-bugs` handled | Conflict resolution logic |
| 001d-INT-006 | Integration | P1 | `--no-report-bugs` suppresses URL generation | Flag propagation to handler |
| 001d-E2E-004 | E2E | P1 | `tea run broken.yaml --no-report-bugs` shows no URL | Privacy user journey |

### AC-22: Environment variable `TEA_REPORT_BUGS=false` to disable globally

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-007 | Unit | P1 | Environment variable parsing (true/false/1/0) | Env var interpretation logic |
| 001d-INT-007 | Integration | P1 | `TEA_REPORT_BUGS=false` disables URL generation | Env var to handler flow |
| 001d-INT-008 | Integration | P1 | CLI flag overrides environment variable | Precedence handling |
| 001d-E2E-005 | E2E | P2 | `TEA_REPORT_BUGS=false tea run broken.yaml` shows no URL | Global disable journey |

### AC-23: Clear message with explanation of what's included

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-008 | Unit | P1 | Message includes "version, platform, and stack trace" text | Output content validation |
| 001d-UNIT-009 | Unit | P1 | Message includes "No personal information" disclaimer | Privacy messaging |
| 001d-UNIT-010 | Unit | P2 | Visual separator (━━━) is rendered correctly | Formatting logic |

### AC-24: Option to copy URL to clipboard (if terminal supports it)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-011 | Unit | P2 | `copy_to_clipboard` function succeeds with valid text | Pure clipboard logic |
| 001d-UNIT-012 | Unit | P2 | `copy_to_clipboard` gracefully handles missing clipboard support | Error handling logic |
| 001d-INT-009 | Integration | P2 | Clipboard copy triggered after URL display | Component sequence |
| 001d-INT-010 | Integration | P2 | "Copied to clipboard" message shown on success | Feedback integration |
| 001d-INT-011 | Integration | P2 | No error shown when clipboard unavailable | Graceful degradation |

### AC-28: After displaying minimal URL, prompt: "Include more context? [y/N]"

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-013 | Unit | P1 | `prompt_yes_no` parses 'y'/'Y' as true | Input parsing logic |
| 001d-UNIT-014 | Unit | P1 | `prompt_yes_no` parses 'n'/'N'/Enter as false | Input parsing logic |
| 001d-UNIT-015 | Unit | P1 | `prompt_yes_no` respects timeout (10 seconds) | Timeout logic |
| 001d-INT-012 | Integration | P1 | Prompt displayed after minimal URL | Flow sequencing |
| 001d-E2E-006 | E2E | P1 | User can opt-in to extended context via prompt | Interactive user journey |

### AC-29: If user opts in, generate extended URL with additional data

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-INT-013 | Integration | P1 | Extended URL generated when user types 'y' | User input to generation flow |
| 001d-INT-014 | Integration | P1 | Extended URL is different from minimal URL | Data inclusion verification |

### AC-30: Extended context includes: sanitized YAML structure, node names, action types

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-016 | Unit | P1 | `add_extended_context` includes node names | Data extraction logic |
| 001d-UNIT-017 | Unit | P1 | `add_extended_context` includes action types | Data extraction logic |
| 001d-UNIT-018 | Unit | P1 | `add_extended_context` includes graph structure | Data extraction logic |

### AC-31: Extended context excludes: state data, secrets, environment variables, file contents

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-INT-015 | Integration | P0 | Extended report does NOT contain state data | Security verification |
| 001d-INT-016 | Integration | P0 | Extended report does NOT contain secrets | Security verification |
| 001d-E2E-007 | E2E | P0 | Extended URL verified to exclude sensitive data | Critical security path |

### AC-32: `--report-extended` flag to auto-include extended context (skip prompt)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-019 | Unit | P1 | `--report-extended` flag parsed correctly | Flag parsing logic |
| 001d-INT-017 | Integration | P1 | `--report-extended` skips prompt and generates extended URL | Flag to behavior flow |
| 001d-E2E-008 | E2E | P2 | `tea run broken.yaml --report-extended` generates extended URL directly | Non-interactive journey |

### AC-33: `--report-minimal` flag to skip extended prompt entirely

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-020 | Unit | P1 | `--report-minimal` flag parsed correctly | Flag parsing logic |
| 001d-INT-018 | Integration | P1 | `--report-minimal` skips prompt entirely | Flag to behavior flow |

### AC-34: Extended URL clearly marked as containing more information in the viewer

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-UNIT-021 | Unit | P2 | Extended display shows "Extended report" label | Output formatting logic |
| 001d-UNIT-022 | Unit | P2 | Extended display lists included context items | Output content validation |

---

## Cross-Runtime Parity Tests

These tests ensure Rust and Python implementations behave identically:

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001d-INT-PARITY-001 | Integration | P1 | Rust and Python produce identical URL for same error | Cross-runtime consistency |
| 001d-INT-PARITY-002 | Integration | P2 | Rust and Python display same message format | UX consistency |
| 001d-E2E-PARITY-001 | E2E | P1 | Same workflow produces same output in both runtimes | Parity validation |

---

## Risk Coverage

| Risk | Impact | Test Scenarios Mitigating |
|------|--------|---------------------------|
| Prompt blocks CI | Medium | 001d-UNIT-015 (timeout), detect non-interactive environment |
| Clipboard fails | Low | 001d-UNIT-012, 001d-INT-011 (graceful fallback) |
| URL display breaks | Low | 001d-UNIT-002, 001d-UNIT-010 (formatting validation) |
| Sensitive data leak | High | 001d-INT-015, 001d-INT-016, 001d-E2E-007 (security verification) |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. 001d-UNIT-001: URL formatting
2. 001d-UNIT-003: Default flag value
3. 001d-UNIT-005: Disable flag parsing

### Phase 2: P0 Integration Tests
4. 001d-INT-001: Rust panic hook
5. 001d-INT-002: Python excepthook
6. 001d-INT-003: YAML engine errors
7. 001d-INT-004: Executor errors
8. 001d-INT-015: State data exclusion
9. 001d-INT-016: Secrets exclusion

### Phase 3: P0 E2E Tests
10. 001d-E2E-001: File not found
11. 001d-E2E-002: Parse error
12. 001d-E2E-007: Security verification

### Phase 4: P1 Tests (In Order)
13. All P1 Unit tests
14. All P1 Integration tests
15. All P1 E2E tests
16. Parity tests

### Phase 5: P2+ Tests (As Time Permits)
17. Remaining P2 tests
18. P3 tests only in full regression

---

## Test Environment Requirements

### Rust Tests
```bash
# Unit tests
cd rust && cargo test --lib cli_integration

# Integration tests
cd rust && cargo test --test test_cli_report

# E2E tests
cd rust && cargo run -- run test_fixtures/broken.yaml 2>&1 | grep -q "Report this bug"
```

### Python Tests
```bash
# Unit tests
cd python && pytest tests/test_report_cli.py -k "unit"

# Integration tests
cd python && pytest tests/test_report_cli.py -k "integration"

# E2E tests
cd python && tea run test_fixtures/broken.yaml 2>&1 | grep -q "Report this bug"
```

### Environment Variables for Testing
```bash
# Disable report generation
TEA_REPORT_BUGS=false

# Enable extended context
TEA_REPORT_EXTENDED=true

# Non-interactive mode (for CI)
TEA_NON_INTERACTIVE=true
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (001d-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Security-critical paths have P0 coverage
- [x] Cross-runtime parity addressed

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 18
    integration: 16
    e2e: 8
  by_priority:
    p0: 12
    p1: 18
    p2: 10
    p3: 2
  coverage_gaps: []
  parity_tests: 3
  security_tests: 4
  critical_paths_covered:
    - panic_hook_integration
    - excepthook_integration
    - yaml_error_handling
    - executor_error_handling
    - sensitive_data_exclusion
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-REPORT-001d-test-design-20260111.md
P0 tests identified: 12
P1 tests identified: 18
Security-critical tests: 4
Cross-runtime parity tests: 3
```
