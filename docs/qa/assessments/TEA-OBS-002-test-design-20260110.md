# Test Design: Story TEA-OBS-002

**Date:** 2026-01-10
**Designer:** Quinn (Test Architect)
**Story:** Opik Observability for Rust and WASM LLM Calls
**Status:** Draft

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 67 | 100% |
| **Unit tests** | 28 | 42% |
| **Integration tests** | 24 | 36% |
| **E2E tests** | 15 | 22% |
| **Priority distribution** | P0: 18, P1: 29, P2: 16, P3: 4 | - |

### Risk Assessment Summary

| Risk Area | Priority | Test Emphasis |
|-----------|----------|---------------|
| API key security (never in YAML) | P0 | Unit + E2E |
| Opik REST API integration | P0 | Integration |
| Fire-and-forget reliability | P0 | Unit + Integration |
| Cross-runtime parity | P1 | Integration |
| Demo UI functionality | P2 | E2E |

---

## Test Scenarios by Acceptance Criteria

### Section 1: Rust Native (AC-1 to AC-9)

#### AC-1: OpikHandler implements EventHandler trait

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-001 | Unit | P0 | `OpikHandler` struct compiles with `EventHandler` trait bounds | Trait compliance is compile-time verification |
| OBS002-UNIT-002 | Unit | P1 | `OpikHandler::handle()` accepts `LogEvent` and returns | Core interface contract |
| OBS002-UNIT-003 | Unit | P1 | `OpikHandler::flush()` drains buffer completely | Interface completeness |

#### AC-2: OpikHandler sends spans to Opik REST API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-004 | Unit | P0 | `send_batch()` constructs correct JSON payload structure | Pure data transformation |
| OBS002-UNIT-005 | Unit | P0 | `send_batch()` includes required headers (Authorization, Content-Type) | Security-critical header handling |
| OBS002-INT-001 | Integration | P0 | Mock HTTP server receives POST to `/v1/traces` | API contract verification |
| OBS002-INT-002 | Integration | P1 | Trace payload matches Opik schema (name, project_name, start_time, input, metadata) | Data integrity |

#### AC-3: Configuration via OPIK_API_KEY environment variable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-006 | Unit | P0 | `from_config()` reads `OPIK_API_KEY` from environment | Core security configuration |
| OBS002-UNIT-007 | Unit | P0 | `from_config()` fails with clear error when `OPIK_API_KEY` missing | Graceful failure |
| OBS002-UNIT-008 | Unit | P1 | API key is never logged or serialized | Security: no credential leakage |

#### AC-4: YAML settings support (project_name, workspace)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-009 | Unit | P1 | `OpikConfig` deserializes from YAML `settings.opik` block | Configuration parsing |
| OBS002-UNIT-010 | Unit | P2 | `project_name` defaults to "the-edge-agent" when not specified | Sensible default |
| OBS002-UNIT-011 | Unit | P2 | `workspace` is optional and can be None | Optional field handling |

#### AC-5: Auto-enable when YAML config + env var present

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-003 | Integration | P1 | Handler auto-registers when both `settings.opik` and `OPIK_API_KEY` present | Auto-configuration flow |
| OBS002-INT-004 | Integration | P1 | Handler NOT registered when `settings.opik` missing (even with env var) | Explicit opt-in |
| OBS002-INT-005 | Integration | P1 | Handler NOT registered when env var missing (even with YAML config) | Security gate |

#### AC-6: Token usage captured in spans

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-012 | Unit | P0 | `LogEvent.metrics.tokens` maps to `usage.prompt_tokens` | Token tracking accuracy |
| OBS002-UNIT-013 | Unit | P1 | Token usage is optional (null/None handled gracefully) | Robustness |
| OBS002-INT-006 | Integration | P1 | LLM call captures and reports token count to Opik | End-to-end token flow |

#### AC-7: Latency (duration_ms) captured in spans

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-014 | Unit | P1 | `duration_ms` calculated from start/end timestamps | Latency calculation |
| OBS002-UNIT-015 | Unit | P2 | Duration is non-negative (sanity check) | Data validity |

#### AC-8: Graceful degradation when API key missing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-016 | Unit | P0 | `from_config()` returns `Err` with helpful message, not panic | No crash on misconfiguration |
| OBS002-INT-007 | Integration | P0 | Workflow completes successfully even when Opik handler creation fails | Workflow resilience |

#### AC-9: Async batch sending

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-017 | Unit | P1 | Buffer accumulates events until `batch_size` reached | Batching logic |
| OBS002-UNIT-018 | Unit | P1 | `flush()` sends remaining events regardless of batch size | Flush completeness |
| OBS002-UNIT-019 | Unit | P2 | Failed batch send logs error but doesn't crash | Fire-and-forget reliability |

---

### Section 2: WASM (AC-10 to AC-19)

#### AC-10: Export enableOpikTracing(config?)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-020 | Unit | P0 | `enableOpikTracing` is exported from `tea-wasm-llm/opik` | Package API surface |
| OBS002-UNIT-021 | Unit | P1 | `enableOpikTracing()` accepts optional config parameter | API flexibility |

#### AC-11: Export disableOpikTracing()

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-022 | Unit | P1 | `disableOpikTracing` is exported and callable | API completeness |
| OBS002-UNIT-023 | Unit | P1 | `disableOpikTracing()` clears the registered handler | Cleanup behavior |

#### AC-12: Dynamic import of @comet-ml/opik-js

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-024 | Unit | P1 | `enableOpikTracing()` uses dynamic `import()` for SDK | Tree-shaking support |
| OBS002-INT-008 | Integration | P1 | Bundle size unchanged when `enableOpikTracing()` not called | Zero-cost when unused |

#### AC-13: Node.js reads OPIK_API_KEY from process.env

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-009 | Integration | P0 | `enableOpikTracing()` reads `process.env.OPIK_API_KEY` in Node.js | Environment integration |
| OBS002-INT-010 | Integration | P1 | Explicit `config.apiKey` overrides environment variable | Config precedence |

#### AC-14: Browser requires explicit apiKey

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-001 | E2E | P0 | Browser: `enableOpikTracing()` without apiKey throws clear error | Security enforcement |
| OBS002-E2E-002 | E2E | P1 | Browser: `enableOpikTracing({ apiKey: "..." })` succeeds | Browser API works |

#### AC-15: Clear error message when API key missing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-025 | Unit | P0 | Error message includes environment-specific guidance | User experience |
| OBS002-UNIT-026 | Unit | P1 | Node.js error mentions `OPIK_API_KEY` env var | Contextual help |
| OBS002-UNIT-027 | Unit | P1 | Browser error mentions `{ apiKey: "..." }` syntax | Contextual help |

#### AC-16: LLM callback captures token usage and latency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-011 | Integration | P0 | WASM LLM callback records start_time before invocation | Timing accuracy |
| OBS002-INT-012 | Integration | P0 | WASM LLM callback records end_time after response | Timing accuracy |
| OBS002-INT-013 | Integration | P1 | Token usage extracted from LLM response JSON | Data extraction |

#### AC-17: Handler receives structured JSON with span data

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-028 | Unit | P1 | `OpikSpanData` serializes to expected JSON schema | Data contract |
| OBS002-INT-014 | Integration | P1 | JS handler receives valid JSON string | Cross-language boundary |

#### AC-18: Telemetry failures don't block workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-015 | Integration | P0 | Workflow completes even when Opik handler throws | Fire-and-forget guarantee |
| OBS002-INT-016 | Integration | P1 | Network failure in `sendToOpik()` logs warning, doesn't throw | Resilience |
| OBS002-E2E-003 | E2E | P1 | Demo runs to completion with invalid API key | User experience |

#### AC-19: TypeScript declarations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-029 | Unit | P2 | `.d.ts` files export `enableOpikTracing`, `disableOpikTracing` | TypeScript support |
| OBS002-UNIT-030 | Unit | P2 | `OpikTracingConfig` interface is exported | Developer experience |

---

### Section 3: Package Configuration (AC-20 to AC-21)

#### AC-20: @comet-ml/opik-js as optional peer dependency

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-UNIT-031 | Unit | P1 | `package.json` lists SDK in `peerDependencies` | Package config |
| OBS002-UNIT-032 | Unit | P1 | `peerDependenciesMeta` marks SDK as `optional: true` | Optional dependency |

#### AC-21: Clear error if peer dependency not installed

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-017 | Integration | P0 | `enableOpikTracing()` throws helpful error when SDK missing | Actionable error |
| OBS002-INT-018 | Integration | P2 | Error message includes `npm install @comet-ml/opik-js` | Install guidance |

---

### Section 4: Testing ACs (AC-22 to AC-25)

#### AC-22-25: Testing infrastructure

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-019 | Integration | P0 | OpikHandler unit tests pass with mock HTTP | CI requirement |
| OBS002-INT-020 | Integration | P2 | Integration test with real Opik API (ignored by default) | Manual verification |
| OBS002-E2E-004 | E2E | P1 | Playwright test for WASM with mock opik handler | Browser validation |
| OBS002-INT-021 | Integration | P1 | Rust and WASM produce equivalent trace data for same workflow | Cross-runtime parity |

---

### Section 5: Documentation (AC-26 to AC-28)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-022 | Integration | P3 | README code examples compile/run successfully | Doc accuracy |
| OBS002-INT-023 | Integration | P3 | YAML reference examples validate against schema | Doc accuracy |

---

### Section 6: WASM Demo (AC-29 to AC-39)

#### AC-29: Tab navigation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-005 | E2E | P1 | Demo shows "Standard" and "Opik Tracing" tabs | UI structure |
| OBS002-E2E-006 | E2E | P1 | Clicking tab switches visible content | Tab interaction |

#### AC-30-31: Trace timeline and token badges

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-007 | E2E | P1 | Trace timeline displays node names after workflow execution | UI feedback |
| OBS002-E2E-008 | E2E | P2 | Duration displayed in milliseconds | UI detail |
| OBS002-E2E-009 | E2E | P2 | Token usage badge shows "X -> Y tokens" format | UI detail |

#### AC-32: Standard tab unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-010 | E2E | P0 | Standard tab workflow execution still works | Regression prevention |

#### AC-33-34: Demo mode vs Live mode

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-011 | E2E | P1 | No API key shows "Demo Mode" badge (yellow) | Mode indication |
| OBS002-E2E-012 | E2E | P1 | With API key shows "Live Mode" badge (green) | Mode indication |

#### AC-35-37: Live mode indicators

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-E2E-013 | E2E | P2 | Sent traces show "Sent" status | Success feedback |
| OBS002-E2E-014 | E2E | P2 | "View in Opik" link appears for sent traces | Navigation |
| OBS002-E2E-015 | E2E | P2 | Failed submissions show "Error" with tooltip | Error feedback |

#### AC-38-39: Real-time badge toggle and config inputs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| OBS002-INT-024 | Integration | P2 | Typing in API key input toggles badge without form submit | Real-time UX |
| OBS002-E2E-016 | E2E | P3 | Project name input value used in Opik submission | Config passthrough |
| OBS002-E2E-017 | E2E | P3 | Workspace input value used in Opik submission | Config passthrough |

---

## Risk Coverage Matrix

| Risk | Mitigating Tests | Coverage |
|------|-----------------|----------|
| API key leaked in YAML | OBS002-UNIT-008, story design prohibits | Strong |
| Workflow blocked by telemetry failure | OBS002-INT-015, OBS002-INT-016, OBS002-E2E-003 | Strong |
| SDK breaking changes | OBS002-INT-008 (dynamic import), version pin | Medium |
| Cross-runtime inconsistency | OBS002-INT-021 (parity test) | Strong |
| Demo regression | OBS002-E2E-010 (standard tab unchanged) | Strong |

---

## Recommended Execution Order

1. **P0 Unit tests** (OBS002-UNIT-001 to 008, 016, 020, 025-27) - Fail fast on core logic
2. **P0 Integration tests** (OBS002-INT-001, 007, 009, 011-12, 015, 017, 019) - API contracts
3. **P0 E2E tests** (OBS002-E2E-001, 010) - Critical user paths
4. **P1 tests** in order - Core functionality
5. **P2+ tests** as time permits - Polish and edge cases

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-OBS-002
  designer: Quinn
  date: 2026-01-10
  scenarios_total: 67
  by_level:
    unit: 28
    integration: 24
    e2e: 15
  by_priority:
    p0: 18
    p1: 29
    p2: 16
    p3: 4
  coverage_gaps: []
  critical_paths:
    - Rust OpikHandler trait implementation and HTTP sending
    - WASM enableOpikTracing() with dynamic SDK import
    - Fire-and-forget telemetry (no workflow blocking)
    - Cross-runtime parity for trace data
  estimated_test_effort: Medium-High (39 ACs across 2 runtimes + demo)
```

---

## Quality Checklist

- [x] Every AC has at least one test (39 ACs -> 67 scenarios)
- [x] Test levels are appropriate (shift-left applied: 42% unit)
- [x] No duplicate coverage across levels (each scenario has clear justification)
- [x] Priorities align with business risk (security/reliability = P0)
- [x] Test IDs follow naming convention (`OBS002-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-OBS-002-test-design-20260110.md`
P0 tests identified: 18
