# Requirements Traceability Matrix

## Story: TEA-DX-001.7 — Quiet-mode heartbeat

Date: 2026-05-01
Tracer: Quinn (Test Architect)
Mode: YOLO (non-interactive)
Story status: Draft (implementation not started; mapping is to **planned** test scenarios in test design)
Test design ref: [docs/qa/assessments/TEA-DX-001.7-test-design-20260501.md](TEA-DX-001.7-test-design-20260501.md)
Risk ref: [docs/qa/assessments/TEA-DX-001.7-risk-20260501.md](TEA-DX-001.7-risk-20260501.md)
NFR ref: [docs/qa/assessments/TEA-DX-001.7-nfr-20260501.md](TEA-DX-001.7-nfr-20260501.md)

---

## Coverage Summary

- **Total Requirements:** 16 (AC-1..AC-12 + AC-13..AC-16 from NFR-recommended additions) + 1 implicit Tech-Note constraint (exit codes)
- **Fully Covered (planned):** 16 (94%)
- **Partially Covered:** 1 (6%) — Tech-Note exit-code preservation (manual P3 only on failure scenario)
- **Not Covered:** 0 (0%)

> Note: "Coverage" here = mapping to a defined scenario in the test design. No tests are implemented yet — this is a **planning-time** trace. Coverage status will be re-evaluated at review time once the PR lands.

---

## Requirement Mappings

### AC-1: New CLI flag `--heartbeat` (default off, independent of `--quiet`)

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-001` — `tea run --help`
  - **Given:** `tea` CLI installed and `run` subcommand registered
  - **When:** User runs `tea run --help`
  - **Then:** Help output lists `--heartbeat` as a flag with default=False
- **Integration Test (planned):** `TEA-DX-001.7-INT-002` — `run` invocation without `--heartbeat`
  - **Given:** A 3-node sequential workflow YAML
  - **When:** `tea run <yaml>` is invoked with no flags
  - **Then:** No heartbeat-pattern lines appear on stdout or stderr (default-off)

---

### AC-2: Heartbeat line format `[<node_name> done in <duration>]` on stderr

**Coverage: FULL (planned)** — combined unit + integration

- **Unit Test (planned):** `TEA-DX-001.7-UNIT-001` — `format_duration(0.05)`
  - **Given:** Sub-100ms duration value (0.05s)
  - **When:** Duration formatter is called
  - **Then:** Returns sub-second form (`50ms` or `0.1s` per chosen impl); deterministic, no exception
- **Unit Test (planned):** `TEA-DX-001.7-UNIT-002` — `format_duration(1.2)`
  - **Given:** Duration value of 1.2s
  - **When:** Formatter is called
  - **Then:** Returns `"1.2s"` (matches AC-2 sample)
- **Unit Test (planned):** `TEA-DX-001.7-UNIT-003` — `format_duration(45.3)`
  - **Given:** Duration value of 45.3s
  - **When:** Formatter is called
  - **Then:** Returns `"45.3s"` (matches AC-2 sample)
- **Unit Test (planned):** `TEA-DX-001.7-UNIT-004` — `format_duration(138)`
  - **Given:** Duration crossing the minute boundary (138s)
  - **When:** Formatter is called
  - **Then:** Returns `"2m 18s"` (matches AC-2 sample)
- **Integration Test (planned):** `TEA-DX-001.7-INT-003` — 3-node sequential run with `--heartbeat`
  - **Given:** A 3-node sequential YAML and `--heartbeat` flag
  - **When:** Workflow runs to completion
  - **Then:** stderr contains exactly 3 lines matching `^\[\S+ done in .+\]$`, in node order

---

### AC-3: `--heartbeat` + `--quiet` emits ONLY heartbeat lines (no other chatter)

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-004`
  - **Given:** A 3-node sequential YAML, `--quiet --heartbeat` flags
  - **When:** Workflow runs to completion
  - **Then:** stderr contains exactly N heartbeat lines and nothing else; no progress/info chatter

---

### AC-4: Heartbeat output goes to stderr (stdout untouched)

**Coverage: FULL (planned)** — split across two test levels for different threats

- **Integration Test (planned):** `TEA-DX-001.7-INT-005`
  - **Given:** `CliRunner(mix_stderr=False)` and `--heartbeat`
  - **When:** Workflow runs
  - **Then:** `result.stdout` has zero heartbeat-pattern matches; `result.stderr` has them
- **E2E Test (planned):** `TEA-DX-001.7-E2E-001`
  - **Given:** Real subprocess invocation with separated `stdout=PIPE, stderr=PIPE`
  - **When:** Workflow runs with `--heartbeat`
  - **Then:** stdout is byte-identical to the same workflow without `--heartbeat`; stderr is non-empty and matches heartbeat pattern (defends against `CliRunner` stream-mixing artefacts)

---

### AC-5: When `--heartbeat` is off, behavior is unchanged

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-006` (also covers AC-14, COMPAT-2)
  - **Given:** A 3-node sequential YAML and `--quiet` only (no `--heartbeat`)
  - **When:** Workflow runs successfully
  - **Then:** stderr contains zero heartbeat-pattern lines (regression guard for the silence contract)
- **Integration Test (planned):** `TEA-DX-001.7-INT-007`
  - **Given:** Default invocation (no flags)
  - **When:** Workflow runs successfully
  - **Then:** No heartbeat-pattern lines on stderr or stdout

---

### AC-6: Failed-node heartbeat `[<node_name> FAILED in <duration>]`

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-008`
  - **Given:** A 3-node workflow where the middle node raises `RuntimeError`
  - **When:** Workflow runs with `--heartbeat`
  - **Then:** stderr contains `[<node> FAILED in <duration>]` for the failing node; non-zero exit code preserved

---

### AC-7: Compatible with `--quiet`, `--stream`, `--show-graph`

**Coverage: FULL (planned)** — `--quiet` covered by INT-004; `--stream` and `--show-graph` covered below

- **Integration Test (planned):** `TEA-DX-001.7-INT-010`
  - **Given:** `--heartbeat --stream` on a 3-node workflow
  - **When:** Workflow runs to completion
  - **Then:** No crash; both heartbeat lines and stream events appear on stderr (documents redundant-but-not-harmful behavior, COMPAT-1)
- **Integration Test (planned):** `TEA-DX-001.7-INT-011`
  - **Given:** `--heartbeat --show-graph` on a 3-node workflow
  - **When:** Command runs
  - **Then:** No crash; graph rendering unaffected; heartbeat lines emitted

---

### AC-8: Compatible with `--debug-state` (TEA-DX-001.3)

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-012`
  - **Given:** `--heartbeat --debug-state <dir>` on a 2-node workflow
  - **When:** Workflow runs to completion
  - **Then:** No crash; heartbeat lines on stderr AND state dumps written to `<dir>`

---

### AC-9: Parallel branches each emit a prefixed heartbeat

**Coverage: FULL (planned)** — split unit (formatting helper) + integration (end-to-end attribution)

- **Unit Test (planned):** `TEA-DX-001.7-UNIT-006`
  - **Given:** A parallel branch label
  - **When:** Branch-prefix helper is called
  - **Then:** Returns the agreed prefix (`parallel:<branch>` per test design assumption)
- **Integration Test (planned):** `TEA-DX-001.7-INT-013` (also covers AC-16)
  - **Given:** Fan-out to K=3 parallel branches with `--heartbeat`
  - **When:** All branches complete
  - **Then:** stderr contains exactly 3 prefixed heartbeat lines; **set semantics** — branch set equals `{"branch_1","branch_2","branch_3"}`; ordering across branches is NOT asserted (TECH-2 mitigation)

---

### AC-10: Test asserts heartbeat lines for a 3-node workflow

**Coverage: FULL (planned)** — shared mapping

- Covered by `TEA-DX-001.7-INT-003` (see AC-2 mapping). The same scenario validates both line format (AC-2) and line count for a 3-node workflow (AC-10).

---

### AC-11: Test asserts heartbeat goes to stderr, not stdout

**Coverage: FULL (planned)** — shared mapping

- Covered by `TEA-DX-001.7-INT-005` and `TEA-DX-001.7-E2E-001` (see AC-4 mapping). INT-005 uses `CliRunner(mix_stderr=False)`; E2E-001 confirms real OS-level pipe separation.

---

### AC-12: Documented in CLI reference

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-014`
  - **Given:** `tea run --help`
  - **When:** Help text is rendered
  - **Then:** Help output contains a `--heartbeat` description AND explicitly mentions `stderr` as the output destination

---

### AC-13: Duration formatter unit tests cover boundary cases (NFR-recommended)

**Coverage: FULL (planned)** — shared mapping

- Covered by `TEA-DX-001.7-UNIT-001..005`. Boundaries: <100ms (UNIT-001), `1.2s` (UNIT-002), `45.3s` (UNIT-003), `2m 18s` (UNIT-004), `>1h` (UNIT-005).

---

### AC-14: Regression test — `--quiet` alone produces zero heartbeat lines (NFR-recommended)

**Coverage: FULL (planned)** — shared mapping

- Covered by `TEA-DX-001.7-INT-006` (see AC-5 mapping). Explicit COMPAT-2 silence-guarantee guard.

---

### AC-15: Failure-path duration is > 0 and ≤ wall-clock bound (NFR-recommended)

**Coverage: FULL (planned)**

- **Integration Test (planned):** `TEA-DX-001.7-INT-009`
  - **Given:** A workflow with a failing middle node and `--heartbeat`
  - **When:** Failure is observed and the heartbeat line is parsed
  - **Then:** Parsed duration > 0 AND ≤ wall-clock bound captured by the test (defends TECH-3)

---

### AC-16: Parallel-branch test asserts set semantics, not ordering (NFR-recommended)

**Coverage: FULL (planned)** — shared mapping

- Covered by `TEA-DX-001.7-INT-013` (see AC-9 mapping). Test compares **branch sets**, never orders.

---

### Tech-Note Constraint: Heartbeat does not change exit codes

**Coverage: PARTIAL (planned)** — defensive unit + manual E2E only on a long workflow

- **Unit Test (planned):** `TEA-DX-001.7-UNIT-007`
  - **Given:** A formatter that raises (simulated)
  - **When:** Heartbeat path executes around the formatter
  - **Then:** Exception is swallowed; main run continues; exit code reflects workflow only (defensive guard)
- **E2E Test (planned, manual/CI smoke only):** `TEA-DX-001.7-E2E-002`
  - **Given:** A long-ish workflow exercised in CI under `--heartbeat`
  - **When:** Both success and failure paths are run
  - **Then:** Exit code matches the workflow outcome; CI tooling does not flag stderr lines as errors (covers OPS-1)

> Marked PARTIAL because exit-code invariance is asserted directly only on the failure path (INT-008) and on the unit-level guard (UNIT-007); a dedicated unit/integration-level success-path exit-code assertion is not separately enumerated. Acceptable for a non-AC tech-note constraint, but flagged for awareness.

---

## Critical Gaps

**None blocking.** All ACs (1–12) and NFR-recommended ACs (13–16) have at least one planned test scenario in the test design. The only nuance:

1. **Exit-code preservation on success path (Tech Note)** — covered indirectly via INT-003 (which asserts `exit_code == 0`), but not promoted to its own scenario. *Severity: low.* No action required unless review reveals a real regression.

---

## Coverage Pre-Conditions (Implementation-Side)

The trace above assumes two design questions from the risk profile are resolved **before** test authoring begins. Until they are, INT-003 and INT-008 cannot be implemented to a fixed contract:

1. **TECH-1 design choice (BLOCKING for test authoring):** Pick wall-clock delta between consecutive `state` events vs. add an explicit `node_start` event in the engine. The current Technical Notes reference a `node_start` event that doesn't exist in `cli.py:2236+`. *Risk: P0 tests written against the wrong assumption will need to be rewritten.*
2. **Parallel-branch prefix format (BLOCKING for UNIT-006/INT-013):** Confirm `parallel:<branch>` (current test-design assumption) or another agreed format. UNIT-006 and INT-013 both pin to this contract.

These are not gaps in *test coverage* — they are gaps in *implementation contract* that must be settled before P0 tests can be written.

---

## Test Design Recommendations

No additional scenarios beyond the 22 already enumerated in the test design. The test design already addresses all coverage gaps the risk profile and NFR assessment surfaced. Specifically:

- TECH-1 → INT-003 + INT-008 (timing path, both success and failure)
- TECH-2 → UNIT-006 + INT-013 (set semantics, no ordering)
- TECH-3 → INT-009 + UNIT-007 (failure duration bound + defensive guard)
- COMPAT-1 → INT-010 (heartbeat + stream coexistence)
- COMPAT-2 → INT-002, INT-006, INT-007 (default-off regression guards)
- TEST-1 → all INT tests use `CliRunner(mix_stderr=False)` per fixture guidance
- OPS-1 → E2E-002 (CI smoke)

If anything is added during implementation, the natural candidate is a dedicated success-path exit-code assertion (e.g., `assert result.exit_code == 0` on `INT-007` baseline). Optional, not blocking.

---

## Risk Assessment (per requirement)

- **High Risk (no coverage):** *None.*
- **Medium Risk (partial coverage):** *None at AC level.* Tech-Note exit-code constraint is partial — see note above; severity low.
- **Low Risk (full unit + integration coverage):** AC-1, AC-2, AC-3, AC-4, AC-5, AC-6, AC-9, AC-10, AC-11, AC-13, AC-14, AC-15, AC-16.
- **Coverage at integration only (acceptable):** AC-7, AC-8, AC-12 — these are CLI-surface contracts where unit-level tests would be redundant.

---

## Gate YAML Block

```yaml
trace:
  totals:
    requirements: 16
    full: 16
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.7-test-design-20260501.md'
  uncovered: []
  notes: 'Story is in Draft. Mapping is to PLANNED test scenarios from test design. Implementation-side preconditions (TECH-1 design choice; parallel-branch prefix format) must be resolved before P0 test authoring. Tech-Note exit-code preservation is partial (covered indirectly on success path, directly on failure and via defensive unit guard) — non-blocking. See docs/qa/assessments/TEA-DX-001.7-trace-20260501.md.'
```

---

## Trace References

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.7-trace-20260501.md
Test design: docs/qa/assessments/TEA-DX-001.7-test-design-20260501.md (22 scenarios, 9 P0)
ACs covered: 16/16 (planned)
Implementation preconditions: 2 (TECH-1 design choice; parallel-branch prefix format)
```

---

## Quality Checklist

- [x] Every AC (1–12) and NFR-recommended AC (13–16) maps to ≥1 planned test scenario
- [x] Critical paths (success, failure, parallel) have multi-level coverage (unit + integration; E2E where OS-pipe semantics matter)
- [x] Edge cases explicitly covered: sub-100ms, minute rollover, hour rollover, parallel attribution, failure path
- [x] NFR concerns from prior assessments are mapped (TECH-1/2/3, COMPAT-1/2, TEST-1, OPS-1)
- [x] Given-When-Then is used for documentation only — actual test code follows pytest conventions
- [x] Trace explicitly distinguishes **planned** vs **implemented** coverage; no false PASS implication for a Draft story
