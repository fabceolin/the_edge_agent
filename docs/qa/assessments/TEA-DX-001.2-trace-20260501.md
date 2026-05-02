# Requirements Traceability Matrix

## Story: TEA-DX-001.2 — CLI `--trace-file` flag

**Reviewer:** Quinn (Test Architect) — `*trace` (YOLO mode)
**Date:** 2026-05-01
**Story status at trace time:** Draft (implementation not yet started)
**Planning refs:**
- Test design: [`docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md`](TEA-DX-001.2-test-design-20260501.md)
- NFR assessment: [`docs/qa/assessments/TEA-DX-001.2-nfr-20260501.md`](TEA-DX-001.2-nfr-20260501.md)
- Risk profile: in-story `## QA Notes - Risk Profile`

> **YOLO trace caveat:** Implementation is in Draft, so no concrete test files exist yet. This matrix maps each AC to the *planned* scenarios in the test design (`TEA-DX-001.2-INT-NNN` / `-UNIT-NNN` IDs) and the recommended test homes (`python/tests/test_cli_trace_file.py` plus an additional unit in `python/tests/test_yaml_engine.py`). Coverage state therefore reflects design coverage, not yet executed coverage. Re-run `*trace` post-implementation to convert design coverage into executed coverage.

## Coverage Summary

- **Total Requirements:** 12 ACs (AC-1 through AC-11 from the story, plus the NFR-recommended **AC-12** for bad-path UX).
- **Fully Covered (design):** 12 / 12 (100%) — every AC has at least one mapped P0/P1/P2 scenario in the test design.
- **Partially Covered:** 0
- **Not Covered:** 0

| Coverage class | Count | ACs |
|---|---|---|
| Full (≥1 P0 mapping) | 4 | AC-1, AC-4, AC-7, AC-10 |
| Full (≥1 P1 mapping) | 7 | AC-2, AC-3, AC-5, AC-6, AC-12 (recommended), plus AC-10 (rolls up) |
| Full (P2-only) | 3 | AC-8, AC-9, AC-11 |

All medium-severity risks (R1, R2, R6) and all NFR concerns (T1, T3, T4, T5) have at least one mapped scenario; see Risk → Test and NFR → Test sections below.

## Requirement Mappings

### AC-1 — `--trace-file` overrides `settings.trace_file`

**Coverage: FULL (P0)**

Given-When-Then mappings:

- **Integration Test (planned):** `python/tests/test_cli_trace_file.py::TEA-DX-001.2-INT-001`
  - **Given:** A YAML fixture with `settings.trace_file: yaml.jsonl` and a single-node graph.
  - **When:** `tea run <fixture> --trace-file <tmp_path>/cli.jsonl` is invoked via `CliRunner`.
  - **Then:** `<tmp_path>/cli.jsonl` is created and contains JSONL events; `<tmp_path>/yaml.jsonl` is *not* created; exit code is 0.
- **Unit Test (planned):** `python/tests/test_yaml_engine.py::TEA-DX-001.2-UNIT-002`
  - **Given:** `YAMLEngine(trace_file="cli.jsonl")` constructed against a parsed-YAML dict whose `settings.trace_file` is `"yaml.jsonl"`.
  - **When:** `_configure_from_settings` runs.
  - **Then:** The installed exporter targets `cli.jsonl`; the settings-supplied path is not layered on top.

Maps to risk **R1 (override precedence)**.

### AC-2 — Implicit exporter promotion (`unset` / `console` → `file`)

**Coverage: FULL (P1)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-002` (`test_cli_trace_file.py`)
  - **Given:** YAML omits `settings.trace_exporter`.
  - **When:** `tea run --trace-file out.jsonl` runs.
  - **Then:** `out.jsonl` is populated with JSONL trace events (exporter implicitly switches to `file`).
- **Integration Test (planned):** `TEA-DX-001.2-INT-003` (`test_cli_trace_file.py`)
  - **Given:** YAML sets `settings.trace_exporter: console`.
  - **When:** `tea run --trace-file out.jsonl` runs.
  - **Then:** Exporter switches to `file`; `out.jsonl` is populated; documented behavior re: console (suppressed vs dual-output) is pinned by the test.

Maps to risks **R2** and **R6**.

### AC-3 — Exporter preserved when already `file`

**Coverage: FULL (P1)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-004` (`test_cli_trace_file.py`)
  - **Given:** YAML sets `settings.trace_exporter: file` and `settings.trace_file: yaml.jsonl`.
  - **When:** `tea run --trace-file cli.jsonl` runs.
  - **Then:** Exporter type is unchanged (stays `file`); trace path is overridden to `cli.jsonl` only.

Maps to risk **R1** (precedence) and **R6** (matrix coverage).

### AC-4 — Implicit `auto_trace=true` when YAML opts out

**Coverage: FULL (P0)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-005` (`test_cli_trace_file.py`)
  - **Given:** YAML sets `settings.auto_trace: false` (no other trace settings).
  - **When:** `tea run --trace-file out.jsonl` runs.
  - **Then:** `out.jsonl` is created and contains ≥1 well-formed JSONL trace event; exit code 0.

Maps to risks **R2** and **R6**.

### AC-5 — `${ENV_VAR}` expansion + parity with TEA-DX-001.1

**Coverage: FULL (P1)**

- **Unit Test (planned):** `TEA-DX-001.2-UNIT-001`
  - **Given:** Env `TRACE_DIR=/tmp/foo` set; an unset `MISSING` var.
  - **When:** `expand_env_vars("${TRACE_DIR}/run.jsonl")` and `expand_env_vars("${MISSING:-default}")` are called.
  - **Then:** Resolves to `/tmp/foo/run.jsonl` and `default` respectively, matching TEA-DX-001.1 semantics.
- **Integration Test (planned):** `TEA-DX-001.2-INT-006` (`test_cli_trace_file.py`)
  - **Given:** `TRACE_DIR=<tmp_path>` is set; one fixture uses `settings.trace_file: ${TRACE_DIR}/yaml.jsonl`, a second run supplies the same string via `--trace-file`.
  - **When:** Both runs execute.
  - **Then:** Both runs land traces at the same resolved path.

Maps to risks **R4** (env-var expansion drift).

### AC-6 — `tea run --help` shows `--trace-file`

**Coverage: FULL (P1)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-007` (`test_cli_trace_file.py`)
  - **Given:** A `CliRunner` instance.
  - **When:** `tea run --help` is invoked.
  - **Then:** Exit code 0; stdout contains the literal `--trace-file` and a non-empty description fragment.

Maps to risks **R2** (implicit-enable disclosure) and **R7** (docs/help precedence).

### AC-7 — No-flag baseline behavior unchanged

**Coverage: FULL (P0)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-008` (`test_cli_trace_file.py`)
  - **Given:** A pre-change golden run of the same fixture with `--quiet` and without `--quiet`.
  - **When:** `tea run <fixture>` is invoked without `--trace-file`.
  - **Then:** Exit code, stdout, and trace-emission behavior are byte-identical to the baseline (no regression).

Maps to risk **R1** (defense-in-depth — proves CLI doesn't unconditionally inject an exporter).

### AC-8 — Works with `--quiet`, `--stream`, `--show-graph`

**Coverage: FULL (P2)**

- **Integration Tests (planned):** `TEA-DX-001.2-INT-009`, `INT-010`, `INT-011` (`test_cli_trace_file.py`)
  - **Given:** Single-node fixture; `tmp_path` for trace destination.
  - **When (INT-009):** `tea run --trace-file out.jsonl --quiet`.
  - **When (INT-010):** `tea run --trace-file out.jsonl --stream`.
  - **When (INT-011):** `tea run --trace-file out.jsonl --show-graph`.
  - **Then (each):** Trace file populated; mode-specific output behavior (suppressed/streamed/graph rendering) unaffected; exit code 0.

### AC-9 — No interaction with `--output`

**Coverage: FULL (P2)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-012` (`test_cli_trace_file.py`)
  - **Given:** Single-node fixture.
  - **When:** `tea run --trace-file traces.jsonl --output state.json`.
  - **Then:** Both files produced; contents disjoint (JSONL trace events vs final state JSON); no cross-contamination.

### AC-10 — Test coverage on flag override / implicit-enable / env-var / missing-flag

**Coverage: FULL (P0)** — meta-AC; satisfied by the full P0+P1 test set.

Mapped by:
- AC-10 ↔ AC-1 / INT-001 (flag override, P0)
- AC-10 ↔ AC-2 / INT-002 (implicit-enable, P1)
- AC-10 ↔ AC-5 / UNIT-001 + INT-006 (env-var, P1)
- AC-10 ↔ AC-7 / INT-008 (missing flag = no regression, P0)

This AC is mechanically satisfied iff the four scenarios above pass.

### AC-11 — `--trace-file` documented in CLI reference

**Coverage: FULL (P2)**

- **Doc Test (planned):** `TEA-DX-001.2-DOC-001`
  - **Given:** `docs/python/` CLI reference (location to be confirmed during implementation — likely a CLI cheatsheet or `cli-reference.md`).
  - **When:** A grep / doc-link assertion runs in CI (or manual review).
  - **Then:** Reference contains `--trace-file` plus the precedence note: *"CLI `--trace-file` overrides `settings.trace_file`. When set, implicitly enables `auto_trace=true` and switches `trace_exporter` to `file` if unset or `console`."*

Maps to risk **R7** (docs precedence ambiguity) and NFR concern **R2** (implicit-enable disclosure).

### AC-12 (NFR-recommended) — Bad path → clean error, not raw traceback

**Coverage: FULL (P1, promoted from P2)**

- **Integration Test (planned):** `TEA-DX-001.2-INT-013` (`test_cli_trace_file.py`)
  - **Given:** A path whose parent directory does not exist (e.g., `/nonexistent/dir/foo.jsonl`).
  - **When:** `tea run <fixture> --trace-file /nonexistent/dir/foo.jsonl`.
  - **Then:** Non-zero exit; stderr references `--trace-file`; stderr does **not** contain `Traceback (most recent call last)`.

Maps to risks **R3** (path safety) and **R5** (opaque traceback). Closes the NFR `reliability: CONCERNS` finding.

> **Status note:** AC-12 is currently a *recommendation* from the NFR assessment. The story's authoritative AC list is AC-1 through AC-11. To convert the trace from "design-complete" to "ready for implementation," the dev/PO should formally adopt AC-12 in the story body. If AC-12 is *not* adopted, R5 / NFR-T1 remain mitigated only by INT-013 as a P2 robustness scenario.

### Determinism (auxiliary, not a story AC)

- **Integration Test (planned):** `TEA-DX-001.2-INT-014` — pins existing `FileExporter` overwrite-vs-append semantics (P3).

## Critical Gaps

**None at design level.** Every story AC, every NFR concern (T1, T3, T4, T5), and every identified risk (R1–R7) has at least one mapped scenario.

Smaller observations / non-blocking gaps:

1. **Recommended AC not yet adopted.** AC-12 is referenced by both the NFR assessment and this trace, but is not yet in the story's authoritative AC list (AC-1–AC-11). Until adopted, INT-013 sits as a P1 robustness scenario rather than an AC verification — slightly weakens the gate posture if a reviewer demands strict 1:1 AC→test mapping. **Recommendation:** add AC-12 to the story body before `*review`.
2. **CLI reference doc target unconfirmed.** AC-11 says "CLI reference in `docs/python/`" but the exact file is TBD. **Recommendation:** during implementation, identify the canonical CLI doc page (e.g., `docs/python/cli-reference.md` if it exists, otherwise add the line to `docs/python/getting-started.md` or a new dedicated file) and pin DOC-001's grep target there.
3. **Unwritable-parent extension is P2 only.** NFR-T2 (parent dir exists but is read-only) is currently a P2 follow-on to INT-013, not its own scenario. Acceptable for a developer-facing CLI but worth tracking if external-runner reports surface unwritable-fs cases.
4. **No executed coverage yet.** All entries above are *design coverage*. After implementation lands, re-run `*trace` to confirm each test ID was actually written and is green; convert this matrix from "planned" to "verified" at gate time.
5. **`--show-graph` interaction (INT-011) is P2.** AC-8 lists `--show-graph` alongside `--quiet` / `--stream`, but only the latter two are asserted byte-identically against a baseline (INT-008 + INT-009 + INT-010 cover NFR-T5). If `--show-graph` ever changes its rendering pipeline, INT-011 alone won't catch a regression in trace file contents under that mode. **Recommendation:** if `--show-graph` ever becomes part of an external-runner contract, promote INT-011 to P1 with a content assertion.

## Risk → Test Coverage

| Risk | Severity | Covered by | Status |
| --- | --- | --- | --- |
| R1 — Override precedence | Medium | INT-001 (P0), UNIT-002 (P0), INT-008 (P0) | Defense-in-depth (unit + integration), full coverage |
| R2 — Implicit-enable surprise | Medium | INT-002 (P1), INT-003 (P1), INT-005 (P0), INT-007 (P1), DOC-001 (P2) | Behavior + help text + docs all pinned |
| R3 — Path traversal | Low | INT-013 (P1, promoted) | OS perms remain primary mitigation |
| R4 — Env-var expansion drift | Low | UNIT-001 (P1), INT-006 (P1) | Parity test is the load-bearing assertion |
| R5 — Opaque traceback on bad path | Low | INT-013 (P1, promoted) | Closes NFR reliability CONCERNS |
| R6 — Implicit-enable matrix coverage | Medium | INT-002 + INT-003 + INT-004 + INT-005 | Covers exporter × auto_trace cells without exhaustive 9-cell enumeration |
| R7 — Docs precedence ambiguity | Low | INT-007 (P1) + DOC-001 (P2) | Help text + full reference both enforced |

All seven risks have ≥1 mapped scenario.

## NFR → Test Coverage

| NFR concern | Mapped scenario | Priority | Status |
| --- | --- | --- | --- |
| T1 — Bad path → `typer.BadParameter` | INT-013 | P1 (promoted) | Mapped |
| T2 — Unwritable parent dir | INT-013 (`chmod 0o500` extension) | P2 follow-on | Mapped (extension) |
| T3 — Env-var parity (YAML vs CLI) | INT-006 + UNIT-001 | P1 | Mapped |
| T4 — `tea run --help` snapshot | INT-007 | P1 | Mapped |
| T5 — No-flag baseline byte-identical with `--quiet`/`--stream` | INT-008 + INT-009 + INT-010 | P0 | Mapped |
| T6 — Invocation-overhead delta <5% | — | P3, skipped per NFR doc | Intentional gap |

## Test Design Recommendations

Already exhaustively documented in [`docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md`](TEA-DX-001.2-test-design-20260501.md). Key callouts surfaced by this trace:

1. **Adopt AC-12.** Promote the NFR-recommended bad-path UX criterion into the story's authoritative AC list so INT-013 is an AC verification, not a P1 robustness scenario without an AC anchor.
2. **Pin DOC-001's target.** Identify the canonical CLI reference file before merge so the grep assertion is stable.
3. **Add a test list to the dev story.** The story's `Tasks / Subtasks > Task 4: Tests` currently says "Test in `tests/test_cli_*.py`: flag passes through to engine, override works, no-flag = no change." Replace with the explicit test ID list (`UNIT-001/002`, `INT-001`–`INT-014`, `DOC-001`) so the dev agent has a deterministic checklist.
4. **Re-run `*trace` post-implementation.** Convert design coverage to executed coverage at the `*review` step.

## Risk Assessment

- **High Risk (no coverage):** none.
- **Medium Risk (partial coverage):** none. Every AC has ≥1 P0 or P1 mapping (or, for AC-8 / AC-9 / AC-11, multiple P2 mappings appropriate to their lower risk weight).
- **Low Risk:** AC-8 (multi-mode interactions) and AC-11 (docs) are P2-only — acceptable given the underlying risks (R6 medium, R7 low) are otherwise covered by P1 scenarios.

## Recommendations

1. **Story-side actions before implementation:**
   - Adopt AC-12 (bad-path UX) into the AC list.
   - Replace generic "Task 4: Tests" with the explicit test ID list from the test design.
   - Decide and document the canonical CLI doc file for AC-11 / DOC-001.

2. **Implementation-side actions:**
   - Follow the constructor-passthrough approach (`YAMLEngine(trace_file=...)`) per Technical Notes — avoids mutating the parsed YAML dict and pre-empts R1.
   - Extend `expand_env_vars` reuse to the CLI value, not the parsed YAML default, to satisfy AC-5 + INT-006.
   - Wrap `FileExporter` init or first write so that I/O failure becomes a `typer.BadParameter` referencing `--trace-file` (closes R5 / INT-013).

3. **Post-implementation actions:**
   - Re-run `*trace` to verify each planned scenario was actually authored.
   - Run `*nfr-assess` again; if INT-013 is green, the reliability gate flips from CONCERNS to PASS.
   - Run `*review` and fold the trace + NFR results into the gate file.

## Gate YAML Block (paste into qa-gate file under `trace`)

```yaml
trace:
  totals:
    requirements: 12
    full: 12
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md'
  uncovered: []
  notes: 'See docs/qa/assessments/TEA-DX-001.2-trace-20260501.md. AC-12 is NFR-recommended and not yet adopted in the story AC list — flagged in the trace gaps section. Coverage above is design coverage; re-run *trace post-implementation to confirm executed coverage.'
```

## Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.2-trace-20260501.md
```

## Sign-off

**Design coverage:** complete. All 11 story ACs plus the NFR-recommended AC-12 have ≥1 mapped scenario, all medium risks have defense-in-depth coverage, and all NFR concerns map to specific test IDs. **No critical gaps.** Trace can graduate from "design-complete" to "verified" once implementation lands and the planned `test_cli_trace_file.py` + `test_yaml_engine.py::TEA-DX-001.2-UNIT-002` tests are written and green.
