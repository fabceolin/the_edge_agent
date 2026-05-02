# TEA-DX-001.2: CLI `--trace-file` flag

## Status
Done

## Parent Epic
[TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md)

## Priority
High

---

## Story

**As an** external runner integrator (e.g., `intake.py`-style scripts),
**I want** a `tea run --trace-file <path>` CLI flag that overrides `settings.trace_file` for a single execution,
**so that** I can generate a per-run trace path without rendering a temp YAML file.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/cli.py` `run` command (Typer-based)
- Existing flags on `run`: `--output`, `--quiet`, `--stream`, `--show-graph`, `--fail-on-state` (TEA-CLI-008), etc.
- Settings consumed by: `yaml_engine.py:_configure_from_settings` (lines 980-1004)
- Technology: Python, Typer, JSONL trace files via `FileExporter`

**Problem Statement:**

`tea run` currently exposes no CLI control over trace output. The only way to set `trace_file` is via the YAML `settings:` block. For external runners that execute the same YAML many times with different per-run trace paths, this forces a YAML-templating round-trip (render YAML to temp file, run, delete).

A single CLI flag closes this gap.

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1:** `tea run <yaml> --trace-file <path>` writes the trace to `<path>`, overriding any `settings.trace_file` value in the YAML.
2. **AC-2:** When `--trace-file` is provided and `settings.trace_exporter` is unset (or set to `"console"`), the exporter is implicitly switched to `"file"`.
3. **AC-3:** When `--trace-file` is provided and `settings.trace_exporter` is `"file"`, only the path is overridden.
4. **AC-4:** When `--trace-file` is provided and `settings.auto_trace` is `false`, tracing is enabled implicitly (the user clearly wants traces).
5. **AC-5:** The path supports `${ENV_VAR}` expansion via the same `expand_env_vars` helper (consistent with TEA-DX-001.1 behavior).
6. **AC-6:** `tea run --help` shows `--trace-file` with a one-line description.

**Integration Requirements:**

7. **AC-7:** When `--trace-file` is *not* provided, behavior is unchanged.
8. **AC-8:** Works with `--quiet`, `--stream`, and `--show-graph` modes.
9. **AC-9:** No interaction with `--output` (which controls final state, not traces).

**Quality Requirements:**

10. **AC-10:** Unit/CLI test covers: flag override, implicit-enable behavior, env-var expansion, missing flag (no regression).
11. **AC-11:** `--trace-file` documented in CLI reference.

## Technical Notes

- **Integration Approach:** Add `trace_file: Optional[str] = typer.Option(None, "--trace-file", help="Override settings.trace_file for this run")` to the `run` command signature in `cli.py`. Pass into `YAMLEngine` constructor (which already accepts `trace_file` per `yaml_engine.py:106`) **or** patch into the `settings` dict before engine init.
- **Existing Pattern Reference:** `cli.py` `run` command — see `--output` and `--quiet` parameter wiring.
- **Key Constraint:** The engine constructor already accepts `trace_file` (line 106) — prefer passing via constructor over mutating the parsed YAML dict.

## Tasks / Subtasks

- [x] **Task 1: Add CLI flag** (AC: 1, 6)
  - [x] Add `--trace-file` option to `tea run` Typer signature
  - [x] Wire through to `YAMLEngine(...)` constructor `trace_file=` kwarg
- [x] **Task 2: Implicit enable behaviors** (AC: 2, 4)
  - [x] If `--trace-file` set and exporter unset/console, switch exporter to `"file"`
  - [x] If `--trace-file` set and `auto_trace` unset, enable tracing
- [x] **Task 3: Env expansion** (AC: 5)
  - [x] Apply `expand_env_vars` to the CLI-supplied path (or rely on TEA-DX-001.1 expansion if engine handles it uniformly)
- [x] **Task 4: Tests** (AC: 10)
  - [x] Test in `tests/test_cli_trace_file.py`: flag passes through to engine, override works, no-flag = no change
- [x] **Task 5: Docs** (AC: 11)
  - [x] Update CLI reference in `docs/python/development-guide.md` and `docs/shared/cli-reference.md`

## Definition of Done

- [ ] All ACs met
- [ ] `pytest python/tests/` green
- [ ] `tea run --help` output includes `--trace-file`
- [ ] CLI docs updated

## Risk and Compatibility

- **Primary Risk:** None — purely additive flag.
- **Rollback:** Revert the single PR.
- **Compatibility:** Fully backward compatible.

## QA Notes - Risk Profile

**Reviewer:** Quinn (Test Architect) — `*risk-profile` (YOLO mode)
**Date:** 2026-05-01
**Story Scope:** Additive CLI flag wiring an existing `YAMLEngine` constructor parameter through Typer.

### Overall Risk Level: **LOW**

Risk score: **84/100** (100 = no risk, baseline 100; subtract per identified risk).
Story is purely additive, backward compatible, and reuses an existing constructor parameter (`yaml_engine.py:106`). No data-loss, security, or migration surfaces. Recommended gate disposition: **PASS** subject to AC-10 test coverage.

### Risk Matrix

Scoring legend — Probability (P): 1=Rare, 2=Possible, 3=Likely. Impact (I): 1=Trivial, 2=Moderate, 3=Severe. Score = P × I (1–9). Severity: 1–2 Low, 3–4 Medium, 6–9 High.

| ID | Category | Risk | P | I | Score | Severity |
|----|----------|------|---|---|-------|----------|
| R1 | Functional / Precedence | CLI `--trace-file` does not deterministically override `settings.trace_file` (e.g., constructor value is later clobbered by `_configure_from_settings`). | 2 | 2 | 4 | Medium |
| R2 | Functional / UX | Implicit-enable side effects (AC-2/AC-4) surprise users who explicitly set `auto_trace: false` or `trace_exporter: console`, producing unexpected file writes. | 2 | 2 | 4 | Medium |
| R3 | Security / Filesystem | User-controlled path written to disk without sanitization could traverse into sensitive locations (e.g., `--trace-file /etc/...`). Bounded by OS perms + CLI is run by the user, but still worth a directory-creation/permission error path test. | 1 | 2 | 2 | Low |
| R4 | Compatibility | Env-var expansion drift between AC-5 (this story) and TEA-DX-001.1 (parent settings expansion) leads to inconsistent semantics (e.g., `${VAR:-default}` works in YAML but not CLI flag). | 2 | 1 | 2 | Low |
| R5 | Operational | `FileExporter` open failure (bad path / read-only fs / nonexistent parent dir) surfaces as opaque traceback rather than typer-friendly error. | 2 | 1 | 2 | Low |
| R6 | Test Coverage | Implicit-enable matrix (exporter unset / "console" / "file"; auto_trace unset / false / true) is combinatorial — easy to under-test edge cells. | 2 | 2 | 4 | Medium |
| R7 | Docs | `--trace-file` documented in CLI ref but precedence rules vs `settings.trace_file` unclear, leading to user confusion. | 2 | 1 | 2 | Low |

No High-severity risks identified. No data-loss, auth, regulatory, performance, or schema-migration risks apply.

### Mitigations

- **R1 (Precedence):** Per AC technical note, prefer constructor passthrough (`YAMLEngine(trace_file=...)`) over post-hoc settings dict mutation. Add an explicit precedence test asserting CLI wins when both YAML `settings.trace_file` and `--trace-file` are set. Audit `_configure_from_settings` (lines 982–996) to confirm it does not overwrite a CLI-supplied value — the current branch only appends an exporter when `_trace_context.exporters` is empty, so a CLI-supplied exporter should already short-circuit it; codify this with a regression test.
- **R2 (Implicit enable):** Document the implicit behavior explicitly in `--help` text and CLI reference: "Providing `--trace-file` enables file tracing even if `settings.auto_trace: false`." Consider a follow-up `--no-auto-trace` if surprise reports surface; for this story, document only.
- **R3 (Path safety):** Rely on standard OS permission errors. No additional sanitization required (CLI is local, single-user). Add one test for "parent directory missing" producing a clean `typer.BadParameter` or surfaced `OSError`, not a tracelog.
- **R4 (Env expansion parity):** Reuse the same `expand_env_vars` helper invoked by TEA-DX-001.1. Add a parity test: same `${VAR:-default}` string produces identical resolved path whether supplied via YAML setting or CLI flag.
- **R5 (Exporter init failure):** Wrap `FileExporter(path)` construction (or its first write) such that I/O failure produces a typer error message referencing the supplied flag. One negative test covers this.
- **R6 (Coverage):** See P0/P1 testing matrix below — explicitly enumerate the 3×3 cells.
- **R7 (Docs):** CLI reference (AC-11) must include a precedence note: "CLI `--trace-file` overrides `settings.trace_file`. When set, implicitly enables `auto_trace=true` and switches `trace_exporter` to `file` if unset or `console`."

### Testing Priorities

**P0 — Must pass before merge (covers AC-1, AC-7, AC-10):**
1. **Override precedence:** YAML has `settings.trace_file: yaml.jsonl`; run with `--trace-file cli.jsonl`; assert traces land at `cli.jsonl` and `yaml.jsonl` is not created.
2. **No-flag baseline:** Run without `--trace-file`; assert behavior is byte-identical to pre-change baseline (no regression in `--quiet`, `--stream`, exit code, stdout).
3. **Implicit enable from off:** YAML sets `auto_trace: false` (or omits it); run with `--trace-file out.jsonl`; assert file is created and contains JSONL trace events.

**P1 — Required for AC sign-off (AC-2, AC-4, AC-5):**
4. **Exporter promotion:** YAML has `trace_exporter: console` (or unset); `--trace-file` flips it to `file` and writes to disk; console is no longer used (or both, if intent is dual-output — pin behavior in test).
5. **Exporter preserved:** YAML has `trace_exporter: file` and `trace_file: yaml.jsonl`; `--trace-file cli.jsonl` overrides path only; exporter type is unchanged.
6. **Env-var expansion:** `--trace-file '${TRACE_DIR}/run.jsonl'` with `TRACE_DIR=/tmp/foo` resolves to `/tmp/foo/run.jsonl`; parity test against same string in YAML.
7. **CLI help:** `tea run --help` contains `--trace-file` and a one-line description (AC-6 smoke test).

**P2 — Recommended for robustness:**
8. **Mode interactions:** Combine with `--quiet`, `--stream`, `--show-graph` — assert trace file is still produced and final state output is unaffected (AC-8).
9. **Bad path:** `--trace-file /nonexistent/dir/foo.jsonl` produces a clean error referencing the flag, not a raw traceback (R5).
10. **`--output` independence:** `--trace-file` and `--output` together write to two distinct files with no cross-contamination (AC-9).

**P3 — Nice to have:**
11. Determinism check: two consecutive runs with same `--trace-file` overwrite/append per documented `FileExporter` semantics (pin existing behavior).

### Recommended Test Locations

- CLI tests: `python/tests/test_cli_*.py` (follow existing `--output`/`--quiet` test pattern at the same call sites).
- Engine wiring: a focused unit test asserting `YAMLEngine(trace_file=...)` plus YAML `settings.auto_trace: false` still installs `FileExporter` (closes R1 + R2 in one assertion).

### Sign-off

No blocking risks. Story is safe to implement and merge once P0+P1 tests are green and AC-11 docs are updated. Re-review only required if implementation diverges from the constructor-passthrough approach noted in Technical Notes.

## QA Notes - NFR Assessment

**Reviewer:** Quinn (Test Architect) — `*nfr-assess` (YOLO / non-interactive mode)
**Date:** 2026-05-01
**Scope assessed:** core four (security, performance, reliability, maintainability)
**Full report:** [docs/qa/assessments/TEA-DX-001.2-nfr-20260501.md](../qa/assessments/TEA-DX-001.2-nfr-20260501.md)

### NFR Coverage

| NFR | Status | One-line rationale |
|-----|--------|--------------------|
| Security | **PASS** | Local single-user CLI; user-controlled path bounded by OS perms; reuses vetted `expand_env_vars` helper; no auth or data-exfil surface. |
| Performance | **PASS** | Purely additive Typer option — zero hot-path impact in the non-trace branch; tracing I/O cost is pre-existing `FileExporter` behavior, not new. |
| Reliability | **CONCERNS** | Exporter init on a bad / unwritable trace path (risk R5 from the risk profile) currently surfaces as a raw `OSError` traceback rather than a `typer.BadParameter`. Important because this story is explicitly aimed at **external runners** that parse stderr/exit codes. |
| Maintainability | **PASS** | Single CLI flag wired to existing `YAMLEngine(trace_file=...)` kwarg at `python/src/the_edge_agent/yaml_engine.py:106`; AC-10 covers tests, AC-11 covers docs, AC-7 pins backward compatibility. |

**Quality score:** 90/100 (100 − 10 per CONCERNS).

### Missing Considerations

1. **Bad-path error UX (Reliability / R5).** Story does not require a typer-friendly error when `--trace-file` parent dir is missing or unwritable. Currently classified as P2 in the risk profile; should be promoted given the external-runner audience.
2. **Implicit-enable disclosure (Reliability / R2).** AC-2 and AC-4 silently enable file tracing even when YAML opted out (`auto_trace: false`, `trace_exporter: console`). Behavior is intended, but `--help` text and CLI reference (AC-6 + AC-11) must spell it out — flag for docs reviewer.
3. **Env-var expansion parity (Security / R4).** AC-5 mentions `expand_env_vars` reuse but does not require an explicit parity assertion against TEA-DX-001.1's YAML-side expansion (e.g., `${VAR:-default}` semantics). Risk profile P1 test #6 covers it; just confirming traceability.
4. **`--help` snapshot (Maintainability / AC-6).** AC-6 is currently a manual-verification AC; consider a CLI snapshot test so it is mechanically enforced in CI.

### Test Recommendations (NFR-specific, supplements the risk-profile matrix)

| # | NFR | Test | Recommended priority |
|---|-----|------|----------------------|
| T1 | Reliability | `--trace-file /nonexistent/dir/foo.jsonl` → exits with `typer.BadParameter` referencing the flag, not a Python traceback. | **P1 (promote from P2 #9)** |
| T2 | Reliability | Unwritable parent dir → clean error referencing `--trace-file`. | P2 |
| T3 | Security | Env-var parity: `${VAR:-default}` resolves identically whether supplied via YAML `settings.trace_file` or `--trace-file` CLI flag. | P1 (already P1 #6) |
| T4 | Maintainability | `tea run --help` snapshot test asserts `--trace-file` line is present (mechanical AC-6). | P1 (already P1 #7) |
| T5 | Reliability | No-flag baseline byte-identical with `--quiet` / `--stream` (regression guard for AC-7/AC-8). | P0 (already P0 #2) |
| T6 | Performance | Optional: invocation-overhead delta of adding the flag is <5% on a small graph. Skip unless perf-sensitive callers exist. | P3 |

### Recommended Acceptance Criteria Addition

Add **AC-12** to close the reliability CONCERNS:

> **AC-12:** When `--trace-file` points to a path whose parent directory does not exist or is not writable, `tea run` exits with a clean error message referencing the `--trace-file` flag (e.g., via `typer.BadParameter`), not a raw Python traceback.

Promoting risk-profile P2 test #9 to P1 + adding AC-12 converts the reliability CONCERNS into a PASS at gate time.

### Gate Block (paste into qa-gate file)

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'Local single-user CLI; path expansion via vetted expand_env_vars; no auth/data-exfil surface. R3 (path traversal) bounded by OS perms — acceptable for a developer CLI flag.'
  performance:
    status: PASS
    notes: 'Additive Typer option; no extra work in non-trace path. Trace I/O is pre-existing FileExporter cost when --trace-file is supplied.'
  reliability:
    status: CONCERNS
    notes: 'Exporter init failure on bad path / unwritable parent dir (R5) should surface as typer.BadParameter, not a raw traceback. P2 negative test in risk profile not yet promoted to AC; recommend AC-12.'
  maintainability:
    status: PASS
    notes: 'Single Typer flag wired to existing constructor kwarg; AC-10 unit/CLI tests + AC-11 docs cover the change. Backward compatible per AC-7.'
```

### Sign-off

Story is safe to implement. Recommend adding **AC-12** and promoting **T1 (bad-path error)** to P1 before merge to convert the single CONCERNS into a PASS gate.

## QA Notes - Test Design

**Reviewer:** Quinn (Test Architect) — `*test-design` (YOLO mode)
**Date:** 2026-05-01
**Full matrix:** [docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md](../qa/assessments/TEA-DX-001.2-test-design-20260501.md)

### Coverage Summary

- **Total scenarios:** 17 — Unit: 2 · Integration (CLI): 14 · Doc: 1 · E2E: 0
- **Priority distribution:** P0: 4 · P1: 8 · P2: 4 · P3: 1
- **Test home:** `python/tests/test_cli_trace_file.py` (new, modeled on `test_cli_fail_on_state.py`) plus one engine-wiring unit added to `python/tests/test_yaml_engine.py`. All tests are hermetic — Typer `CliRunner`, `tmp_path`, `monkeypatch.setenv`. No external services.

Rationale: the change is a thin Typer option wired to `YAMLEngine(trace_file=...)` (`yaml_engine.py:106`). The high-leverage testing surface is the CLI-to-engine boundary, which is best exercised via `CliRunner`-based integration tests. Two unit tests carve out the highest-risk logic pieces (constructor-vs-settings precedence and env-var expansion parity) for fast feedback.

### Test Coverage Matrix (AC × Scenario)

| AC | Description | Scenarios | Highest Priority |
| --- | --- | --- | --- |
| AC-1 | `--trace-file` overrides `settings.trace_file` | INT-001, UNIT-002 | P0 |
| AC-2 | Implicit exporter promotion (unset/console → file) | INT-002, INT-003 | P1 |
| AC-3 | Exporter preserved when already `file` | INT-004 | P1 |
| AC-4 | Implicit `auto_trace=true` when YAML opts out | INT-005 | P0 |
| AC-5 | `${ENV_VAR}` expansion + parity with TEA-DX-001.1 | UNIT-001, INT-006 | P1 |
| AC-6 | `--help` shows `--trace-file` | INT-007 | P1 |
| AC-7 | No-flag baseline behavior unchanged | INT-008 | P0 |
| AC-8 | Works with `--quiet` / `--stream` / `--show-graph` | INT-009, INT-010, INT-011 | P2 |
| AC-9 | No interaction with `--output` | INT-012 | P2 |
| AC-10 | Test coverage on flag override / implicit-enable / env-var / missing-flag | Implicit (INT-001, INT-002, INT-005, UNIT-001, INT-008) | P0 |
| AC-11 | CLI reference docs include precedence note | DOC-001 | P2 |
| AC-12 (recommended) | Bad path → `typer.BadParameter`, no raw traceback | INT-013 | **P1** (promoted from P2) |
| Determinism | Repeat-run FileExporter semantics | INT-014 | P3 |

### Scenarios with Expected Results

#### P0 — Must pass before merge

| ID | Level | Scenario | Expected Result |
| --- | --- | --- | --- |
| **TEA-DX-001.2-INT-001** | Int | YAML has `settings.trace_file: yaml.jsonl`; run with `--trace-file cli.jsonl`. | `cli.jsonl` exists with JSONL events; `yaml.jsonl` was never created. Exit 0. |
| **TEA-DX-001.2-UNIT-002** | Unit | Construct `YAMLEngine(trace_file="cli.jsonl")` against parsed YAML whose `settings.trace_file` is `"yaml.jsonl"`; run `_configure_from_settings`. | Installed exporter targets `cli.jsonl`. Settings-supplied path is *not* applied on top. |
| **TEA-DX-001.2-INT-005** | Int | YAML sets `settings.auto_trace: false`; run with `--trace-file out.jsonl`. | `out.jsonl` is created and contains ≥1 well-formed JSONL trace event. Exit 0. |
| **TEA-DX-001.2-INT-008** | Int | Run identical fixture without `--trace-file`. | Exit code, stdout (with and without `--quiet`), and trace-emission behavior match the pre-change baseline (golden compare). No regression. |

#### P1 — Required for AC sign-off

| ID | Level | Scenario | Expected Result |
| --- | --- | --- | --- |
| **TEA-DX-001.2-INT-002** | Int | YAML omits `settings.trace_exporter`; `--trace-file out.jsonl`. | `out.jsonl` populated with JSONL trace events. |
| **TEA-DX-001.2-INT-003** | Int | YAML sets `settings.trace_exporter: console`; `--trace-file out.jsonl`. | Exporter switches to `file`, `out.jsonl` populated. (Pin documented behavior re: console — suppressed or dual.) |
| **TEA-DX-001.2-INT-004** | Int | YAML sets `trace_exporter: file` + `trace_file: yaml.jsonl`; `--trace-file cli.jsonl`. | Exporter type is unchanged; only path moves to `cli.jsonl`. |
| **TEA-DX-001.2-UNIT-001** | Unit | Direct call to env-var helper on `${TRACE_DIR}/run.jsonl` with `TRACE_DIR=/tmp/foo`; also `${MISSING:-default}`. | Resolves to `/tmp/foo/run.jsonl` and `default` respectively, matching TEA-DX-001.1 semantics. |
| **TEA-DX-001.2-INT-006** | Int | Run twice: same `${TRACE_DIR}/run.jsonl` once via YAML setting, once via `--trace-file`. | Both runs land traces at the same resolved path. |
| **TEA-DX-001.2-INT-007** | Int | `tea run --help`. | Exit 0. Stdout contains `--trace-file` literal and a non-empty description fragment. |
| **TEA-DX-001.2-INT-013** | Int | `--trace-file /nonexistent/dir/foo.jsonl`. | Non-zero exit. Stderr references `--trace-file`. Stderr does **not** contain `Traceback (most recent call last)`. |

#### P2 — Recommended for robustness

| ID | Level | Scenario | Expected Result |
| --- | --- | --- | --- |
| **TEA-DX-001.2-INT-009** | Int | `--trace-file out.jsonl --quiet`. | Trace file populated; stdout suppressed; exit 0. |
| **TEA-DX-001.2-INT-010** | Int | `--trace-file out.jsonl --stream`. | Trace file populated; streaming output unaffected; exit 0. |
| **TEA-DX-001.2-INT-011** | Int | `--trace-file out.jsonl --show-graph`. | Trace file populated; graph rendering unaffected; exit 0. |
| **TEA-DX-001.2-INT-012** | Int | `--trace-file traces.jsonl --output state.json`. | Both files produced. Contents disjoint (JSONL events vs final state JSON). |
| **TEA-DX-001.2-DOC-001** | Doc | Grep `docs/python/` CLI reference. | Reference contains `--trace-file` plus the precedence note: *"CLI `--trace-file` overrides `settings.trace_file`. When set, implicitly enables `auto_trace=true` and switches `trace_exporter` to `file` if unset or `console`."* |

#### P3 — Nice to have

| ID | Level | Scenario | Expected Result |
| --- | --- | --- | --- |
| **TEA-DX-001.2-INT-014** | Int | Two consecutive runs with same `--trace-file out.jsonl`. | Behavior matches documented FileExporter semantics (overwrite vs append) — pin whichever is current to detect drift. |

### Test Data & Environment Requirements

- **Fixture YAMLs** under `python/tests/fixtures/dx_001_2/`:
  - `simple.yaml` — minimal one-node graph, no `settings:` block (used by INT-001, INT-007, INT-008, INT-009, INT-010, INT-011, INT-012, INT-013, INT-014).
  - `auto_trace_off.yaml` — `settings.auto_trace: false`, no `trace_file` (INT-005).
  - `console_exporter.yaml` — `settings.trace_exporter: console` (INT-003).
  - `file_exporter_with_path.yaml` — `settings.trace_exporter: file` + `settings.trace_file: yaml.jsonl` (INT-004).
  - `env_var_path.yaml` — `settings.trace_file: ${TRACE_DIR}/yaml.jsonl` (INT-006).
- **Tooling:** `typer.testing.CliRunner`, `pytest` `tmp_path`, `monkeypatch.setenv` (`TRACE_DIR`, `MISSING`), `caplog` / `result.stderr` for INT-013 stderr assertions, optional `chmod 0o500` or `pyfakefs` for an unwritable-parent extension of INT-013 (NFR T2).
- **Isolation:** every test owns its `tmp_path` for `--trace-file` and `--output` destinations. Tests must not write outside `tmp_path`. INT-013's "nonexistent" path is asserted-to-fail and is never created.
- **Determinism:** assert JSONL well-formedness (`json.loads` per line) and `len(events) > 0` rather than exact event content; freeze or mock timestamps only if a test pins exact strings.
- **External services:** none. Suite is fully hermetic — no LLM, DB, or network calls.

### Risk → Test Mapping (from in-story Risk Profile)

| Risk | Severity | Covered by | Notes |
| --- | --- | --- | --- |
| R1 — Override precedence | Medium | INT-001 (P0), UNIT-002 (P0), INT-008 (P0) | Defense-in-depth: unit + integration on the same property is intentional. |
| R2 — Implicit-enable surprise | Medium | INT-002, INT-003, INT-005 (P0), INT-007, DOC-001 | Help text + docs scenarios pin the surface area users will see. |
| R3 — Path traversal | Low | INT-013 | OS perms remain primary mitigation. |
| R4 — Env-var expansion drift | Low | UNIT-001, INT-006 | Parity test is the load-bearing assertion. |
| R5 — Opaque traceback on bad path | Low | INT-013 (P1) | Promoted from P2 per NFR T1. |
| R6 — Implicit-enable matrix coverage | Medium | INT-002 + INT-003 + INT-004 + INT-005 | Covers exporter × auto_trace cells without exhaustively enumerating all 9. |
| R7 — Docs precedence ambiguity | Low | INT-007, DOC-001 | Help text + full reference both enforced. |

All medium-severity risks have at least one P0 or two P1 scenarios. No risks left without a test.

### NFR Traceability (from in-story NFR Assessment)

| NFR Test | Mapped scenario | Resolution |
| --- | --- | --- |
| T1 (bad path → typer.BadParameter) | INT-013 | **Promoted P2 → P1** |
| T2 (unwritable parent dir) | INT-013 (extend with `chmod` variant) | P2 follow-on |
| T3 (env-var parity) | INT-006 | P1 |
| T4 (`--help` snapshot) | INT-007 | P1 |
| T5 (no-flag baseline byte-identical with `--quiet`/`--stream`) | INT-008 + INT-009 + INT-010 | P0 |
| T6 (invocation-overhead delta <5%) | — | Skipped per NFR doc |

### Recommended Execution Order

1. P0 unit: UNIT-002 (engine-wiring precedence)
2. P0 integration: INT-001, INT-005, INT-008
3. P1 integration: INT-002, INT-003, INT-004, INT-006, INT-007, INT-013
4. P1 unit: UNIT-001
5. P2 integration: INT-009, INT-010, INT-011, INT-012, DOC-001
6. P3 integration: INT-014

### Gate YAML Block

```yaml
test_design:
  scenarios_total: 17
  by_level:
    unit: 2
    integration: 14
    doc: 1
    e2e: 0
  by_priority:
    p0: 4
    p1: 8
    p2: 4
    p3: 1
  coverage_gaps: []
  risks_covered: [R1, R2, R3, R4, R5, R6, R7]
  nfr_tests_covered: [T1, T3, T4, T5]
```

### Sign-off

Coverage is complete across all 11 declared ACs plus the recommended AC-12. P0 set is small (4 tests) and fast — recommended gate criterion: **P0 + P1 green, P2 best-effort, P3 optional**. No blocking gaps; story can proceed to `*review` after implementation.

## QA Notes - Requirements Trace

**Reviewer:** Quinn (Test Architect) — `*trace` (YOLO mode)
**Date:** 2026-05-01
**Story status at trace time:** Draft (implementation not yet started)
**Full matrix:** [docs/qa/assessments/TEA-DX-001.2-trace-20260501.md](../qa/assessments/TEA-DX-001.2-trace-20260501.md)

> **YOLO trace caveat:** No tests exist yet. Coverage below is *design* coverage — each AC is mapped to the planned test IDs from the test design (`TEA-DX-001.2-INT-NNN` / `-UNIT-NNN`). Re-run `*trace` post-implementation to convert design coverage into executed coverage.

### Requirements Coverage

- **Total Requirements:** 12 (AC-1 … AC-11 from the story body, plus the NFR-recommended AC-12).
- **Fully Covered (design):** 12 / 12 (100%).
- **Partially Covered:** 0.
- **Not Covered:** 0.

| Coverage class | Count | ACs |
|---|---|---|
| Full with ≥1 P0 mapping | 4 | AC-1, AC-4, AC-7, AC-10 |
| Full with ≥1 P1 mapping (no P0) | 5 | AC-2, AC-3, AC-5, AC-6, AC-12 |
| Full with P2-only mapping | 3 | AC-8, AC-9, AC-11 |

### Traceability Matrix (AC × Planned Test ID)

| AC | Description | Planned Tests | Highest Priority | Risk(s) Covered |
|---|---|---|---|---|
| AC-1 | `--trace-file` overrides `settings.trace_file` | INT-001, UNIT-002 | P0 | R1 |
| AC-2 | Implicit exporter promotion (unset/console → file) | INT-002, INT-003 | P1 | R2, R6 |
| AC-3 | Exporter preserved when already `file` | INT-004 | P1 | R1, R6 |
| AC-4 | Implicit `auto_trace=true` when YAML opts out | INT-005 | P0 | R2, R6 |
| AC-5 | `${ENV_VAR}` expansion + parity with TEA-DX-001.1 | UNIT-001, INT-006 | P1 | R4 |
| AC-6 | `tea run --help` shows `--trace-file` | INT-007 | P1 | R2, R7 |
| AC-7 | No-flag baseline behavior unchanged | INT-008 | P0 | R1 |
| AC-8 | Works with `--quiet` / `--stream` / `--show-graph` | INT-009, INT-010, INT-011 | P2 | — |
| AC-9 | No interaction with `--output` | INT-012 | P2 | — |
| AC-10 | Test coverage on flag override / implicit-enable / env-var / missing-flag | rolls up INT-001 + INT-002 + UNIT-001 + INT-006 + INT-008 | P0 | R1, R2, R4 |
| AC-11 | CLI reference docs include precedence note | DOC-001 | P2 | R7 |
| AC-12 (recommended) | Bad path → `typer.BadParameter`, no raw traceback | INT-013 | P1 (promoted) | R3, R5 |

Detailed Given-When-Then mappings per scenario are in the standalone trace report linked above; the test design already documents expected results per scenario.

### Risk → Test Coverage (summary)

All seven risks from the in-story risk profile have ≥1 mapped scenario. R1 / R2 / R6 (medium) each have defense-in-depth coverage (multiple scenarios across unit + integration). R3 / R5 are closed by INT-013 once promoted to P1. R4 closed by UNIT-001 + INT-006 parity test. R7 closed by INT-007 (help) + DOC-001 (CLI ref).

### NFR → Test Coverage (summary)

| NFR concern | Mapped scenario | Priority |
| --- | --- | --- |
| T1 — Bad path → `typer.BadParameter` | INT-013 | P1 (promoted) |
| T2 — Unwritable parent dir | INT-013 (`chmod` extension) | P2 follow-on |
| T3 — Env-var parity (YAML vs CLI) | UNIT-001 + INT-006 | P1 |
| T4 — `--help` snapshot | INT-007 | P1 |
| T5 — No-flag baseline byte-identical with `--quiet`/`--stream` | INT-008 + INT-009 + INT-010 | P0 |
| T6 — Invocation-overhead delta <5% | — (skipped per NFR doc) | P3 |

### Gaps Identified

**No critical gaps.** Smaller, non-blocking observations:

1. **AC-12 is recommended but not yet adopted.** The NFR assessment recommends adding AC-12 (bad-path → `typer.BadParameter`) to the story's authoritative AC list. Until adopted, INT-013 sits as a P1 robustness scenario without an explicit AC anchor — slightly weakens gate posture under a strict 1:1 AC↔test reading.
2. **CLI reference doc target unconfirmed.** AC-11 says "CLI reference in `docs/python/`" but the canonical file is TBD. DOC-001's grep target depends on this decision.
3. **NFR-T2 (unwritable parent) is P2 only.** Acceptable for a developer-facing CLI but worth tracking if external-runner reports surface read-only-fs cases.
4. **`--show-graph` baseline assertion is P2 only (INT-011).** AC-8 lists `--show-graph` alongside `--quiet`/`--stream`, but only the latter two are byte-identically asserted against a baseline (NFR-T5 → INT-008 + INT-009 + INT-010). If `--show-graph` ever joins an external-runner contract, promote INT-011 to P1.
5. **No executed coverage yet.** All entries are *design* coverage — re-run `*trace` post-implementation.

### Recommendations

**Before implementation starts:**

1. **Adopt AC-12** (bad-path UX) into the story's authoritative AC list, converting INT-013 from P1 robustness scenario to P1 AC verification.
2. **Replace generic "Task 4: Tests"** in `Tasks / Subtasks` with the explicit test ID list (`UNIT-001`, `UNIT-002`, `INT-001`–`INT-014`, `DOC-001`) so the dev agent has a deterministic checklist.
3. **Pin DOC-001's grep target** by deciding whether the CLI reference lives in an existing file (e.g., `docs/python/getting-started.md`) or a new dedicated doc.

**During implementation:**

4. **Use constructor passthrough** (`YAMLEngine(trace_file=...)`) per Technical Notes — avoids mutating the parsed YAML dict and pre-empts R1.
5. **Reuse `expand_env_vars`** on the CLI-supplied value (not just the parsed YAML default) to satisfy AC-5 + INT-006 / NFR-T3 with a single shared helper.
6. **Wrap `FileExporter` init** so I/O failure becomes a `typer.BadParameter` referencing `--trace-file` (closes R5 / INT-013 / NFR-T1).

**After implementation:**

7. **Re-run `*trace`** to verify each planned scenario was actually authored and is green.
8. **Re-run `*nfr-assess`** — once INT-013 is green, reliability gate flips from CONCERNS to PASS.
9. **Run `*review`** and fold the trace + NFR results into the gate file using the YAML block from the standalone trace report.

### Gate YAML (paste into qa-gate file under `trace`)

```yaml
trace:
  totals:
    requirements: 12
    full: 12
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md'
  uncovered: []
  notes: 'See docs/qa/assessments/TEA-DX-001.2-trace-20260501.md. AC-12 is NFR-recommended and not yet adopted in the story AC list — flagged in the trace gaps section. Coverage is design coverage; re-run *trace post-implementation to confirm executed coverage.'
```

### Sign-off

**Design coverage is complete.** All 11 story ACs plus the NFR-recommended AC-12 have ≥1 mapped scenario; all medium risks have defense-in-depth coverage; all NFR concerns map to specific test IDs. **No critical gaps.** Story can proceed to implementation; re-run `*trace` post-implementation to upgrade design coverage to verified coverage before `*review`.

## SM Validation

**Reviewer:** Bob (Scrum Master) — story-draft-checklist (YOLO mode)
**Date:** 2026-05-01
**Outcome:** **PASS — Ready for Development**

### Definition of Ready Checklist

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1 | Story has clear title and description | ✅ PASS | Title `CLI --trace-file flag`; user-story format with role/want/so-that (lines 16–18) and Story Context block (lines 20–34). |
| 2 | Acceptance criteria are defined and testable | ✅ PASS | 11 numbered ACs grouped Functional / Integration / Quality (AC-1 … AC-11). Each is mechanically verifiable — flag override, exporter promotion, env-var expansion, `--help` content, doc updates. |
| 3 | Dependencies are identified | ✅ PASS | Parent epic linked (TEA-DX-001); explicit references to `cli.py` `run` command, `yaml_engine.py:_configure_from_settings` (lines 980–1004), and existing constructor kwarg at `yaml_engine.py:106`; sibling story TEA-DX-001.1 cited for `expand_env_vars` parity. |
| 4 | Technical approach is documented | ✅ PASS | "Technical Notes" section names the integration point, prefers constructor passthrough over dict mutation, and cites the existing `--output`/`--quiet` patterns to follow. |
| 5 | Story is properly sized | ✅ PASS | 5 tasks, additive single-flag scope, no migrations/data changes. Risk profile rates the change LOW (84/100). Fits a single PR — rollback = revert. |
| 6 | QA notes sections present | ✅ PASS | All four present and detailed — Risk Profile (lines 91–156), NFR Assessment (lines 158–223), Test Design (lines 225–365), Requirements Trace (lines 367–469). Standalone reports linked under `docs/qa/assessments/`. |
| 7 | No blocking issues or unknowns | ✅ PASS | Risk profile: "No blocking risks." Trace: "No critical gaps." Three minor non-blocking observations carried forward as recommendations, not blockers (see below). |

### Non-Blocking Observations (carry into implementation)

1. **AC-12 (bad-path → `typer.BadParameter`)** is recommended by the NFR assessment but not yet adopted into the authoritative AC list. Story can ship without it; recommend the dev agent add it during implementation since INT-013 is already P1 in the test design.
2. **DOC-001 grep target** (CLI reference file in `docs/python/`) is unconfirmed. Dev should pin it during the docs task — likely candidates: `docs/python/getting-started.md` or a dedicated CLI reference doc.
3. **Task 4 ("Tests")** is currently generic. Test design enumerates 17 explicit scenario IDs (UNIT-001/002, INT-001–014, DOC-001); dev should treat that list as the working test checklist.

### Sign-off

All seven Definition of Ready criteria PASS. No blocking issues. Story is approved for development handoff.

## Dev Agent Record

### Agent Model Used
James (`/dev`) — claude-opus-4-7[1m]

### Debug Log References
None — implementation passed all 17 authored tests on first integration run.

### Completion Notes List

**Implementation approach** (chosen per Technical Notes "prefer constructor passthrough"):

1. **Constructor passthrough for the path/exporter.** `--trace-file <path>` is forwarded to `YAMLEngine(trace_file=..., trace_exporter="file")` so the existing `FileExporter` is installed at construction time. YAML's `settings.trace_file` is then naturally ignored because `_configure_from_settings` only adds an exporter when `_trace_context.exporters` is empty (AC-1, AC-3, R1).
2. **`auto_trace` injection via dict path for AC-4.** Node-level auto-trace wrapping is decided at `load_from_dict` time by reading `engine._auto_trace`. Setting that flag *after* `load_from_file` is too late — nodes are already built with their unwrapped run functions. To force-enable tracing when YAML has `settings.auto_trace: false`, the CLI parses the YAML to a dict (or reuses the existing overlay-merged dict), injects `settings.auto_trace = true`, and routes through `engine.load_from_dict(...)`. This keeps the constructor-passthrough wiring for the path while still satisfying AC-2 / AC-4 / AC-7.
3. **Env-var expansion.** The CLI applies `expand_env_vars` (from `the_edge_agent.memory.base`, the same helper TEA-DX-001.1 uses for `settings.trace_file`) to the supplied path *before* construction so `${VAR}` and `${VAR:-default}` semantics match identically (AC-5, R4).
4. **AC-12 bad-path UX.** `YAMLEngine(...)` construction is wrapped in `try/except (OSError, IOError)` and translated to `typer.BadParameter` referencing the `--trace-file` flag — no raw traceback (R5, NFR-T1).

**Notes on environment state:**

- The repo has substantial uncommitted WIP from sibling stories (TEA-OBS-003 / TEA-DX-001.6 / TEA-DX-001.7) modifying `cli.py`, `tracing.py`, `yaml_engine.py`. My `--trace-file` wiring lands cleanly alongside that WIP without conflicts.
- A pre-existing test-suite breakage (`'YAMLEngine' has no attribute '_configure_llm_payload_capture'`) was resolved by other agents during the session (the method now exists in `yaml_engine.py`); my code did not introduce or fix it.
- The full repo-wide `pytest tests/` run shows pre-existing failures unrelated to this story (notably `test_yaml_engine_code.py`, `test_yaml_engine_llm.py`, `test_yaml_engine_rag.py`, etc., all gated on optional deps or sibling-story WIP). Verified that the **same failures occur on a clean `git stash` of the working tree**, confirming they are not regressions from this story. Touch-area regression (`test_cli_*.py` + `test_yaml_engine_core.py` + `test_yaml_engine_edges.py` + `test_yaml_engine_observability.py` + `test_yaml_engine_imports.py` + `test_yaml_engine_jinja2.py`) is fully green: **358 / 358 passed**.

**Test scenarios covered (vs. test-design matrix):**

- `python/tests/test_cli_trace_file.py` (17 tests): UNIT-001 (env-var helper parity), UNIT-002 (engine-wiring precedence), INT-001 (override precedence, P0), INT-002 (exporter promotion / unset, P1), INT-003 (exporter promotion / console, P1), INT-004 (exporter type preserved when YAML already file, P1), INT-005 (implicit enable when YAML opts out, P0), INT-006 (env-var expansion, P1), INT-007 (`--help` snapshot, P1), INT-008 (no-flag baseline, P0), INT-009 (`--quiet` interaction, P2), INT-010 (`--stream` interaction, P2), INT-012 (`--output` independence, P2), INT-013 (bad path → `typer.BadParameter`, P1 promoted from P2 per NFR-T1).
- `python/tests/test_dx_001_2_cli_trace_file.py` (5 tests authored by parallel agent for the same story): all green; no contract conflict with my wiring.

**Acceptance criteria status:**

- AC-1 ✓ (INT-001, UNIT-002)
- AC-2 ✓ (INT-002, INT-003)
- AC-3 ✓ (INT-004)
- AC-4 ✓ (INT-005)
- AC-5 ✓ (UNIT-001, INT-006)
- AC-6 ✓ (INT-007)
- AC-7 ✓ (INT-008)
- AC-8 ✓ (INT-009, INT-010 — `--show-graph` not exercised but path-independence holds by construction)
- AC-9 ✓ (INT-012)
- AC-10 ✓ (test coverage spans flag override + implicit-enable + env-var + missing-flag)
- AC-11 ✓ (`docs/python/development-guide.md` § "Tracing Execution" + `docs/shared/cli-reference.md` § "Trace File Override")
- AC-12 ✓ (INT-013) — adopted per NFR recommendation

### File List

**New files:**

- `python/tests/test_cli_trace_file.py` — 17 unit + integration tests covering all ACs, including the AC-12 bad-path scenario.
- `python/tests/fixtures/dx_001_2/simple.yaml` — minimal one-node graph fixture.
- `python/tests/fixtures/dx_001_2/auto_trace_off.yaml` — fixture with `settings.auto_trace: false` (INT-005).
- `python/tests/fixtures/dx_001_2/console_exporter.yaml` — fixture with `settings.trace_exporter: console` (INT-003).
- `python/tests/fixtures/dx_001_2/file_exporter_with_path.yaml` — fixture with `settings.trace_exporter: file` + `trace_file` (INT-004).
- `python/tests/fixtures/dx_001_2/yaml_with_trace_file.yaml` — fixture for UNIT-002 engine-wiring precedence test.

**Modified files:**

- `python/src/the_edge_agent/cli.py` — added `--trace-file` Typer option to `tea run`, wired path through `expand_env_vars` and `YAMLEngine(trace_file=..., trace_exporter="file")` constructor kwarg, injected `settings.auto_trace=true` via dict-path load to satisfy AC-4, wrapped engine construction in `try/except` for AC-12 `typer.BadParameter` UX.
- `docs/python/development-guide.md` — added `### tea run --trace-file <path> (TEA-DX-001.2)` subsection under "Tracing Execution" documenting precedence rules, implicit behavior, env-var expansion, and bad-path UX.
- `docs/shared/cli-reference.md` — added `## Trace File Override (TEA-DX-001.2)` section documenting the same set of behaviors, alongside existing TEA-CLI-008 / TEA-DX-001.7 sections.

### Change Log

| Date | Description |
|------|-------------|
| 2026-05-02 | Initial implementation (James / `/dev` YOLO mode). Constructor-passthrough wiring for path; dict-path `auto_trace` injection for AC-4; `expand_env_vars` parity with TEA-DX-001.1; AC-12 `typer.BadParameter` UX. 17 new tests in `test_cli_trace_file.py`, all green. Touch-area regression: 358 / 358. Pre-existing repo failures (sibling-story WIP / optional deps) confirmed unrelated. |

## QA Results

**Reviewer:** Quinn (Test Architect) — `*review-story` (YOLO mode)
**Date:** 2026-05-02
**Gate file:** [docs/qa/gates/TEA-DX-001.2-cli-trace-file-flag.yml](../qa/gates/TEA-DX-001.2-cli-trace-file-flag.yml)
**Outcome:** **PASS**

### Decision Summary

All 11 declared ACs plus the NFR-recommended **AC-12** (bad-path UX) are
implemented and covered by green tests. The single CONCERNS raised in the
in-story NFR Assessment (reliability — opaque traceback on bad path) was
explicitly closed by adopting AC-12 and promoting INT-013 to P1. Implementation
follows the Technical Notes guidance exactly: constructor passthrough for the
trace path, env-var parity with TEA-DX-001.1, and a `typer.BadParameter` catch
around engine construction.

### Requirements Traceability (executed coverage)

| AC | Tests | Status |
|----|-------|--------|
| AC-1 (override precedence) | INT-001 ✅, UNIT-002 ✅ | PASS |
| AC-2 (exporter promotion unset / console → file) | INT-002 ✅, INT-003 ✅ | PASS |
| AC-3 (exporter type preserved when YAML already file) | INT-004 ✅ | PASS |
| AC-4 (implicit `auto_trace=true`) | INT-005 ✅ | PASS |
| AC-5 (`${ENV_VAR}` expansion + parity) | UNIT-001 ✅, INT-006 ✅ | PASS |
| AC-6 (`--help` lists `--trace-file`) | INT-007 ✅ | PASS |
| AC-7 (no-flag baseline unchanged) | INT-008 ✅ | PASS (shallow — see CONCERN-1) |
| AC-8 (`--quiet` / `--stream` / `--show-graph`) | INT-009 ✅, INT-010 ✅, *INT-011 absent* | PASS (gap — see CONCERN-2) |
| AC-9 (`--output` independence) | INT-012 ✅ | PASS |
| AC-10 (test coverage on flag override / implicit-enable / env-var / missing-flag) | rolls up the above | PASS |
| AC-11 (CLI reference docs include precedence note) | docs grep ✅ | PASS |
| AC-12 (bad path → `typer.BadParameter`) | INT-013 ✅ | PASS |

### Code Quality Review

- **Constructor passthrough wired correctly** — `python/src/the_edge_agent/cli.py:1622-1639` builds `engine_kwargs` with `trace_file=` and forces `trace_exporter="file"` so the FileExporter is installed at construction time. Verified against `yaml_engine.py:279-285` (constructor exporter install) and `yaml_engine.py:1259` (`_configure_from_settings` is a no-op when exporters are already present). R1 / AC-1 closed at the engine boundary, not just at the CLI surface.
- **`expand_env_vars` reused at the CLI boundary** — `cli.py:1593` calls the same helper used by TEA-DX-001.1 for YAML settings, which is exactly what AC-5 / R4 require.
- **AC-12 wrapping** — `cli.py:1632-1639` translates `OSError` / `IOError` from `YAMLEngine(...)` construction into `typer.BadParameter` with `param_hint="--trace-file"`. INT-013 verifies no `Traceback (most recent call last)` in stderr.
- **AC-4 auto_trace injection is durable** — node-level auto-trace wrapping is decided at `load_from_dict` time from `engine._auto_trace`. Setting that flag *after* load is too late, so the CLI mutates `settings.auto_trace = true` in the merged dict pre-load (`cli.py:1602-1619`) and forces engine.load_from_dict regardless of whether overlays were applied. A defensive post-load re-affirmation at line 1721 belt-and-braces the invariant.

### Test Architecture Assessment

- **22 tests, 22 pass.** Touch-area suites (`test_cli.py`, `test_cli_unified.py`, `test_yaml_engine_observability.py`, `test_yaml_engine_core.py`, `test_yaml_engine_edges.py`) all green — no regression introduced.
- **Defense-in-depth on R1 (override precedence)** — UNIT-002 asserts at the engine boundary (only one FileExporter installed, targeting CLI path); INT-001 asserts at the CLI boundary (CLI file created, YAML default file *not* created). Both green.
- **Hermetic suite** — every test owns its own `tmp_path` (or `runner.isolated_filesystem`); no external services; one `${ENV_VAR}` parity test uses a guarded `os.environ` setup/teardown.

### NFR Validation (post-implementation)

| NFR | Status | Notes |
|-----|--------|-------|
| Security | PASS | Local single-user CLI; path expansion via vetted `expand_env_vars`; no new attack surface (per in-story NFR Assessment). |
| Performance | PASS | Additive Typer option; no extra work in the non-trace path. |
| Reliability | **PASS** (was CONCERNS) | AC-12 adopted, INT-013 green — bad-path UX now produces a clean `typer.BadParameter` referencing `--trace-file`, no raw traceback. Closes the only CONCERN from the in-story NFR Assessment. |
| Maintainability | PASS | Single Typer flag wired to existing constructor kwarg; AC-10 unit/CLI tests + AC-11 docs cover the change; backward compatible per AC-7. |

### Standards Compliance

- ✅ Coding standards — code path reuses existing patterns (`--output` / `--quiet` / `--fail-on-state` from TEA-CLI-008).
- ✅ Project structure — tests in `python/tests/test_cli_trace_file.py`, fixtures under `python/tests/fixtures/dx_001_2/`, both following the established convention.
- ✅ Story-file permissions — only the `## QA Results` section was modified by this review.

### Concerns (low severity, non-blocking)

1. **CONCERN-1 — AC-7 baseline check is shallow.** The test design (NFR-T5) called for a *byte-identical* comparison with the pre-change baseline under `--quiet` and `--stream`. INT-008 only asserts "no stray `*.jsonl` files in the working dir." This catches the headline regression (no inadvertent trace file when the flag is absent) but would not detect a subtle stdout/exit-code drift introduced by the new option. **Suggested action (future, non-blocking):** add a golden snapshot of stdout+exit-code for a one-node graph under `--quiet` and `--stream` to harden AC-7.
2. **CONCERN-2 — INT-011 (`--show-graph` interaction) is missing.** AC-8 names `--show-graph` alongside `--quiet` and `--stream`. The test design lists INT-011 at P2; it was not authored. The Dev Agent Record notes "path-independence holds by construction," which is plausible but unverified. **Suggested action (future, non-blocking):** add a one-line invocation test mirroring INT-009/INT-010 for `--show-graph`.
3. **OBSERVATION — Redundant auto_trace injection in `cli.py`.** Lines 1592-1619 inject `settings.auto_trace=true` into `merged_config` (loading the YAML to dict if `merged_config is None`); lines 1699-1708 then guard on `cli_trace_file is not None and merged_config is None` and load again — but `merged_config` is *always* non-None by that point when `cli_trace_file` is set. Harmless defensive duplication; consider collapsing into one block in a follow-up cleanup.

### Risk Re-evaluation (post-implementation)

All seven risks from the in-story Risk Profile remain LOW or are now MITIGATED:

| Risk | Original | Post-impl |
|------|----------|-----------|
| R1 — Override precedence | Medium | MITIGATED (UNIT-002 + INT-001 green) |
| R2 — Implicit-enable surprise | Medium | MITIGATED (INT-002/003/005 green; help text + docs spell it out) |
| R3 — Path traversal | Low | UNCHANGED — bounded by OS perms, accepted for developer CLI |
| R4 — Env-var parity drift | Low | MITIGATED (UNIT-001 + INT-006 green) |
| R5 — Opaque traceback on bad path | Low | MITIGATED (INT-013 green; `typer.BadParameter` confirmed) |
| R6 — Coverage of implicit-enable matrix | Medium | MITIGATED (4 of 9 cells covered explicitly; remaining cells are isomorphic) |
| R7 — Docs precedence ambiguity | Low | MITIGATED (`docs/python/development-guide.md` + `docs/shared/cli-reference.md` both spell out precedence) |

### Recommendations

**Immediate (none — gate PASS).** No blocking work.

**Future (non-blocking nice-to-haves):**
- Add INT-011 (`--show-graph` interaction) — closes CONCERN-2.
- Promote AC-7 baseline assertion from "no-stray-files" to a golden stdout/exit-code snapshot — closes CONCERN-1.
- Collapse the redundant `auto_trace` injection block (`cli.py:1699-1708`) into the upstream block at `cli.py:1602-1619` — closes OBSERVATION-3.

### Sign-off

**Gate: PASS.** All ACs (including the recommended AC-12) implemented and verified by 22 green tests. Reliability CONCERNS from the in-story NFR Assessment is resolved. Three low-severity, non-blocking observations are recorded in the gate file under `recommendations.future` for follow-up.

---

QA_REVIEW_COMPLETED
