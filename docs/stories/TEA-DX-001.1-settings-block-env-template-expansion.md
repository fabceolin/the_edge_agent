# TEA-DX-001.1: Settings-block env & template expansion

## Status
Done

QA Gate: **PASS** (Quality Score 95/100) — see [`docs/qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml`](../qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml). All 16 requirements FULL coverage; 55/55 tests green; all 9 risks mitigated. Pre-implementation blockers (AC-4 contradiction and `trace_format` schema gap) were resolved in-PR per the QA NFR-AC-1 rewrite — see Dev Agent Record for the implementation contract that shipped.

## Parent Epic
[TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md)

## Priority
High

---

## Story

**As a** workflow author or external runner integrator,
**I want** `settings.trace_file` (and adjacent trace keys) to support `${ENV_VAR}` and `{{ env.VAR }}` expansion,
**so that** I can vary trace output paths per run via environment variables instead of rendering YAML to a temp file.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/yaml_engine.py:980-1004` (`_configure_from_settings` trace block)
- Helper available: `expand_env_vars()` imported at `yaml_engine.py:47` and already applied to `ltm_settings` (line 1015), `secrets_settings` (line 1052), and `firestore_settings` (line 1104)
- Technology: Python, Jinja2, YAML
- Follows pattern: identical `expand_env_vars(...)` invocation already used three times in the same method for other settings sub-blocks

**Problem Statement:**

Today the settings parser reads `trace_file` and `trace_exporter` literally:

```python
# yaml_engine.py:987-988
trace_exporter = settings.get("trace_exporter", "console")
trace_file = settings.get("trace_file")
```

A YAML like:

```yaml
settings:
  auto_trace: true
  trace_exporter: file
  trace_file: ${TEA_TRACE_FILE}   # <-- never expanded
```

passes the literal string `"${TEA_TRACE_FILE}"` to `FileExporter`, which then tries to open a file named `${TEA_TRACE_FILE}`. Forces external runners to render YAML to a temp file just to vary the trace path per run.

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1:** `settings.trace_file`, `settings.trace_exporter`, and `settings.trace_format` (if present) are passed through `expand_env_vars()` before being consumed.
2. **AC-2:** `${VAR}` and `${VAR:-default}` syntax both work in these keys, matching existing `expand_env_vars` behavior used in `ltm_settings`.
3. **AC-3:** Literal values (no `${...}` markers) pass through unchanged — no regression for existing YAML files.
4. **AC-4:** Missing env vars without a default raise the same error the existing `expand_env_vars` raises elsewhere (consistent failure mode).

**Integration Requirements:**

5. **AC-5:** Existing `examples/yaml/` workflows continue to load and run unchanged.
6. **AC-6:** The expansion applies *before* `FileExporter(trace_file)` is constructed, so the exporter receives the resolved path.
7. **AC-7:** Implementation mirrors the established pattern at `yaml_engine.py:1015,1052,1104` — does **not** blanket-expand the entire `settings:` block (which would risk corrupting prompt templates that legitimately contain `${...}`).

**Quality Requirements:**

8. **AC-8:** Unit test covers: literal path (no markers), `${VAR}` resolved, `${VAR:-default}` resolved, missing-required-var error path.
9. **AC-9:** No new dependencies.

## Technical Notes

- **Integration Approach:** Add a single `settings = {**settings, **expand_env_vars({k: settings.get(k) for k in ["trace_file", "trace_exporter", "trace_format"] if k in settings})}` (or equivalent narrow expansion) inside the `if settings.get("auto_trace", False)` block, *before* line 987 reads `trace_exporter` and `trace_file`.
- **Existing Pattern Reference:** `yaml_engine.py:1015` — `ltm_settings = expand_env_vars(ltm_settings)`.
- **Key Constraint:** Do **not** apply `expand_env_vars` to the entire `settings:` dict. Some settings (e.g., `variables` containing prompt templates) may legitimately contain `${...}` literals that should not be expanded. Stay narrow.

## Tasks / Subtasks

- [x] **Task 1: Implement narrow expansion** (AC: 1, 2, 3, 4, 6, 7)
  - [x] In `yaml_engine.py` (auto-trace settings block), just before reading `trace_exporter` and `trace_file`, expand only those keys via `expand_env_vars` (the call site moved during in-flight refactors but the conditional structure matches the original `_configure_from_settings` semantics)
  - [x] Confirm `trace_format` is also covered if it exists in the schema (included in `EXPANDED_TRACE_KEYS` list; engine does not consume it today per CG-1, but the narrow-expansion list is forward-compatible)
  - [x] WARNING + skip when `trace_exporter == "file"` and post-expansion `trace_file` is empty (NFR-AC-2)
  - [x] Inline comment at the call site lists the three expanded keys (NFR-AC-4)
- [x] **Task 2: Tests** (AC: 8)
  - [x] Added `TestSettingsTraceFileEnvExpansion` and `TestSettingsTraceFileIntegration` classes in `python/tests/test_yaml_engine_observability.py`
  - [x] Covers literal (UNIT-005), `${VAR}` (UNIT-001/002/002b), `${VAR:-default}` (UNIT-004), missing-var-no-default (UNIT-006), empty-→-WARN-and-skip (UNIT-007), narrow-expansion regression (UNIT-008), idempotency (UNIT-009), `trace_format` placeholder (UNIT-003)
  - [x] Integration tests: `${VAR}` inside `nodes[*].run` source preserved (INT-002), YAML_REFERENCE.md grep guard (INT-004), inline-comment guard (INT-005), `examples/yaml/*.yaml` smoke load (INT-001 — parametrized)
- [x] **Task 3: Verify no regression** (AC: 5)
  - [x] All 14 `examples/yaml/*.yaml` smoke-load with no spurious empty-`trace_file` WARNING (INT-001)

## Definition of Done

- [x] All ACs met (AC-4 read per QA rewrite: missing var → `""`, no exception; combined with NFR-AC-2 WARNING + skip)
- [x] `pytest python/tests/test_yaml_engine_observability.py` green (55/55 passing, including 26 new TEA-DX-001.1 scenarios)
- [x] No changes to `expand_env_vars` itself — story is consumer-side only
- [x] Documentation note added to `docs/shared/YAML_REFERENCE.md` `settings.trace_*` section (NFR-AC-3)

## Risk and Compatibility

- **Primary Risk:** Accidentally expanding a key that legitimately contains `${...}` (e.g., a prompt template). Mitigated by keeping the expansion narrow to known trace keys.
- **Rollback:** Revert the single PR.
- **Compatibility:** Fully additive — literal values pass through unchanged.

## QA Notes - Risk Profile

Reviewer: Quinn (Test Architect) · Date: 2026-05-01 · Mode: YOLO
Full assessment: [`docs/qa/assessments/TEA-DX-001.1-risk-20260501.md`](../qa/assessments/TEA-DX-001.1-risk-20260501.md)

### Risk Level

- **Overall:** Medium-Low — Risk Score **73 / 100**
- **Suggested Gate (deterministic):** **CONCERNS** — driven by TECH-002 (score 6). One AC clarification + one extra warning path get this to PASS.

### Identified Risks

| ID       | Category | Title                                                                            | Score | Priority |
| -------- | -------- | -------------------------------------------------------------------------------- | ----- | -------- |
| TECH-002 | TECH     | AC-4 expects "raise on missing var" but `expand_env_vars` returns `""`           | 6     | High     |
| TECH-005 | TECH     | Empty post-expansion `trace_file` silently drops `FileExporter` (no warning)     | 4     | Medium   |
| TECH-003 | TECH     | Narrow expansion accidentally widened to keys with legitimate `${...}` literals  | 2     | Low      |
| OPS-001  | OPS      | Resolved trace path lands in non-writable / unexpected location                  | 2     | Low      |
| OPS-002  | OPS      | Doc note in `YAML_REFERENCE.md` missed (already in DoD)                          | 2     | Low      |
| SEC-001  | SEC      | Env-driven trace path enables file-write redirection if env is untrusted         | 2     | Low      |
| DATA-001 | DATA     | Sensitive trace payloads routed to shared/networked storage via env-driven path  | 2     | Low      |
| TECH-001 | TECH     | Future trace keys added without extending the narrow expansion list              | 1     | Minimal  |
| TECH-004 | TECH     | Dict-merge of expanded subset mutates `settings` (no current downstream impact)  | 1     | Minimal  |

### Mitigations (must-fix before Done)

1. **Rewrite AC-4** to match `expand_env_vars` semantics in `python/src/the_edge_agent/memory/base.py:528-537` (missing var → `""`, no exception). Spawning a strict variant is **out of scope**; DoD forbids modifying `expand_env_vars`.
2. **Add WARNING + skip** in `yaml_engine.py:991` when `trace_exporter == "file"` and post-expansion `trace_file` is empty. Without this, operators get silent loss of tracing — the most likely real-world failure mode of this feature.
3. **Add the YAML_REFERENCE.md note** (already in DoD) covering: env expansion semantics, default-fallback syntax, and a one-line operator-responsibility statement for SEC-001 / DATA-001.
4. **Single inline comment** at the narrow-expansion call site listing the keys (`trace_file`, `trace_exporter`, `trace_format`) so future schema additions surface in code review (TECH-001).

### Testing Priorities

**Priority 1 (high-risk coverage — required for gate):**
- Missing var, no default → expands to `""` (not exception). Aligns with the rewritten AC-4.
- `trace_exporter=file` + empty `trace_file` after expansion → warning logged, `_trace_context.exporters` unchanged (TECH-005).
- `${VAR:-/tmp/default.jsonl}` with `VAR` unset → exporter constructed with `/tmp/default.jsonl`.

**Priority 2 (AC coverage):**
- Literal path passes through unchanged (AC-3).
- `${VAR}` resolves when set (AC-1, AC-2).
- Sibling key under `settings` containing `${...}` (e.g., `variables.prompt_template`) is **not** expanded (AC-7 narrowness check).
- `trace_format` expansion if present in schema (AC-1).

**Priority 3 (regression):**
- Smoke-load every `examples/yaml/*.yaml`; assert no behavior delta (AC-5).

### Gate Block

```yaml
risk_summary:
  totals: { critical: 0, high: 1, medium: 1, low: 5, minimal: 2 }
  highest:
    id: TECH-002
    score: 6
    title: 'AC-4 contradicts expand_env_vars actual behavior'
  recommendations:
    must_fix:
      - 'Rewrite AC-4 to match expand_env_vars semantics (empty string on missing, no exception)'
      - 'Log WARNING + skip FileExporter when trace_exporter=file and post-expansion trace_file is empty'
      - 'Add YAML_REFERENCE.md note for settings.trace_* env expansion'
    monitor:
      - 'Empty-trace_file warning rate post-deploy as operator-misconfig signal'
```

## QA Notes - NFR Assessment

Reviewer: Quinn (Test Architect) · Date: 2026-05-01 · Mode: YOLO (core-four default)
Full assessment: [`docs/qa/assessments/TEA-DX-001.1-nfr-20260501.md`](../qa/assessments/TEA-DX-001.1-nfr-20260501.md)

### NFR Coverage

| NFR             | Status   | Quality Score Impact |
| --------------- | -------- | -------------------- |
| Security        | CONCERNS | −10                  |
| Performance     | PASS     | 0                    |
| Reliability     | CONCERNS | −10                  |
| Maintainability | PASS     | 0                    |

**Quality score:** **80 / 100**. **Deterministic NFR gate:** **CONCERNS**.

Compatibility, usability, portability, and functional suitability were not assessed — no triggering signals in the story (narrow, additive config plumbing; no UX surface; no cross-platform contract change).

### Headline Findings

- **Security (CONCERNS).** `expand_env_vars` is a trusted helper used at three sibling sites (`yaml_engine.py:1015,1052,1104`) and AC-7 keeps the expansion narrow, so no new attack surface is introduced. However, a resolved `trace_file` is a file-write sink — the operator-trust caveat (SEC-001 / DATA-001 in the risk profile) needs to land in `YAML_REFERENCE.md` before this becomes PASS. No path validation / allowlist is in scope (correctly so for a narrow story).
- **Performance (PASS).** One `re.sub` over ≤3 string values at `_configure_from_settings` time. Not on the runtime hot path. Nothing to monitor.
- **Reliability (CONCERNS).** Two issues, both already flagged in the risk profile and both addressable in the same PR:
  1. **AC-4 is wrong.** `expand_env_vars` (`python/src/the_edge_agent/memory/base.py:528-537`) returns `""` for an unset var with no default — it does **not** raise. AC-4 must be rewritten before tests are written, or tests will encode the wrong contract.
  2. **Silent skip.** With AC-4 fixed, `trace_exporter=file` + unset env (no default) yields `trace_file=""`, which the existing guard at `yaml_engine.py:991` quietly drops. Operator turns on tracing, gets nothing. A `logger.warning(...)` + skip is required.
- **Maintainability (PASS).** Mirrors three established call sites; AC-8 covers the obvious tests; AC-9 + DoD constrain blast radius (no new deps, no edits to `expand_env_vars`); rollback is a single PR.

### Missing Considerations

The functional ACs are sound modulo AC-4. Beyond what's already in the story / risk profile, the NFR lens adds:

1. **Confidentiality of trace payloads.** Trace output can include LLM I/O / state. Routing through an env-parameterised path is a confidentiality consideration even when the env is trusted (e.g., shared NFS, world-readable `/tmp`, log-aggregator scrape paths). The doc note should call this out in one line.
2. **Schema-drift guard.** TECH-001 in the risk profile (future trace keys added without extending the narrow expansion list) is a real maintainability risk for a list-of-keys pattern. A single inline comment at the call site enumerating `trace_file`, `trace_exporter`, `trace_format` is the lightest mitigation; a small `assert set(EXPANDED_TRACE_KEYS) == {...}` test is optional.
3. **Narrowness regression.** AC-7 forbids blanket-expansion of `settings`. There is no test today that *verifies* an unrelated `${...}`-bearing sibling key (e.g., `settings.variables.prompt_template`) is left untouched. One small assertion catches future regressions.

### Test Recommendations

Adding to the AC-8 list:

- **Empty-after-expansion path** *(reliability must-fix):* `trace_exporter: file`, `trace_file: ${UNSET_VAR}` → exactly one `WARNING` logged identifying the empty trace path, `_trace_context.exporters` length unchanged, no exception.
- **Defaulted path:** `trace_file: ${UNSET_VAR:-/tmp/x.jsonl}` → `FileExporter` constructed with `/tmp/x.jsonl`.
- **Narrowness regression:** `settings.variables.prompt_template: "Hello ${USER}"` (with `USER` set) remains literal `"Hello ${USER}"` post-`_configure_from_settings`. Doubles as AC-7 enforcement.
- **trace_format expansion:** if `trace_format` is in the schema, mirror the `trace_file` test for it (AC-1 explicitly lists it).

### NFR Acceptance Criteria (additive — required for NFR PASS)

- **NFR-AC-1 (Reliability):** AC-4 is rewritten to match `expand_env_vars` semantics: missing env vars without a default expand to `""` (no exception). Strict-required behavior is explicitly out of scope per DoD.
- **NFR-AC-2 (Reliability):** When `trace_exporter == "file"` and post-expansion `trace_file` is empty/falsy, the runner logs a `WARNING` (naming the offending key) and does not append a `FileExporter`. Unit test required.
- **NFR-AC-3 (Security + Maintainability):** `docs/shared/YAML_REFERENCE.md` `settings.trace_*` section documents (a) `${VAR}` / `${VAR:-default}` semantics; (b) empty-string-on-missing → tracing skipped with warning; (c) operator-trust note — resolved path is written to the filesystem as-is, so the env source must be trusted; trace payloads can be sensitive.
- **NFR-AC-4 (Maintainability, nice-to-have, non-gating):** Single inline comment at the narrow-expansion call site enumerates the three expanded keys.

When NFR-AC-1..3 are satisfied, the NFR gate moves to **PASS** (quality score → 100).

### Combined Gate Posture

Risk profile gate: **CONCERNS** (TECH-002 score 6).
NFR gate: **CONCERNS** (Security + Reliability).
**Combined recommended gate: CONCERNS.** All three blockers (AC-4 rewrite, empty-`trace_file` warning, YAML_REFERENCE.md note) are inside-the-PR fixes; expected gate after fixes is **PASS**.

## QA Notes - Test Design

Designer: Quinn (Test Architect) · Date: 2026-05-01 · Mode: YOLO
Full assessment: [`docs/qa/assessments/TEA-DX-001.1-test-design-20260501.md`](../qa/assessments/TEA-DX-001.1-test-design-20260501.md)

### Test Strategy Overview

- **Total scenarios:** 14 (9 unit, 5 integration, 0 e2e)
- **Priority distribution:** P0: 7 · P1: 5 · P2: 2
- **Strategy bias:** unit-heavy. The story is narrow string-substitution config plumbing — no UX, no cross-system flow. Integration tier is reserved for full-YAML regression breadth (examples-yaml smoke, run-block source preservation, doc-note presence, schema-drift comment guard). E2E tests are **not warranted**.
- **Pre-condition:** AC-4 must be rewritten per risk profile TECH-002 + NFR-AC-1 ("missing var → empty string + warning, no exception") before tests are authored. UNIT-006 / UNIT-007 encode the rewritten contract.

### Test Coverage Matrix

| AC / NFR-AC                                              | Scenario IDs                          | Levels             | Priority |
| -------------------------------------------------------- | ------------------------------------- | ------------------ | -------- |
| AC-1 `trace_file` / `trace_exporter` / `trace_format`    | UNIT-001, UNIT-002, UNIT-003          | Unit               | P0/P1/P2 |
| AC-2 `${VAR}` and `${VAR:-default}`                      | UNIT-001, UNIT-004                    | Unit               | P0       |
| AC-3 Literal pass-through                                | UNIT-005                              | Unit               | P0       |
| AC-4 (rewritten) Missing var → `""`                      | UNIT-006, UNIT-007                    | Unit               | P0       |
| AC-5 `examples/yaml/` regression                         | INT-001                               | Integration        | P1       |
| AC-6 Expansion before `FileExporter` constructed         | UNIT-001 (covered)                    | Unit               | P0       |
| AC-7 Narrow expansion (no whole-`settings` blanket)      | UNIT-008, INT-002                     | Unit + Integration | P0/P1    |
| AC-8 Test coverage (literal/`${VAR}`/defaulted/missing)  | UNIT-001, UNIT-004, UNIT-005, UNIT-006, UNIT-007 | Unit  | P0       |
| AC-9 No new dependencies                                 | INT-003                               | Integration        | P2       |
| NFR-AC-2 Empty `trace_file` → WARNING + skip exporter    | UNIT-007                              | Unit               | P0       |
| NFR-AC-3 Doc note in `YAML_REFERENCE.md`                 | INT-004                               | Integration        | P1       |
| NFR-AC-4 Inline-comment listing expanded keys            | INT-005                               | Integration        | P2       |
| Idempotency / dict-merge side effects (TECH-004)         | UNIT-009                              | Unit               | P1       |

### Scenarios with Expected Results

**P0 — must pass for gate**

| ID                    | Test                                                                                                                                        | Expected Result                                                                                  |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| TEA-DX-001.1-UNIT-001 | `auto_trace=True`, `trace_exporter=file`, `trace_file=${TEA_TRACE_FILE}`, env var set to `/tmp/x.jsonl`                                     | `_trace_context.exporters` contains exactly one `FileExporter` constructed with `/tmp/x.jsonl`   |
| TEA-DX-001.1-UNIT-004 | `trace_file=${UNSET_VAR:-/tmp/default.jsonl}`, env unset                                                                                    | `FileExporter` constructed with `/tmp/default.jsonl`                                             |
| TEA-DX-001.1-UNIT-005 | `trace_file=/tmp/literal.jsonl` (no markers)                                                                                                | `FileExporter` constructed with `/tmp/literal.jsonl` byte-identical                              |
| TEA-DX-001.1-UNIT-006 | `trace_file=${UNSET_VAR}`, env unset, no default                                                                                            | No exception; expanded value is `""`                                                             |
| TEA-DX-001.1-UNIT-007 | UNIT-006 inputs + `trace_exporter=file`                                                                                                     | Exactly one `WARNING` logged naming `trace_file`; `_trace_context.exporters` length is 0         |
| TEA-DX-001.1-UNIT-008 | `settings.variables.prompt_template="Hello ${USER}"` with `USER` set to `alice`                                                             | Post-call value equals literal `"Hello ${USER}"` (NOT `"Hello alice"`) — narrow expansion proves AC-7 |

**P1 — strongly recommended**

| ID                    | Test                                                                                                                          | Expected Result                                                                          |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-002 | `trace_exporter=${TEA_TRACE_EXPORTER}` resolved to `console` / `file`                                                         | Correct exporter type appended for each resolved value                                   |
| TEA-DX-001.1-UNIT-009 | Call `_configure_from_settings` twice with the same valid file-trace settings                                                 | `_trace_context.exporters` length stays at 1 (existing idempotency guard preserved)      |
| TEA-DX-001.1-INT-001  | Parametrized: `YAMLEngine(p)` for every `p in examples/yaml/*.yaml`                                                           | No exception; no spurious empty-`trace_file` WARNING                                     |
| TEA-DX-001.1-INT-002  | YAML with `${VAR}` inside a `nodes[*].run` block                                                                              | Run-block source preserved verbatim post-load — narrow expansion did not pre-expand it  |
| TEA-DX-001.1-INT-004  | Grep `docs/shared/YAML_REFERENCE.md` `settings.trace_*` section                                                               | Section mentions `${VAR}` syntax, default fallback, empty-→-skip-with-warning, operator-trust note |

**P2 — nice-to-have**

| ID                    | Test                                                                                                                          | Expected Result                                                                          |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-003 | `trace_format` expansion — only if engine consumes `trace_format`                                                             | If unwired today, mark xfail with TODO; otherwise mirror UNIT-001 for `trace_format`     |
| TEA-DX-001.1-INT-003  | Diff `python/setup.py` and `python/pyproject.toml` between branch and `main`                                                  | No new entries under `install_requires` / `dependencies`                                 |
| TEA-DX-001.1-INT-005  | Grep narrow-expansion call site in `yaml_engine.py`                                                                           | Comment lists `trace_file`, `trace_exporter`, `trace_format` near the expansion call     |

### Test Data & Environment Requirements

- **Test file location:** `python/tests/test_yaml_engine_observability.py` (closest existing module to trace-config surface).
- **Fixtures required:** none new — synthetic in-test settings dicts only. Existing `examples/yaml/*.yaml` are reused as-is by INT-001.
- **Tooling:** `pytest`, `monkeypatch.setenv` / `monkeypatch.delenv(..., raising=False)`, `caplog.at_level("WARNING")`. `tmp_path` only if `FileExporter.__init__` opens the file eagerly.
- **No external deps:** no network, no DB, no LTM backend, no LLM credentials, no Docker, no CI-only services.
- **Helper:** a small `_make_minimal_engine()` fixture that constructs a `YAMLEngine` with a one-node graph so `_configure_from_settings` can be exercised in isolation.

### Risk Coverage

| Risk     | Severity | Mitigated by                                                |
| -------- | -------- | ----------------------------------------------------------- |
| TECH-002 | High     | UNIT-006, UNIT-007 (assumes AC-4 rewritten)                 |
| TECH-005 | Medium   | UNIT-007                                                    |
| TECH-003 | Low      | UNIT-008, INT-002                                           |
| TECH-001 | Minimal  | INT-005 (inline comment guard)                              |
| TECH-004 | Minimal  | UNIT-009 (idempotency)                                      |
| OPS-002 / SEC-001 / DATA-001 | Low | INT-004 (doc note presence)                       |

### Coverage Gaps

- **AC-1 `trace_format`:** engine does not currently consume `trace_format` (`yaml_engine.py:987-988` reads only `trace_exporter` and `trace_file`). Resolve by either tightening AC-1 to drop `trace_format`, or extending engine + UNIT-003 to consume it. **Recommendation: tighten AC-1** — drop `trace_format` to avoid carrying a dormant test.

### Recommended Execution Order

1. P0 Unit (UNIT-001, 004, 005, 006, 007, 008) — fail-fast on high-risk semantics + narrowness contract
2. P1 Unit (UNIT-002, 009) — secondary expansion + idempotency
3. P1 Integration (INT-001, 002, 004) — examples regression, run-block preservation, doc note
4. P2 (UNIT-003, INT-003, INT-005) — schema reality, dep review, comment guard

### Gate YAML Block

```yaml
test_design:
  scenarios_total: 14
  by_level: { unit: 9, integration: 5, e2e: 0 }
  by_priority: { p0: 7, p1: 5, p2: 2 }
  coverage_gaps:
    - 'AC-1 trace_format: engine does not currently consume trace_format; tighten AC-1 or wire trace_format and add UNIT-003'
  preconditions:
    - 'AC-4 rewritten per risk profile TECH-002 / NFR-AC-1 — missing var → empty string + warning, no exception'
```

## QA Notes - Requirements Trace

Reviewer: Quinn (Test Architect) · Date: 2026-05-01 · Mode: YOLO
Full assessment: [`docs/qa/assessments/TEA-DX-001.1-trace-20260501.md`](../qa/assessments/TEA-DX-001.1-trace-20260501.md)

### Requirements Coverage

| Bucket             | Total | Full | Partial | None       |
| ------------------ | ----- | ---- | ------- | ---------- |
| Functional ACs     | 7     | 0    | 0       | 7          |
| Integration ACs    | 3     | 0    | 0       | 3          |
| Quality ACs        | 2     | 0    | 0       | 2          |
| NFR-ACs (additive) | 4     | 0    | 0       | 4          |
| **TOTAL**          | **16**| **0**| **0**   | **16 (100%)** |

**Observed reality.** Story is in `Draft`. `python/src/the_edge_agent/yaml_engine.py:987-988` still reads `trace_file` / `trace_exporter` literally — no `expand_env_vars` call yet. A search of `python/tests/` for `test_settings_trace_file_env_expansion`, `TEA-DX-001.1`, and `TEA_TRACE_FILE` returns zero matches. All 16 requirements are therefore currently **uncovered**. The mappings below are the **planned** coverage from the test-design assessment; coverage flips to FULL once the 14 planned scenarios land alongside implementation.

**AC-4 caveat.** AC-4 as written ("missing env vars without a default raise the same error") contradicts the actual behavior of `expand_env_vars` at `python/src/the_edge_agent/memory/base.py:528-537` (`os.environ.get(var_name, default)` with `default == ""` → silent empty string, no exception). The trace treats AC-4 as the **rewritten** version per risk profile TECH-002 / NFR-AC-1 ("missing var → empty string + WARNING + skip exporter"). Without the rewrite, AC-4 is unimplementable as specified (DoD forbids modifying `expand_env_vars`).

### Traceability Matrix (compact)

| AC / NFR-AC                              | Planned Tests                                          | Level                | Priority | Coverage (today) |
| ---------------------------------------- | ------------------------------------------------------ | -------------------- | -------- | ---------------- |
| AC-1 (`trace_file/exporter/format` exp.) | UNIT-001, UNIT-002, UNIT-003                           | Unit                 | P0/P1/P2 | none             |
| AC-2 (`${VAR}` + default)                | UNIT-001, UNIT-004                                     | Unit                 | P0       | none             |
| AC-3 (literal pass-through)              | UNIT-005                                               | Unit                 | P0       | none             |
| AC-4 (rewritten — missing var → `""`)    | UNIT-006, UNIT-007                                     | Unit                 | P0       | none             |
| AC-5 (`examples/yaml/` regression)       | INT-001                                                | Integration          | P1       | none             |
| AC-6 (expansion before `FileExporter`)   | UNIT-001 (transitive)                                  | Unit                 | P0       | none             |
| AC-7 (narrow expansion)                  | UNIT-008, INT-002                                      | Unit + Integration   | P0/P1    | none             |
| AC-8 (test matrix)                       | UNIT-001, UNIT-004, UNIT-005, UNIT-006, UNIT-007       | Unit                 | P0       | none             |
| AC-9 (no new deps)                       | INT-003                                                | Integration / CI     | P2       | none             |
| NFR-AC-1 (Reliability — AC-4 contract)   | UNIT-006                                               | Unit                 | P0       | none             |
| NFR-AC-2 (Reliability — empty + WARN)    | UNIT-007                                               | Unit                 | P0       | none             |
| NFR-AC-3 (Doc note)                      | INT-004                                                | Integration / doc    | P1       | none             |
| NFR-AC-4 (Inline comment)                | INT-005                                                | Integration / source | P2       | none             |
| TECH-004 (idempotency)                   | UNIT-009                                               | Unit                 | P1       | none             |

Total scenarios planned: **14** (9 unit, 5 integration, 0 e2e). Currently authored: **0**.

### Gaps Identified

1. **CG-1 — `trace_format` not wired into the engine.** AC-1 names `trace_format`, but `yaml_engine.py:987-988` reads only `trace_exporter` and `trace_file`. Adding expansion for a key the engine never reads is dead code. **Action:** tighten AC-1 to drop `trace_format` (test-design recommendation) **or** wire it into the relevant exporter and promote UNIT-003 from xfail to a hard assertion.
2. **CG-2 — AC-4 contradicts `expand_env_vars` semantics.** AC-4 says missing required vars raise; the helper returns `""`. **Must-fix before tests are authored.** Rewrite AC-4 per risk profile TECH-002 / NFR-AC-1.
3. **CG-3 — No narrowness regression test exists.** AC-7 forbids blanket-expansion of `settings`, but no current test verifies a sibling `${...}`-bearing key (e.g., `settings.variables.prompt_template`) is left untouched. Land UNIT-008 + INT-002 in the same PR as the implementation.
4. **CG-4 — Operator-trust note absent from `YAML_REFERENCE.md`.** SEC-001 + DATA-001 + OPS-002 ship undocumented without it. Land doc note + INT-004 grep guard.

### Recommendations

1. **Unblock the story (must-do, before code):**
   - Rewrite AC-4 to: *"Missing env vars without a default expand to the empty string, matching `expand_env_vars` semantics. When `trace_exporter == 'file'` and post-expansion `trace_file` is empty, the runner MUST log a `WARNING` and skip appending a `FileExporter`."*
   - Decide CG-1: drop `trace_format` from AC-1 **or** wire it through. Recommended: **drop** (avoids carrying a dormant test).
2. **Implementation PR scope (must include):**
   - Narrow `expand_env_vars(...)` call covering only the agreed key list, mirroring the established pattern at `yaml_engine.py:1015,1052,1104`. Do **not** blanket-expand `settings`.
   - WARNING + skip path for empty `trace_file` (NFR-AC-2).
   - Inline comment at the call site listing the expanded keys (NFR-AC-4).
   - `docs/shared/YAML_REFERENCE.md` `settings.trace_*` note covering env semantics, default fallback, empty-→-skip-with-warning, and the operator-trust caveat (NFR-AC-3).
3. **Test PR scope (must include):**
   - All P0 unit tests: UNIT-001, UNIT-004, UNIT-005, UNIT-006, UNIT-007, UNIT-008 — added to `python/tests/test_yaml_engine_observability.py`.
   - P1: UNIT-002, UNIT-009, INT-001, INT-002, INT-004.
   - P2: UNIT-003 (xfail if `trace_format` dropped from AC-1), INT-003, INT-005.
   - Tooling: `monkeypatch.setenv` / `monkeypatch.delenv(..., raising=False)`, `caplog.at_level("WARNING")`. No new deps, no fixtures, no network/DB/LLM.
4. **Gate posture forecast:** with the AC-4 rewrite + WARNING-on-empty + doc note + the planned tests, the combined gate moves from **CONCERNS** to **PASS** (per risk profile and NFR assessment).
5. **Out-of-scope (defer):** strict-required env-var variant (DoD forbids modifying `expand_env_vars`); path validation / allowlist for trace files (correctly out of scope for a narrow story); broader settings-block env expansion across non-trace keys (would violate AC-7).

### Trace Gate Block

```yaml
trace:
  totals: { requirements: 16, full: 0, partial: 0, none: 16 }
  planning_ref: 'docs/qa/assessments/TEA-DX-001.1-test-design-20260501.md'
  blockers:
    - 'AC-4 must be rewritten to match expand_env_vars semantics (no exception; "" on missing) — without this, UNIT-006/UNIT-007 are unimplementable as specified.'
    - 'Decide CG-1: tighten AC-1 to drop trace_format OR wire trace_format into the exporter in this story.'
  notes: 'All 16 requirements currently uncovered (story Draft). Plan converts to FULL coverage when the 14 test-design scenarios land alongside implementation. See docs/qa/assessments/TEA-DX-001.1-trace-20260501.md.'
```

## SM Validation

Reviewer: Bob (Scrum Master) · Date: 2026-05-01 · Mode: YOLO

### Definition of Ready Checklist

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | Story has clear title and description | ✅ PASS | Title and User Story (`As a / I want / So that`) are well-formed; "Story Context" + "Problem Statement" anchor the change in code with file/line references. |
| 2 | Acceptance criteria are defined and testable | ❌ FAIL | 9 ACs are present but **AC-4 is unimplementable as written**. AC-4 says missing env vars without a default "raise the same error the existing `expand_env_vars` raises elsewhere", but `expand_env_vars` (`python/src/the_edge_agent/memory/base.py:528-537`) returns `""` and never raises. DoD forbids modifying `expand_env_vars`. AC-4 must be rewritten per QA NFR-AC-1 / risk TECH-002 to: *"Missing env vars without a default expand to `""`. When `trace_exporter == 'file'` and post-expansion `trace_file` is empty, the runner MUST log a `WARNING` and skip appending a `FileExporter`."* |
| 3 | Dependencies are identified | ✅ PASS | Integration target (`yaml_engine.py:980-1004`), helper (`expand_env_vars` at `yaml_engine.py:47`), three sibling reference call sites (`:1015,:1052,:1104`), and the helper definition (`memory/base.py:528-537`) are all cited with line numbers. No new external deps (AC-9). |
| 4 | Technical approach is documented | ✅ PASS | "Technical Notes" gives a concrete code-level approach with the exact insertion point, an explicit constraint against blanket-expansion (AC-7), and a reference implementation pattern. |
| 5 | Story is properly sized | ✅ PASS | Single-method change in `_configure_from_settings`, 6–9 unit tests, 5 integration tests, and a doc note. Three small tasks. Fits well within a single PR; rollback is "revert the single PR". |
| 6 | QA notes sections are present | ✅ PASS | All four present and substantive: Risk Profile (9 risks scored, gate **CONCERNS**), NFR (core-four, gate **CONCERNS**), Test Design (14 scenarios across 9/5/0 unit/int/e2e), Requirements Trace (16 requirements, all currently uncovered with planned mapping). All link to assessment files in `docs/qa/assessments/`. |
| 7 | No blocking issues or unknowns | ❌ FAIL | Two unresolved blockers carried in the story body, both explicitly flagged across the QA assessments: **(a) AC-4 contradiction** (TECH-002, NFR-AC-1, Trace CG-2 — must rewrite before tests are authored). **(b) `trace_format` schema gap** (Test Design CG-1, Trace CG-1) — AC-1 lists `trace_format` but `yaml_engine.py:987-988` does not consume it; expansion would target dead code. Story must either drop `trace_format` from AC-1 or extend the engine to read it. Both decisions are owner-required and pre-code. |

### Verdict

**NEEDS REVISION** — 5/7 criteria pass; criteria 2 and 7 fail on the same two blockers.

### Required Revisions Before "Ready for Development"

1. **Rewrite AC-4** to match observed `expand_env_vars` semantics (empty string on missing, no exception) and fold the "WARNING + skip exporter when `trace_exporter=file` and post-expansion `trace_file` is empty" requirement into AC-4 (or add as new AC-4b). Drives UNIT-006 / UNIT-007.
2. **Resolve CG-1 (`trace_format`)**: either remove `trace_format` from AC-1 and from Tasks/Subtasks + Test Design (recommended — avoids dormant test UNIT-003), **or** extend Tasks to wire `trace_format` into the relevant exporter and promote UNIT-003 to a hard assertion.
3. (Carry-over from QA, already in DoD/Tasks) Confirm doc note (`docs/shared/YAML_REFERENCE.md`) and inline-comment requirements survive the AC rewrite.

After items 1 and 2 land, the gate flips to **Ready for Development** with the QA-projected combined gate of **PASS** once the planned 14 test scenarios + WARNING path + doc note are implemented.

SM_CHECKLIST_COMPLETED

## Dev Agent Record

### Agent Model Used
- Claude Opus 4.7 (1M context) — model id `claude-opus-4-7`

### Implementation Notes
- **AC-4 interpretation:** implemented per QA NFR-AC-1 rewrite — missing env vars without a default expand to `""` (no exception), matching `expand_env_vars` semantics at `python/src/the_edge_agent/memory/base.py:528-537`. Combined with NFR-AC-2 WARNING + skip when `trace_exporter == "file"` and post-expansion `trace_file` is empty.
- **CG-1 (`trace_format`):** included in the `EXPANDED_TRACE_KEYS` tuple even though the engine does not currently consume it. Cost is one extra `re.sub` over a string at engine init; benefit is forward compatibility (when `trace_format` is wired into an exporter, env expansion already works). UNIT-003 covers the load-doesn't-break case.
- **Narrow-expansion call site:** the auto-trace settings block was relocated by in-flight TEA-OBS-003 / TEA-DX-001.6 work that was already in the working tree at session start. The expansion sits inside the same `if settings.get("auto_trace", ...)` guard described in the story's Technical Notes — exactly mirroring the established `ltm_settings`/`secrets_settings`/`firestore_settings` patterns.
- **Pre-existing test failures:** the working tree at session start carried in-flight changes for TEA-OBS-003 (LLM payload capture), TEA-DX-001.6 (validation pre-flight), and others. These produce ~180 pre-existing test failures unrelated to TEA-DX-001.1; verified by stashing my edits and re-running, which produced the same failure count. Net new failures introduced by this story: 0.

### Validations Run
- `python3 -m pytest tests/test_yaml_engine_observability.py` → **55/55 passing** (29 baseline + 26 new TEA-DX-001.1 scenarios)
- Parametrized `INT-001` smoke-load over all 14 `examples/yaml/*.yaml` files → no spurious empty-`trace_file` WARNING
- Pre/post stash diff confirms no new failures outside the observability module

### File List

**Modified**
- `python/src/the_edge_agent/yaml_engine.py` — narrow `expand_env_vars` over `EXPANDED_TRACE_KEYS = ("trace_file", "trace_exporter", "trace_format")` inside the `auto_trace` settings block; WARNING + skip when `trace_exporter="file"` and post-expansion `trace_file` is empty; inline TEA-DX-001.1 comment listing the three expanded keys
- `python/tests/test_yaml_engine_observability.py` — added `TestSettingsTraceFileEnvExpansion` (10 unit tests) and `TestSettingsTraceFileIntegration` (3 integration tests) classes, plus parametrized `test_int_001_examples_yaml_no_spurious_empty_trace_warning` covering all `examples/yaml/*.yaml`
- `docs/shared/YAML_REFERENCE.md` — new `settings.trace_*` subsection (NFR-AC-3) documenting `${VAR}` / `${VAR:-default}` semantics, empty-→-skip-with-warning behavior, operator-trust caveat (SEC-001 / DATA-001), and narrow-expansion guarantee (AC-7)

**New**
- *(none — all changes layered onto existing files)*

**Deleted**
- *(none)*

### Change Log

| Date       | Author             | Note                                                                                                                              |
| ---------- | ------------------ | --------------------------------------------------------------------------------------------------------------------------------- |
| 2026-05-02 | James (dev agent)  | Implemented narrow `expand_env_vars` for `settings.trace_*`; added WARNING + skip on empty `trace_file`; doc note + 26 new tests. |

### Completion Notes
- All 9 functional/integration/quality ACs satisfied (AC-4 per QA rewrite). All 4 NFR-ACs satisfied.
- Combined gate posture (per QA): risk **CONCERNS → PASS**, NFR **CONCERNS → PASS**.
- No new dependencies (AC-9). `expand_env_vars` itself untouched (DoD).

## QA Results

Reviewer: Quinn (Test Architect) · Date: 2026-05-02 · Mode: YOLO (review-story)
Gate file: [`docs/qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml`](../qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml)

### Gate Decision: **PASS** · Quality Score: **95 / 100**

The implementation lands cleanly against the QA-rewritten AC-4 + NFR-AC-1..4 contract. All three CONCERNS-driving items from the upstream Risk Profile and NFR Assessment (TECH-002, TECH-005, OPS-002 / SEC-001 / DATA-001 doc note) are addressed in-PR, and the planned 14-scenario test plan has landed with broader coverage than was specified.

### Code Quality Review

**Implementation site:** `python/src/the_edge_agent/yaml_engine.py:1255-1286` (`load_from_dict` auto-trace block).

- ✅ **Narrow expansion (AC-7):** `EXPANDED_TRACE_KEYS = ("trace_file", "trace_exporter", "trace_format")` at line 1265 with a sub-dict-only `expand_env_vars` call at 1266-1268 — does not blanket-expand `settings`. Mirrors the three sibling call sites (`ltm` :1320, `secrets` :1357, `firestore` further below).
- ✅ **WARNING + skip on empty `trace_file` (NFR-AC-2):** `yaml_engine.py:1280-1284` emits a single `WARNING` naming the offending key and does not append a `FileExporter`. Verified by UNIT-007 with `assertLogs("the_edge_agent.yaml_engine", level="WARNING")`.
- ✅ **Inline schema-drift comment (NFR-AC-4):** Lines 1260-1264 carry the `TEA-DX-001.1` marker and enumerate the three expanded keys. INT-005 grep-guards the comment so future schema additions surface in code review.
- ✅ **Expansion before exporter construction (AC-6):** Line 1266 expansion strictly precedes line 1275 `FileExporter(trace_file)`.
- ✅ **No edits to `expand_env_vars`:** verified — `python/src/the_edge_agent/memory/base.py:528-537` is untouched (DoD).
- ✅ **No new deps (AC-9):** verified — no changes to `python/setup.py` or `python/pyproject.toml`.
- ℹ️ **Note:** A second narrow `EXPANDED_TRACE_KEYS = ("trace_file",)` exists at `yaml_engine.py:835` inside `_resolve_payload_file_path` (TEA-OBS-003 path-resolution helper). Independent of TEA-DX-001.1 but consistent with the same narrow-expansion philosophy. No conflict.

### Requirements Traceability (Given–When–Then summary)

| AC / NFR-AC | Test Coverage | Result |
|-------------|---------------|--------|
| AC-1 (`trace_file` / `trace_exporter` / `trace_format`) | UNIT-001, UNIT-002, UNIT-002b, UNIT-003 | ✅ FULL |
| AC-2 (`${VAR}` + `${VAR:-default}`) | UNIT-001, UNIT-004 | ✅ FULL |
| AC-3 (literal pass-through) | UNIT-005 | ✅ FULL |
| AC-4 (per QA rewrite — missing var → `""`) | UNIT-006 | ✅ FULL |
| AC-5 (`examples/yaml/` regression) | INT-001 (parametrized × 14 files) | ✅ FULL |
| AC-6 (expansion before `FileExporter`) | UNIT-001 (transitive) | ✅ FULL |
| AC-7 (narrow expansion) | UNIT-008, INT-002 | ✅ FULL |
| AC-8 (test matrix) | UNIT-001/004/005/006/007 | ✅ FULL |
| AC-9 (no new deps) | manual diff verified | ✅ FULL |
| NFR-AC-1 (Reliability — AC-4 contract) | UNIT-006 | ✅ FULL |
| NFR-AC-2 (Reliability — empty + WARN + skip) | UNIT-007 | ✅ FULL |
| NFR-AC-3 (`YAML_REFERENCE.md` doc note) | INT-004 | ✅ FULL |
| NFR-AC-4 (inline-comment listing keys) | INT-005 | ✅ FULL |
| TECH-004 (idempotency) | UNIT-009 | ✅ FULL |

**Coverage:** 16/16 requirements FULL (was 0/16 at trace time). Test count: **26 new TEA-DX-001.1 scenarios** (story claimed 14; bonus: UNIT-002b file-resolution variant, plus INT-001 expanded to all 14 `examples/yaml/*.yaml`).

### Test Architecture Assessment

- **Levels:** unit-heavy (10 unit + 4 integration), zero e2e — appropriate for narrow string-substitution config plumbing.
- **Determinism:** all tests use `monkeypatch`-style setenv/delenv guarded by `try/finally`; no shared mutable state. `caplog` scoped to `the_edge_agent.yaml_engine` logger so unrelated WARNINGs don't pollute assertions.
- **No external deps:** no network, no DB, no LLM credentials. Verified by direct read of `test_yaml_engine_observability.py`.
- **Black-box vs white-box balance:** AC-1..AC-7 covered black-box via behavior assertions. NFR-AC-3/4 use grep-style file inspection (INT-004, INT-005) — appropriate for "doc note exists" / "comment exists" guards.

**Run result (this review):** `pytest tests/test_yaml_engine_observability.py -v` → **55 passed, 0 failed, 0 skipped, 0.45s** (29 baseline + 26 new TEA-DX-001.1 scenarios). All 14 parametrized `INT-001` runs over `examples/yaml/*.yaml` green with no spurious empty-`trace_file` WARNINGs.

### NFR Validation (post-implementation)

| NFR | Status (pre) | Status (post) | Evidence |
|-----|--------------|---------------|----------|
| Security | CONCERNS | **PASS** | Narrow expansion (AC-7); operator-trust caveat documented at `YAML_REFERENCE.md:775-780`. |
| Performance | PASS | **PASS** | Single `re.sub` over ≤ 3 strings at engine-init time; off the runtime hot path. |
| Reliability | CONCERNS | **PASS** | AC-4 per rewritten contract (UNIT-006); WARNING + skip on empty (UNIT-007); idempotency preserved (UNIT-009). |
| Maintainability | PASS | **PASS** | Mirrors 3 sibling sites; inline `TEA-DX-001.1` comment + INT-005 grep-guard prevents schema drift. |

Combined NFR gate: **PASS** (quality score → 95).

### Standards Compliance

- ✅ Implementation matches the established `expand_env_vars(...)`-on-a-narrow-subdict pattern used at `yaml_engine.py:1320` (ltm) and :1357 (secrets).
- ✅ Logger name (`the_edge_agent.yaml_engine`) matches the module-level `logger = logging.getLogger(__name__)` convention used elsewhere in the file.
- ✅ Doc note added to `docs/shared/YAML_REFERENCE.md:745-785` follows the `## Settings` subsection style used by adjacent blocks.
- ✅ `expand_env_vars` itself untouched (DoD); rollback is a single PR (per Risk and Compatibility section).

### Risk Summary (post-implementation)

All 9 risks from the QA Risk Profile (`docs/qa/assessments/TEA-DX-001.1-risk-20260501.md`) are mitigated:

| ID | Score | Status |
|----|-------|--------|
| TECH-002 (AC-4 contradiction) | 6 | ✅ MITIGATED — implemented per NFR-AC-1 rewrite |
| TECH-005 (silent empty `trace_file`) | 4 | ✅ MITIGATED — UNIT-007 enforces WARNING + skip |
| TECH-003 (narrowness regression) | 2 | ✅ MITIGATED — UNIT-008 + INT-002 |
| TECH-001 (schema drift) | 1 | ✅ MITIGATED — inline comment + INT-005 grep-guard |
| TECH-004 (dict-merge mutation) | 1 | ✅ MITIGATED — UNIT-009 idempotency assertion |
| OPS-001 / OPS-002 / SEC-001 / DATA-001 | 2 each | ✅ MITIGATED — operator-trust note in `YAML_REFERENCE.md:775-780` |

**Monitor (post-deploy):** rate of `settings.trace_exporter='file' but settings.trace_file expanded to empty string ...` warnings as a real-world operator-misconfig signal.

### Issues Found

**Low — DOC-001 (cosmetic, non-blocking):** The story body still carries the original (pre-rewrite) AC-4 text and a `**Blocked by:**` header (lines 6-10 and line 62 of this story file), even though the implementation, tests, and Dev Agent Record all follow the QA NFR-AC-1 rewrite. The on-disk story does not match what shipped — recommend a follow-up cosmetic edit to rewrite AC-4 in place and drop the `Blocked by:` header. **Out of scope for QA Results-only edit; flagged in the gate `recommendations.future`.**

### Files Reviewed

- `python/src/the_edge_agent/yaml_engine.py` (impl @ :1255-1286, sibling site @ :835)
- `python/src/the_edge_agent/memory/base.py` (helper @ :528-537 — verified untouched)
- `python/tests/test_yaml_engine_observability.py` (new tests @ :603-896)
- `docs/shared/YAML_REFERENCE.md` (doc note @ :745-785)

### Recommended Status

**Ready for Done.** Combined gate **PASS**, all 16 requirements FULL coverage, all 9 risks mitigated, 55/55 tests green. The single low-severity finding (DOC-001) is cosmetic story-text drift and does not block release.

QA_REVIEW_COMPLETED

---

### Review Date: 2026-05-02 (revalidation)

### Reviewed By: Quinn (Test Architect) · Mode: YOLO (review-story)

Gate file: [`docs/qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml`](../qa/gates/TEA-DX-001.1-settings-block-env-template-expansion.yml)

### Gate Decision: **PASS** (re-affirmed) · Quality Score: **95 / 100**

Independent re-execution of the QA review confirms the prior 2026-05-02 verdict. No regressions, no new findings.

#### Re-validation Evidence (run during this review)

- ✅ **Tests:** `pytest python/tests/test_yaml_engine_observability.py -q` → **55 passed, 0 failed, 0 skipped, 0.42s** (29 baseline + 26 TEA-DX-001.1 scenarios). INT-001 ran parametrized across all 14 `examples/yaml/*.yaml` files with no spurious empty-`trace_file` warnings.
- ✅ **Implementation site:** `yaml_engine.py:1255-1286` — `EXPANDED_TRACE_KEYS = ("trace_file", "trace_exporter", "trace_format")` (line 1265) with sub-dict-only `expand_env_vars(...)` call (lines 1266-1268). Inline `TEA-DX-001.1` marker comment present at lines 1260-1264 enumerating the three keys (NFR-AC-4 / INT-005).
- ✅ **WARNING + skip path:** `yaml_engine.py:1280-1284` emits a single `WARNING` and does not append `FileExporter` when `trace_exporter='file'` and post-expansion `trace_file` is empty (NFR-AC-2 / UNIT-007).
- ✅ **Helper untouched:** `git diff python/src/the_edge_agent/memory/base.py` → empty. `expand_env_vars` at lines 528-537 unchanged (DoD compliance).
- ✅ **No new deps:** `git diff main -- python/setup.py python/pyproject.toml` → empty (AC-9).
- ✅ **Sibling pattern parity:** Mirrors three established sibling sites (`yaml_engine.py:1320` ltm, `:1357` secrets, firestore further below). A second narrow `EXPANDED_TRACE_KEYS = ("trace_file",)` at `yaml_engine.py:835` (TEA-OBS-003 payload-path resolution) reuses the same philosophy independently — no conflict.
- ✅ **Doc note:** `docs/shared/YAML_REFERENCE.md:745-786` documents `${VAR}` / `${VAR:-default}` semantics, empty-→-skip-with-warning behavior, operator-trust caveat (SEC-001 / DATA-001), and the narrow-expansion guarantee — all four NFR-AC-3 requirements present.

#### Requirements Traceability (re-checked)

16 / 16 requirements **FULL** coverage (9 functional + 3 integration + 2 quality + 4 NFR-ACs + TECH-004 idempotency). No coverage gaps.

#### NFR Validation (re-checked)

| NFR             | Status | Notes                                                                                                |
|-----------------|--------|------------------------------------------------------------------------------------------------------|
| Security        | PASS   | Narrow expansion preserved (AC-7); operator-trust caveat documented at YAML_REFERENCE.md:775-780.    |
| Performance     | PASS   | Single `re.sub` over ≤ 3 strings at engine init; off the runtime hot path.                           |
| Reliability     | PASS   | UNIT-006 + UNIT-007 enforce QA-rewritten AC-4 + WARNING-on-empty contract; UNIT-009 idempotency.     |
| Maintainability | PASS   | Inline TEA-DX-001.1 comment + INT-005 grep-guard prevent silent schema drift.                        |

#### Risk Coverage (re-checked)

All 9 identified risks remain **MITIGATED** (TECH-001/002/003/004/005, OPS-001/002, SEC-001, DATA-001). No new risks surfaced during revalidation.

#### Compliance Check

- Coding Standards: ✓ — mirrors established narrow `expand_env_vars` pattern at three sibling sites.
- Project Structure: ✓ — single-method change inside `_configure_from_settings`; tests in the closest existing observability module; doc note in the canonical settings reference.
- Testing Strategy: ✓ — unit-heavy (10 unit + 4 integration, 0 e2e) per the test-design assessment; deterministic via `monkeypatch` setenv/delenv with try/finally guards.
- All ACs Met: ✓ (AC-4 read per QA NFR-AC-1 rewrite, as documented in Dev Agent Record).

#### Standing Issues

- **DOC-001 (low, non-blocking, carried from prior review):** Story body lines 6-10 (`Blocked by:` header) and AC-4 text at lines 62-63 still reflect the pre-rewrite contract; implementation, tests, and Dev Agent Record all follow the QA NFR-AC-1 rewrite. Cosmetic only — no runtime impact. Tracked in gate `recommendations.future`. Out of scope for QA-Results-only edit per review-story task.

#### Files Modified During Review

None — no source code, tests, or docs were changed during this revalidation. Only QA Results section appended in this story file and the gate file `updated` / `history` fields refreshed.

#### Recommended Status

**Ready for Done (re-affirmed).** No code changes required. Gate file timestamp + history entry refreshed.

QA_REVIEW_COMPLETED
