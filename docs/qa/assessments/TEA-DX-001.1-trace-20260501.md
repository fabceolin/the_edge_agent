# Requirements Traceability Matrix

## Story: TEA-DX-001.1 — Settings-block env & template expansion

Reviewer: Quinn (Test Architect)
Date: 2026-05-01
Mode: YOLO
Story Status: **Draft** (pre-implementation)
Planning refs:
- Risk profile: [`TEA-DX-001.1-risk-20260501.md`](TEA-DX-001.1-risk-20260501.md)
- NFR assessment: [`TEA-DX-001.1-nfr-20260501.md`](TEA-DX-001.1-nfr-20260501.md)
- Test design: [`TEA-DX-001.1-test-design-20260501.md`](TEA-DX-001.1-test-design-20260501.md)

---

## Coverage Summary

| Bucket            | Total | Fully Covered | Partial | None |
| ----------------- | ----- | ------------- | ------- | ---- |
| Functional ACs    | 7     | 0             | 0       | 7    |
| Integration ACs   | 3     | 0             | 0       | 3    |
| Quality ACs       | 2     | 0             | 0       | 2    |
| NFR-ACs (additive)| 4     | 0             | 0       | 4    |
| **TOTAL**         | **16**| **0 (0%)**    | **0 (0%)** | **16 (100%)** |

> ⚠️ **Observed reality:** All ACs are currently **uncovered** because the story is in `Draft` and no implementation exists. `python/src/the_edge_agent/yaml_engine.py:987-988` still reads `trace_file` / `trace_exporter` literally (no `expand_env_vars` call). A search of `python/tests/` for `test_settings_trace_file_env_expansion`, `TEA-DX-001.1`, and `TEA_TRACE_FILE` returns zero matches. The mappings below are the **planned coverage** from the test-design assessment — once implementation lands, the table converts those `none` rows to `full` / `partial` based on what actually ships.

> ⚠️ **AC-4 contract conflict (TECH-002):** AC-4 as written says missing required env vars *raise an exception*. The actual behavior of `expand_env_vars` (`python/src/the_edge_agent/memory/base.py:528-537`) is `os.environ.get(var_name, default)` where `default == ""` for un-defaulted vars — i.e., **silent empty string, no exception**. The trace below treats AC-4 as the **rewritten** version per risk profile TECH-002 and NFR-AC-1 ("missing var → empty string + WARNING + skip exporter"). If the story author rejects the rewrite, AC-4 remains *unimplementable as written* and the test design's UNIT-006/UNIT-007 will fail by design.

---

## Requirement Mappings

### AC-1 — `trace_file`, `trace_exporter`, `trace_format` are passed through `expand_env_vars` before consumption

**Coverage: NONE** (planned: FULL once UNIT-001/002/003 land)

Planned Given-When-Then:
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-001`
  - Given: `settings = {auto_trace: true, trace_exporter: "file", trace_file: "${TEA_TRACE_FILE}"}` and `monkeypatch.setenv("TEA_TRACE_FILE", "/tmp/x.jsonl")`
  - When: `YAMLEngine` constructed (or `_configure_from_settings(settings)` invoked directly)
  - Then: `self._trace_context.exporters` contains exactly one `FileExporter` whose `filepath == "/tmp/x.jsonl"`
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-002`
  - Given: `trace_exporter: "${TEA_TRACE_EXPORTER}"` resolved to `"console"`, then to `"file"` (with `trace_file` set)
  - When: `_configure_from_settings` invoked
  - Then: Correct exporter type (`ConsoleExporter` / `FileExporter`) appended for each resolved value
- **Unit (P2, conditional)**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-003`
  - Given: `trace_format: "${TEA_TRACE_FORMAT}"` if/when engine consumes the key
  - When: `_configure_from_settings` invoked
  - Then: Resolved format value flows to the exporter; xfail+TODO if `trace_format` is not yet wired (engine currently does not read it — see Coverage Gap CG-1)

---

### AC-2 — `${VAR}` and `${VAR:-default}` syntax both work

**Coverage: NONE** (planned: FULL once UNIT-001 + UNIT-004 land)

Planned Given-When-Then:
- **Unit**: `TEA-DX-001.1-UNIT-001` (above) — covers `${VAR}` form
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-004`
  - Given: `trace_file: "${UNSET_VAR:-/tmp/default.jsonl}"`, `monkeypatch.delenv("UNSET_VAR", raising=False)`
  - When: `_configure_from_settings` invoked
  - Then: `FileExporter` constructed with `/tmp/default.jsonl`

---

### AC-3 — Literal values pass through unchanged

**Coverage: NONE** (planned: FULL once UNIT-005 lands)

Planned Given-When-Then:
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-005`
  - Given: `trace_file: "/tmp/literal.jsonl"` (no `${...}` markers)
  - When: `_configure_from_settings` invoked
  - Then: `FileExporter` constructed with `/tmp/literal.jsonl` byte-identical (no substitution mutation)

---

### AC-4 (rewritten) — Missing env var without default → empty string + WARNING + skip exporter

**Coverage: NONE** (planned: FULL once UNIT-006 + UNIT-007 land, contingent on AC-4 rewrite)

Planned Given-When-Then:
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-006`
  - Given: `trace_file: "${UNSET_VAR}"`, no default, env not set
  - When: `_configure_from_settings` invoked
  - Then: No exception raised; the post-expansion value of `trace_file` is `""`
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-007`
  - Given: same input as UNIT-006 plus `trace_exporter: "file"`, with `caplog.at_level("WARNING")`
  - When: `_configure_from_settings` invoked
  - Then: Exactly one `WARNING` is emitted naming `trace_file` (or the offending key) as empty after expansion; `_trace_context.exporters` length stays at 0

---

### AC-5 — Existing `examples/yaml/` workflows continue to load and run unchanged

**Coverage: NONE** (planned: FULL once INT-001 lands)

Planned Given-When-Then:
- **Integration**: `test_yaml_engine_observability.py::TEA-DX-001.1-INT-001` (parametrized over `examples/yaml/*.yaml`)
  - Given: each YAML file under `examples/yaml/`
  - When: `YAMLEngine(path)` constructed (no execution required)
  - Then: No exception is raised and no spurious `WARNING` about empty `trace_file` appears in `caplog`

---

### AC-6 — Expansion happens *before* `FileExporter(trace_file)` construction

**Coverage: NONE** (planned: FULL via UNIT-001 path-equality assertion)

Planned Given-When-Then:
- **Unit**: `TEA-DX-001.1-UNIT-001` (above) — the assertion that `FileExporter.filepath == "/tmp/x.jsonl"` (the *resolved* path, not the literal `"${TEA_TRACE_FILE}"` placeholder) proves construction-order correctness end-to-end.

---

### AC-7 — Narrow expansion only (no blanket-expand of the whole `settings:` dict)

**Coverage: NONE** (planned: FULL once UNIT-008 + INT-002 land)

Planned Given-When-Then:
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-008`
  - Given: `settings.variables.prompt_template = "Hello ${USER}"` with `USER` set to `alice`, plus a separate trace block for AC-1 happy path
  - When: `_configure_from_settings` invoked
  - Then: Post-call value of `settings["variables"]["prompt_template"]` equals literal `"Hello ${USER}"` (NOT `"Hello alice"`); trace path expands as expected
- **Integration**: `test_yaml_engine_observability.py::TEA-DX-001.1-INT-002`
  - Given: a YAML file containing `${VAR}` inside a `nodes[*].run` block
  - When: `YAMLEngine(yaml_path)` is constructed
  - Then: The `run` block source is preserved verbatim (no substitution applied to node code at load time)

---

### AC-8 — Unit test covers literal / `${VAR}` / `${VAR:-default}` / missing-var paths

**Coverage: NONE** (planned: FULL — UNIT-001/004/005/006/007 collectively satisfy the matrix)

Planned mapping is the union of the AC-1, AC-2, AC-3, AC-4 (rewritten) tests above. AC-8 is a coverage AC, not a behavior AC, so the trace is satisfied transitively when the constituent UNIT-* tests land.

---

### AC-9 — No new dependencies

**Coverage: NONE** (planned: FULL via INT-003)

Planned Given-When-Then:
- **Integration / CI guard**: `TEA-DX-001.1-INT-003`
  - Given: branch with implementation merged
  - When: `python/setup.py` and `python/pyproject.toml` are diffed against `main`
  - Then: No new entries appear under `install_requires` / `dependencies` (assertion executed inline in CI or as a one-off `pytest` marker)

---

### NFR-AC-1 (Reliability) — AC-4 rewritten to match `expand_env_vars` semantics

**Coverage: NONE** (planned: FULL via UNIT-006)

This is a contract / documentation NFR-AC. UNIT-006 is the executable proof that the missing-var path returns `""` (no exception). NFR-AC-1 is satisfied iff the story's AC-4 text is rewritten *and* UNIT-006 passes.

---

### NFR-AC-2 (Reliability) — `trace_exporter=file` + empty `trace_file` → WARNING + skip

**Coverage: NONE** (planned: FULL via UNIT-007)

UNIT-007 (above) is the canonical mapping. Mitigates TECH-005 (silent `FileExporter` drop).

---

### NFR-AC-3 (Security + Maintainability) — Doc note in `docs/shared/YAML_REFERENCE.md`

**Coverage: NONE** (planned: FULL via INT-004)

Planned Given-When-Then:
- **Integration / doc guard**: `TEA-DX-001.1-INT-004`
  - Given: `docs/shared/YAML_REFERENCE.md` post-merge
  - When: the `settings.trace_*` section is grepped
  - Then: the section mentions (a) `${VAR}` / `${VAR:-default}` syntax, (b) empty-string-on-missing → tracing skipped with WARNING, (c) operator-trust note (resolved path is written to filesystem as-is; trace payloads can be sensitive)

Doc-presence test is a hard guard against the SEC-001 / DATA-001 / OPS-002 risks rolling forward unmitigated.

---

### NFR-AC-4 (Maintainability, non-gating) — Inline comment listing expanded keys

**Coverage: NONE** (planned: FULL via INT-005)

Planned Given-When-Then:
- **Integration / source guard**: `TEA-DX-001.1-INT-005`
  - Given: `python/src/the_edge_agent/yaml_engine.py` post-merge
  - When: the narrow-expansion call site is grepped
  - Then: an inline comment near the `expand_env_vars(...)` call enumerates `trace_file`, `trace_exporter`, `trace_format` (mitigates TECH-001 — schema drift)

---

### Auxiliary — Idempotency / dict-merge side effects (TECH-004, P1)

**Coverage: NONE** (planned: FULL via UNIT-009)

Not a numbered AC, but called out by the test design as gate-relevant.

Planned Given-When-Then:
- **Unit**: `test_yaml_engine_observability.py::TEA-DX-001.1-UNIT-009`
  - Given: a settings dict with valid file-trace block
  - When: `_configure_from_settings(settings)` is invoked twice on the same engine
  - Then: `_trace_context.exporters` length stays at 1 (existing idempotency guard at `yaml_engine.py:986` is preserved)

---

## Traceability Matrix (compact)

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

---

## Critical Gaps

1. **CG-1 — `trace_format` not wired into the engine.**
   - **Requirement:** AC-1 explicitly lists `trace_format` as one of the keys to expand.
   - **Reality:** `yaml_engine.py:987-988` reads only `trace_exporter` and `trace_file`; `trace_format` is not consumed anywhere in `_configure_from_settings`. Adding expansion for a key the engine never reads is dead code.
   - **Risk:** Medium — schema drift between AC text and actual engine surface.
   - **Action (recommended):** Tighten AC-1 to drop `trace_format` (per test-design recommendation), or wire `trace_format` into the relevant exporter as part of this story and add UNIT-003 as a hard assertion (not xfail).

2. **CG-2 — AC-4 contradicts `expand_env_vars` semantics.**
   - **Requirement:** AC-4 says "missing env vars without a default raise the same error the existing `expand_env_vars` raises elsewhere".
   - **Reality:** `expand_env_vars` does **not** raise; it returns `""` (`base.py:535`).
   - **Risk:** High — without rewriting AC-4, the implementation cannot be both correct *and* AC-conformant; the DoD forbids modifying `expand_env_vars`.
   - **Action (must-fix before tests are authored):** Rewrite AC-4 per risk profile TECH-002 / NFR-AC-1 ("missing var → empty string; runner logs WARNING and skips `FileExporter` when `trace_exporter == 'file'` and post-expansion `trace_file` is empty").

3. **CG-3 — No narrowness regression test exists today (AC-7).**
   - **Requirement:** AC-7 forbids blanket-expanding `settings`.
   - **Reality:** No existing test verifies that a sibling `${...}`-bearing key (e.g., `settings.variables.prompt_template`) is left untouched. Without UNIT-008 / INT-002, a future refactor could silently break this guarantee.
   - **Risk:** Low today (implementation hasn't shipped), but Medium once shipped without the guard test.
   - **Action:** Land UNIT-008 + INT-002 in the same PR as the implementation.

4. **CG-4 — Operator-trust note absent from `YAML_REFERENCE.md`.**
   - **Requirement:** NFR-AC-3 + DoD documentation bullet.
   - **Reality:** Doc has no `settings.trace_*` env-expansion note.
   - **Risk:** SEC-001 + DATA-001 (env-driven file-write redirection / sensitive trace payload routing) ship undocumented — Low individually, Medium combined for an operator-facing surface.
   - **Action:** Land doc note + INT-004 grep guard.

---

## Test Design Recommendations

Adopt the test-design plan verbatim:

1. **P0 first:** UNIT-001, UNIT-004, UNIT-005, UNIT-006, UNIT-007, UNIT-008 — gate-relevant semantics + narrowness contract.
2. **P1 next:** UNIT-002, UNIT-009, INT-001, INT-002, INT-004 — secondary expansion + idempotency + regression breadth + doc presence.
3. **P2 last:** UNIT-003 (conditional on `trace_format` wiring), INT-003, INT-005 — dependency / comment guards.
4. **Tooling:** `pytest`, `monkeypatch.setenv` / `monkeypatch.delenv(..., raising=False)`, `caplog.at_level("WARNING")`, `tmp_path` only if `FileExporter.__init__` opens eagerly.
5. **Test file:** add to `python/tests/test_yaml_engine_observability.py` (closest existing module to the trace-config surface; matches existing import of `FileExporter` at line 27).

---

## Risk Assessment

- **High Risk:** AC-4 contract conflict (CG-2). Blocks both implementation and test authorship until rewritten.
- **Medium Risk:** `trace_format` schema drift (CG-1). Choose: drop from AC-1 *or* wire into the engine in this story.
- **Medium Risk** (post-merge if guards skipped): silent `FileExporter` drop (TECH-005, mitigated by UNIT-007); narrowness-regression (CG-3, mitigated by UNIT-008 + INT-002); undocumented operator-trust surface (CG-4, mitigated by INT-004).
- **Low Risk:** Idempotency (TECH-004, mitigated by UNIT-009); inline-comment schema-drift (TECH-001, mitigated by INT-005); dependency creep (AC-9, mitigated by INT-003).
- **Lowest Risk:** Literal pass-through (AC-3) and `${VAR:-default}` (AC-2) are pure `expand_env_vars` semantics already covered by `test_ltm_config_parsing.py::test_env_var_with_default_expansion` — UNIT-005 + UNIT-004 are mainly anchor tests.

---

## Gate YAML Block

```yaml
trace:
  totals:
    requirements: 16
    full: 0
    partial: 0
    none: 16
  planning_ref: 'docs/qa/assessments/TEA-DX-001.1-test-design-20260501.md'
  uncovered:
    - ac: 'AC-1'
      reason: 'No tests exist; story Draft. Planned: UNIT-001/002/003. trace_format wiring open (see CG-1).'
    - ac: 'AC-2'
      reason: 'Planned via UNIT-001 + UNIT-004; not yet authored.'
    - ac: 'AC-3'
      reason: 'Planned via UNIT-005; not yet authored.'
    - ac: 'AC-4'
      reason: 'AC text contradicts expand_env_vars (returns "", does not raise). Must rewrite per TECH-002 / NFR-AC-1 before authoring UNIT-006/007.'
    - ac: 'AC-5'
      reason: 'Planned via INT-001 examples-yaml smoke; not yet authored.'
    - ac: 'AC-6'
      reason: 'Covered transitively by UNIT-001 (FileExporter path == resolved path); not yet authored.'
    - ac: 'AC-7'
      reason: 'Planned via UNIT-008 + INT-002 (narrowness regression); not yet authored — high-value guard.'
    - ac: 'AC-8'
      reason: 'Coverage AC; satisfied by union of UNIT-001/004/005/006/007.'
    - ac: 'AC-9'
      reason: 'Planned via INT-003 (deps diff guard); not yet authored.'
    - ac: 'NFR-AC-1'
      reason: 'Requires AC-4 rewrite + UNIT-006.'
    - ac: 'NFR-AC-2'
      reason: 'Requires WARNING-on-empty implementation + UNIT-007.'
    - ac: 'NFR-AC-3'
      reason: 'YAML_REFERENCE.md note absent; INT-004 grep guard not yet authored.'
    - ac: 'NFR-AC-4'
      reason: 'Inline-comment guard (INT-005) not yet authored — non-gating.'
  blockers:
    - 'AC-4 must be rewritten to match expand_env_vars semantics (no exception; "" on missing) — without this, UNIT-006/007 are unimplementable as specified.'
    - 'Decide CG-1: tighten AC-1 to drop trace_format OR wire trace_format into the exporter in this story.'
  notes: 'See docs/qa/assessments/TEA-DX-001.1-trace-20260501.md. All 16 requirements currently uncovered (story Draft). Plan converts to FULL coverage when the 14 test-design scenarios land alongside implementation.'
```

---

## Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.1-trace-20260501.md
```

---

## Quality Indicators (post-implementation target)

- ✅ Every AC mapped to ≥1 test (achieved at planning level; pending authoring)
- ✅ Critical paths (AC-1 / AC-7) have unit + integration coverage
- ✅ Edge cases (missing-var, empty-after-expansion, narrowness regression) explicitly enumerated
- ✅ NFRs (Reliability + Security via doc note) have dedicated tests
- ✅ Clear Given-When-Then for each test in the test-design assessment
- ⚠️ Blockers must clear (AC-4 rewrite + CG-1 decision) before authoring begins
