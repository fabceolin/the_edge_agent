# Test Design: Story TEA-DX-001.1 — Settings-block env & template expansion

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO

## Test Strategy Overview

- **Total test scenarios:** 14
- **Unit tests:** 9 (64%)
- **Integration tests:** 5 (36%)
- **E2E tests:** 0 (0%)
- **Priority distribution:** P0: 7, P1: 5, P2: 2

Strategy notes:
- This is a narrow, additive config-plumbing story (≤3 string keys passed through `expand_env_vars`). The shift-left bias is strong: most behavior is verifiable by calling `_configure_from_settings` (or constructing a `YAMLEngine`) on a synthetic settings dict and inspecting `self._trace_context.exporters` and `caplog`.
- **No E2E tests are warranted.** There is no user journey, no cross-system flow, and no UX surface — `expand_env_vars` is a pure string substitution and the trace exporter wiring is already covered at integration level for the existing `console`/`file`/`opik` branches.
- The "integration" tier here means *full YAML load → engine construction* (smoke + narrowness regression + examples/yaml). Anything below that is unit-level.
- Risk profile flags TECH-002 (AC-4 contradicts helper semantics) and TECH-005 (silent FileExporter drop) as the highest-priority risks. Both gate the implementation and have dedicated P0 tests below. **AC-4 is assumed rewritten per risk profile and NFR-AC-1** ("missing var → empty string"); if AC-4 is not rewritten before tests are authored, UNIT-004 below will fail by design and serve as the forcing function.
- Tests are added to `python/tests/test_yaml_engine_observability.py` (closest existing file to the trace-config surface). New fixtures are minimal — synthetic dicts, no real LLM/network/DB.

## Test Scenarios by Acceptance Criteria

### AC-1 — `trace_file`, `trace_exporter`, `trace_format` are passed through `expand_env_vars` before consumption

| ID                    | Level       | Priority | Test                                                                                                                                                                                                       | Justification                                                                          |
| --------------------- | ----------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-001 | Unit        | P0       | `settings = {auto_trace: true, trace_exporter: file, trace_file: "${TEA_TRACE_FILE}"}` with env var set → `_trace_context.exporters` contains a `FileExporter` whose path equals the env value             | Core happy path; covers AC-1 (`trace_file`) and AC-6 (resolved before `FileExporter`)  |
| TEA-DX-001.1-UNIT-002 | Unit        | P1       | `trace_exporter: "${TEA_TRACE_EXPORTER}"` resolved to `console` → `ConsoleExporter` is appended; resolved to `file` (with `trace_file` set) → `FileExporter` appended                                       | Covers `trace_exporter` expansion (AC-1)                                               |
| TEA-DX-001.1-UNIT-003 | Unit        | P2       | `trace_format` key, if present in the schema and consumed by an exporter, expands `${VAR}` correctly. Skip-with-comment if `trace_format` is not yet wired into the exporter — track with a TODO assertion | AC-1 explicitly lists `trace_format`; verify schema reality before asserting behavior  |

### AC-2 — `${VAR}` and `${VAR:-default}` both supported

| ID                    | Level       | Priority | Test                                                                                                                              | Justification                                                              |
| --------------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-004 | Unit        | P0       | `trace_file: "${UNSET_VAR:-/tmp/default.jsonl}"` with `UNSET_VAR` not set → `FileExporter` constructed with `/tmp/default.jsonl` | Default-fallback contract; mitigates TECH-002 confusion by anchoring expected behavior |

### AC-3 — Literal values (no `${...}` markers) pass through unchanged

| ID                    | Level       | Priority | Test                                                                                                              | Justification                                                |
| --------------------- | ----------- | -------- | ----------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| TEA-DX-001.1-UNIT-005 | Unit        | P0       | `trace_file: "/tmp/literal.jsonl"` (no markers) → `FileExporter` constructed with `/tmp/literal.jsonl` unchanged | Backwards-compat regression guard; cheapest possible test    |

### AC-4 (REWRITTEN per risk) — Missing env var without default expands to `""`

> ⚠️ **Pre-condition:** This AC must be rewritten before implementation per risk profile TECH-002 and NFR-AC-1. The story author was instructed to update AC-4 to: *"Missing env vars without a default expand to the empty string (matching `expand_env_vars` semantics in `python/src/the_edge_agent/memory/base.py:528-537`); the runner MUST log a `WARNING` and skip appending a `FileExporter` when `trace_exporter == 'file'` and post-expansion `trace_file` is empty."* Tests below assume the rewritten AC.

| ID                    | Level       | Priority | Test                                                                                                                                                                       | Justification                                                                                |
| --------------------- | ----------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-006 | Unit        | P0       | `trace_file: "${UNSET_VAR}"`, no default, env not set → no exception; `trace_file` resolves to `""`                                                                        | Anchors actual `expand_env_vars` semantics; mitigates TECH-002                               |
| TEA-DX-001.1-UNIT-007 | Unit        | P0       | Same input + `trace_exporter: file` → exactly one `WARNING` emitted (caplog, INFO+) naming `trace_file` as empty after expansion; `_trace_context.exporters` length is 0 | Mitigates TECH-005 silent-drop; satisfies NFR-AC-2                                           |

### AC-5 — Existing `examples/yaml/` workflows continue to load and run unchanged

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                                  | Justification                                                |
| --------------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| TEA-DX-001.1-INT-001  | Integration | P1       | Parametrized smoke: for every `examples/yaml/*.yaml`, instantiate `YAMLEngine(yaml_path)` (no execution required) and assert no exception, no spurious WARNING about empty `trace_file`. Reuses any existing examples-smoke fixture if present                          | Backwards-compat guard; cheapest broad regression net        |

### AC-6 — Expansion happens before `FileExporter(trace_file)` construction

Covered by **TEA-DX-001.1-UNIT-001** (FileExporter receives the resolved path, not the literal `${...}`).

### AC-7 — Implementation is narrow; does not blanket-expand the entire `settings:` block

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                              | Justification                                                              |
| --------------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-008 | Unit        | P0       | `settings.variables.prompt_template = "Hello ${USER}"` (with `USER` exported) survives `_configure_from_settings` literally — assert post-method-call settings dict (or engine state) still contains the literal `"Hello ${USER}"` | Mitigates TECH-003 narrowness regression; AC-7 enforcement                 |
| TEA-DX-001.1-INT-002  | Integration | P1       | Full YAML load with a `${VAR}` in `nodes[*].run` source code — `${VAR}` is preserved verbatim in the compiled run function source, not pre-expanded by the trace narrow-expansion path                                            | Defends against accidental whole-`settings`-tree expansion at YAML level   |

### AC-8 — Unit test covers literal, `${VAR}`, `${VAR:-default}`, missing-required-var

Satisfied collectively by **UNIT-001 (literal-via-resolved-env), UNIT-004 (defaulted), UNIT-005 (literal), UNIT-006 (missing → empty), UNIT-007 (missing + warning)**. AC-8 explicitly mentions a "missing-required-var error path" — that bullet must be revised in lockstep with AC-4 to read "missing-required-var → empty string + warning path". Tests above already cover the corrected wording.

### AC-9 — No new dependencies

| ID                    | Level       | Priority | Test                                                                                                                                | Justification                                  |
| --------------------- | ----------- | -------- | ----------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| TEA-DX-001.1-INT-003  | Integration | P2       | `pip install -e .` (CI job) succeeds without new requirements; `python/setup.py` and `python/pyproject.toml` diff is implementation-only (no new entries under `install_requires` / `dependencies`) | Hard-requirement; verified by CI dependency review |

### Additional NFR-driven scenarios (from NFR assessment)

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                | Justification                                                          |
| --------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- |
| TEA-DX-001.1-UNIT-009 | Unit        | P1       | Re-invoking `_configure_from_settings` does not double-append a `FileExporter` when `_trace_context.exporters` is already populated (existing guard at `yaml_engine.py:986`); confirms narrow-expansion didn't break this gate | Defends existing idempotency contract preserved by AC-7 narrowness    |
| TEA-DX-001.1-INT-004  | Integration | P1       | YAML_REFERENCE.md doc note exists: a docs-link-or-grep test that asserts a section under `settings.trace_*` mentions (a) `${VAR}` / `${VAR:-default}` semantics, (b) empty-string-on-missing → tracing skipped with warning, (c) operator-trust note | Closes NFR-AC-3 (Security + Maintainability); cheap doc presence check |
| TEA-DX-001.1-INT-005  | Integration | P2       | Inline-comment-presence test: grep `python/src/the_edge_agent/yaml_engine.py` near the narrow-expansion call site for the literal substrings `trace_file`, `trace_exporter`, `trace_format` in a comment — fails if a future trace key is added without comment update | Closes NFR-AC-4 schema-drift guard for TECH-001                        |

## Risk Coverage

Maps to risks in `docs/qa/assessments/TEA-DX-001.1-risk-20260501.md`:

| Risk ID  | Severity     | Mitigated by                                                                                                                  |
| -------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| TECH-002 | High (6)     | UNIT-006 (anchors empty-string semantics), UNIT-007 (warning on empty); both **assume AC-4 rewritten** per risk profile + NFR |
| TECH-005 | Medium (4)   | UNIT-007 (caplog + exporters length unchanged)                                                                                |
| TECH-003 | Low (2)      | UNIT-008 (sibling `${...}` preservation), INT-002 (run-block source preservation)                                             |
| OPS-001  | Low (2)      | Not directly tested — `FileExporter` already fails loud on permission errors; covered by doc note in INT-004                  |
| OPS-002  | Low (2)      | INT-004 (doc-note presence check)                                                                                             |
| SEC-001  | Low (2)      | Doc note (INT-004); no functional test (operator-trust threat model is documentation-only)                                    |
| DATA-001 | Low (2)      | Doc note (INT-004); no functional test                                                                                        |
| TECH-001 | Minimal (1)  | INT-005 (inline-comment grep guard)                                                                                           |
| TECH-004 | Minimal (1)  | UNIT-009 (idempotency); covers the dict-merge-side-effect concern                                                             |

## Test Data & Environment Requirements

**Fixtures / dicts required:**

1. **In-test settings dicts** — no YAML files needed for UNIT-001..009. Each test constructs a synthetic `settings` dict and either:
   - calls `_configure_from_settings(settings)` directly on a `YAMLEngine` instance with a minimal valid graph, or
   - instantiates `YAMLEngine` from an in-memory string via `yaml.safe_dump`.
2. **Existing `examples/yaml/*.yaml`** — used as-is by INT-001 (parametrized smoke). No new example fixtures.
3. **No new tmp-file fixtures** — `FileExporter` is constructed but the file is not necessarily written during `_configure_from_settings`; tests assert constructor input, not file I/O. If `FileExporter(__init__)` opens the file eagerly, use `tmp_path` from pytest.

**Environment / tooling:**

- Python ≥ 3.9 (project floor); `pytest`; `monkeypatch` for env var manipulation; `caplog` for warning assertions.
- No network, no DB, no LLM credentials, no LTM backend.
- `monkeypatch.setenv("TEA_TRACE_FILE", "/tmp/x.jsonl")` and `monkeypatch.delenv(..., raising=False)` are the only env primitives needed.

**Sample assertion shapes:**

Happy-path (UNIT-001):
```python
def test_trace_file_env_expansion_resolves(monkeypatch, tmp_path):
    monkeypatch.setenv("TEA_TRACE_FILE", str(tmp_path / "trace.jsonl"))
    engine = _make_minimal_engine()
    settings = {
        "auto_trace": True,
        "trace_exporter": "file",
        "trace_file": "${TEA_TRACE_FILE}",
    }
    engine._configure_from_settings(settings)
    exporters = engine._trace_context.exporters
    assert len(exporters) == 1
    assert isinstance(exporters[0], FileExporter)
    assert str(tmp_path / "trace.jsonl") in repr(exporters[0])  # or .path attr
```

Empty-after-expansion (UNIT-007):
```python
def test_trace_file_empty_after_expansion_warns(monkeypatch, caplog):
    monkeypatch.delenv("TEA_TRACE_FILE", raising=False)
    engine = _make_minimal_engine()
    settings = {
        "auto_trace": True,
        "trace_exporter": "file",
        "trace_file": "${TEA_TRACE_FILE}",  # unset, no default
    }
    with caplog.at_level("WARNING"):
        engine._configure_from_settings(settings)
    assert engine._trace_context.exporters == []
    warnings = [r for r in caplog.records if r.levelname == "WARNING"]
    assert any("trace_file" in r.message for r in warnings)
    assert len(warnings) == 1
```

Narrowness regression (UNIT-008):
```python
def test_narrow_expansion_does_not_touch_variables(monkeypatch):
    monkeypatch.setenv("USER", "alice")
    engine = _make_minimal_engine()
    settings = {
        "auto_trace": True,
        "trace_exporter": "console",
        "variables": {"prompt_template": "Hello ${USER}"},
    }
    engine._configure_from_settings(settings)
    # variables sub-tree must remain literal — narrow expansion must not touch it
    assert engine.variables.get("prompt_template") == "Hello ${USER}"
```

Defaulted-fallback (UNIT-004):
```python
def test_trace_file_default_fallback(monkeypatch):
    monkeypatch.delenv("TEA_TRACE_FILE", raising=False)
    engine = _make_minimal_engine()
    settings = {
        "auto_trace": True,
        "trace_exporter": "file",
        "trace_file": "${TEA_TRACE_FILE:-/tmp/default.jsonl}",
    }
    engine._configure_from_settings(settings)
    assert any(
        isinstance(e, FileExporter) and "/tmp/default.jsonl" in repr(e)
        for e in engine._trace_context.exporters
    )
```

## Recommended Execution Order

1. **P0 Unit** (UNIT-001, UNIT-004, UNIT-005, UNIT-006, UNIT-007, UNIT-008) — fail-fast on the high-risk semantics and narrowness contract.
2. **P1 Unit** (UNIT-002, UNIT-009) — secondary expansion paths and idempotency.
3. **P1 Integration** (INT-001, INT-002, INT-004) — examples-yaml smoke, code-block preservation, doc note.
4. **P2 Unit/Integration** (UNIT-003, INT-003, INT-005) — `trace_format` schema reality check, dependency review, inline-comment guard.

## Coverage Gaps

None **provided AC-4 is rewritten before implementation** per risk profile TECH-002 and NFR-AC-1. If AC-4 is left unchanged, UNIT-006/UNIT-007 will not match the AC text — they will match the helper's actual behavior. In that scenario the gap is in the AC, not the test plan.

The `trace_format` test (UNIT-003) is contingent on `trace_format` being read by the engine. Today, `_configure_from_settings` does **not** read `trace_format` — only `trace_exporter` and `trace_file` are consumed (see `yaml_engine.py:987-988`). If `trace_format` is not part of the schema by the time this story lands, UNIT-003 should be marked `xfail` with a TODO referencing the AC, OR AC-1 should be tightened to drop the `trace_format` mention. **Recommend the latter** to avoid carrying a dormant test.

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 14
  by_level:
    unit: 9
    integration: 5
    e2e: 0
  by_priority:
    p0: 7
    p1: 5
    p2: 2
  coverage_gaps:
    - 'AC-1 trace_format: requires schema confirmation; engine does not currently consume trace_format. Resolve by either tightening AC-1 to drop trace_format, or extending engine + UNIT-003 to consume it.'
  preconditions:
    - 'AC-4 must be rewritten per risk profile TECH-002 / NFR-AC-1 (missing var → empty string, no exception). UNIT-006 and UNIT-007 encode the rewritten contract; without the AC rewrite they will not match the story text.'
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.1-test-design-20260501.md
P0 tests identified: 7
```

## Quality Checklist

- [x] Every AC (1–9) has at least one test scenario, plus three NFR-driven additions
- [x] Test levels are appropriate — pure substitution + dict-mutation logic at unit; YAML load + doc/source presence at integration; no E2E warranted
- [x] No duplicate coverage across levels — happy-path FileExporter wiring is unit-only; integration tier covers regression breadth (examples-yaml, run-block source, docs, comment guard)
- [x] Priorities align with risk profile — TECH-002 (High) and TECH-005 (Medium) both have P0 unit coverage
- [x] Test IDs follow naming convention `TEA-DX-001.1-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent — each `monkeypatch.setenv` / `monkeypatch.delenv` is scoped per test; no shared mutable state
