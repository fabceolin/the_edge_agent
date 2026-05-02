# Risk Profile: Story TEA-DX-001.1

Story: Settings-block env & template expansion
Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Mode: YOLO

## Executive Summary

- Total Risks Identified: 9
- Critical Risks: 0
- High Risks: 1 (TECH-002 — AC vs. helper behavior mismatch)
- Medium Risks: 1 (TECH-005 — empty-string expansion silently drops file exporter)
- Low Risks: 5
- Minimal Risks: 2
- **Risk Score: 73 / 100**
- **Suggested Gate (deterministic mapping):** CONCERNS — driven by TECH-002 (score 6); resolve by clarifying AC-4 against `expand_env_vars` actual behavior, or harden the helper.

## Risk Matrix

| Risk ID  | Description                                                                | Probability | Impact     | Score | Priority |
| -------- | -------------------------------------------------------------------------- | ----------- | ---------- | ----- | -------- |
| TECH-002 | AC-4 expects "missing required var raises", helper returns `""`           | High (3)    | Medium (2) | 6     | High     |
| TECH-005 | Empty expansion → `trace_file` falsy → file exporter silently skipped     | Medium (2)  | Medium (2) | 4     | Medium   |
| TECH-003 | Narrow expansion accidentally widens to keys with legitimate `${...}`     | Low (1)     | Medium (2) | 2     | Low      |
| OPS-001  | Resolved trace path lands in non-writable / unexpected location           | Medium (2)  | Low (1)    | 2     | Low      |
| OPS-002  | Doc note in `YAML_REFERENCE.md` missed → feature undiscoverable           | Medium (2)  | Low (1)    | 2     | Low      |
| SEC-001  | Env-driven trace path enables file-write redirection if env is untrusted  | Low (1)     | Medium (2) | 2     | Low      |
| DATA-001 | Env-driven path routes sensitive trace payloads to shared/networked store | Low (1)     | Medium (2) | 2     | Low      |
| TECH-001 | Future trace keys (e.g., `trace_dir`) added without extending narrow list | Low (1)     | Low (1)    | 1     | Minimal  |
| TECH-004 | Dict-merge of expanded subset mutates `settings`; downstream consumers    | Low (1)     | Low (1)    | 1     | Minimal  |

## High Risks Requiring Attention

### TECH-002: AC-4 contradicts `expand_env_vars` real behavior

**Score: 6 (High)**
**Probability:** High — `python/src/the_edge_agent/memory/base.py:528-537` substitutes the empty string when an env var is missing and no default is provided; it does **not** raise. AC-4 ("Missing env vars without a default raise the same error the existing `expand_env_vars` raises elsewhere") asserts a contract that no implementation in the repo satisfies.
**Impact:** Medium — implementer following AC-4 literally will either (a) write a test that hangs/fails, (b) modify `expand_env_vars` (explicitly forbidden by DoD: "No changes to `expand_env_vars` itself"), or (c) wrap with a separate strict expander, leading to two divergent expansion semantics in the codebase.

**Mitigation (must-fix before Done):**
- Update AC-4 to match reality: "Missing env vars without a default expand to the empty string; the file exporter MUST log a warning and skip creating the exporter when `trace_exporter == 'file'` and `trace_file` is empty after expansion."
- Or, if strict-mode is genuinely desired, lift it into a new sub-story (changes the contract for ltm/secrets/firestore too — out of scope here).

**Testing focus:**
- Replace the "missing-required-var error path" test case in Task 2 with: (i) missing var with no default → empty string, (ii) `trace_exporter=file` + empty `trace_file` → no FileExporter appended, warning logged.

---

## Medium Risks

### TECH-005: Silent loss of trace output when env var expands to empty

**Score: 4 (Medium)**
**Probability:** Medium — operators routinely forget to export `TEA_TRACE_FILE` in CI / cron jobs.
**Impact:** Medium — the existing guard `elif trace_exporter == "file" and trace_file:` (`yaml_engine.py:991`) silently drops the exporter when `trace_file` is `""`. The user thinks tracing is on; nothing is written. Debugging this is hard because the YAML "looks right".

**Mitigation:**
- When `trace_exporter == "file"` but post-expansion `trace_file` is empty/None, log `WARNING` with the offending key and the env var name parsed from the source string.
- Add unit test asserting the warning is emitted and `_trace_context.exporters` remains empty.

**Testing focus:** Caplog-style assertion in pytest, plus a smoke test confirming default-fallback (`${TEA_TRACE_FILE:-/tmp/trace.jsonl}`) still resolves correctly.

---

## Low Risks (summary)

- **TECH-003** — Story already mandates narrow expansion (AC-7) referencing existing pattern at `yaml_engine.py:1015,1052,1104`. Reviewer must reject any PR diff that pre-expands the whole `settings` dict.
- **OPS-001** — `FileExporter` will fail loud on permission errors; acceptable. Add a single sentence to YAML_REFERENCE.md recommending absolute paths and writable directories.
- **OPS-002** — DoD already requires the doc note; tracked there, listed here so reviewer doesn't lose sight.
- **SEC-001** / **DATA-001** — Env vars are typically operator-controlled in this project's deployment model. Document the threat model in YAML_REFERENCE.md so integrators in less-trusted contexts see the warning.

## Minimal Risks

- **TECH-001** — Mitigated by adding a single TODO comment near the narrow-expansion call listing the trace keys, so future trace keys are noticed during PR review.
- **TECH-004** — The suggested `{**settings, **expand_env_vars(subset)}` pattern is fine; downstream consumers re-read their own sub-blocks. Confirmed no current cross-block reads of `trace_*`.

## Risk Distribution

### By Category

- Technical (TECH): 5 risks (0 critical, 1 high, 1 medium, 2 low/minimal)
- Operational (OPS): 2 risks (0 critical, 2 low)
- Security (SEC): 1 risk (low)
- Data (DATA): 1 risk (low)
- Performance / Business: 0 risks

### By Component

- `python/src/the_edge_agent/yaml_engine.py:982-996` (auto-trace block): 4 risks
- `python/src/the_edge_agent/memory/base.py:505-545` (`expand_env_vars` helper, read-only here): 1 risk (TECH-002)
- `docs/shared/YAML_REFERENCE.md`: 3 risks (documentation gaps)
- Test suite (`tests/test_yaml_engine_*.py`): 1 risk (test coverage of empty-expansion path)

## Risk-Based Testing Strategy

### Priority 1 — High-Risk Coverage (must add)

1. AC clarification test: env var missing → empty string (NOT exception). Confirms TECH-002 resolution.
2. `trace_exporter=file` + empty `trace_file` after expansion → warning logged, no exporter appended (TECH-005).
3. `${VAR:-/tmp/default.jsonl}` with `VAR` unset → exporter created with `/tmp/default.jsonl` (default-fallback path).

### Priority 2 — Acceptance Criteria Coverage

4. Literal path with no markers passes through unchanged (AC-3).
5. `${VAR}` resolved when set (AC-1, AC-2).
6. Narrowness check: a sibling key under `settings` (e.g., `variables.prompt_template: "Use ${name}"`) survives expansion untouched (AC-7).
7. `trace_format` expansion if/when the schema accepts it (AC-1; if not in schema, document and skip).

### Priority 3 — Regression

8. Smoke-load every `examples/yaml/*.yaml` with the existing harness; assert no behavior delta (AC-5).

## Risk Acceptance Criteria

### Must Fix Before Done

- TECH-002: rewrite AC-4 (or open a follow-up story for strict expansion) — blocking the story without this leaves the test suite ambiguous.
- TECH-005: warning + test for empty-after-expansion case.
- OPS-002: YAML_REFERENCE.md note (already in DoD).

### Acceptable with Mitigation

- SEC-001 / DATA-001 / OPS-001: covered by a single "operator responsibility" paragraph in YAML_REFERENCE.md.

### Accepted

- TECH-001, TECH-004: minimal risk; no action beyond a single inline comment listing the narrow key set.

## Monitoring Requirements

- Watch for log-warning rate on `trace_exporter=file` + empty path post-deploy; spikes indicate operator misconfiguration.
- No metrics required — this is a config-time path, not request-path.

## Risk Review Triggers

Re-run risk-profile if:
- New `trace_*` keys are added to the YAML schema (extends the narrow expansion list).
- `expand_env_vars` semantics change (e.g., strict mode added).
- Trace files start carrying redacted PII (raises DATA-001 to medium).

---

## Gate YAML Block (paste into gate file under `risk_summary`)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 1
    medium: 1
    low: 5
    minimal: 2
  highest:
    id: TECH-002
    score: 6
    title: 'AC-4 contradicts expand_env_vars actual behavior (returns "" rather than raising)'
  recommendations:
    must_fix:
      - 'Rewrite AC-4 to match expand_env_vars actual semantics (empty string on missing var, no exception)'
      - 'Log WARNING + skip FileExporter when trace_exporter=file and post-expansion trace_file is empty'
      - 'Add YAML_REFERENCE.md note for settings.trace_* env expansion (already in DoD)'
    monitor:
      - 'Track rate of empty-trace_file warnings post-deploy as operator-misconfig signal'
```

## Story Hook Line

Risk profile: docs/qa/assessments/TEA-DX-001.1-risk-20260501.md
