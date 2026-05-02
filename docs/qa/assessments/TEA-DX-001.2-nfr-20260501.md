# NFR Assessment: TEA-DX-001.2

Date: 2026-05-01
Reviewer: Quinn (Test Architect) — `*nfr-assess` (YOLO / non-interactive mode)
Story: [TEA-DX-001.2 — CLI `--trace-file` flag](../../stories/TEA-DX-001.2-cli-trace-file-flag.md)
Scope assessed: core four (security, performance, reliability, maintainability)

## Summary

- **Security:** PASS — User-controlled path bounded by OS perms; reuses vetted `expand_env_vars`; CLI is local single-user.
- **Performance:** PASS — Additive flag, no hot-path impact; trace I/O cost is pre-existing `FileExporter` behavior.
- **Reliability:** CONCERNS — Bad-path / unwritable-parent handling is a known risk (R5) but not yet codified as a required AC; one negative test is recommended (currently P2).
- **Maintainability:** PASS — Single CLI flag, clear ACs, unit/CLI tests planned, docs update planned (AC-11), reuses existing `YAMLEngine(trace_file=...)` constructor at `python/src/the_edge_agent/yaml_engine.py:106`.

Quality score: **90/100** (100 − 10 for one CONCERNS NFR).

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
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
    notes: 'Exporter init failure on bad path / unwritable parent dir (R5) should surface as typer.BadParameter, not a raw traceback. P2 negative test in risk profile not yet promoted to AC; recommend AC-12 below.'
  maintainability:
    status: PASS
    notes: 'Single Typer flag wired to existing constructor kwarg; AC-10 unit/CLI tests + AC-11 docs cover the change. Backward compatible per AC-7.'
```

## Critical Issues

None. No FAIL-level NFRs.

## Concerns & Missing Considerations

### Reliability — opaque error on bad trace path (R5)

- **Risk:** `--trace-file /nonexistent/dir/foo.jsonl` (or read-only parent, or path pointing at an existing directory) will today bubble up as an `OSError` traceback from `FileExporter` rather than a typer-formatted error referencing the `--trace-file` flag.
- **Why it matters here:** This story is explicitly aimed at **external runners** (intake.py-style scripts). External runners parse stderr / exit codes, so a clean error message is part of the contract.
- **Fix:** Wrap `FileExporter(path)` (or its first write) in cli.py such that I/O failure raises `typer.BadParameter("--trace-file: <reason>")`. Cost: ~30 min, one negative test.

### Security — path expansion parity (R4, low)

- AC-5 demands `${ENV_VAR}` expansion via `expand_env_vars`. Confirm parity with TEA-DX-001.1 (`${VAR:-default}` form must produce identical resolved path whether supplied via YAML setting or CLI flag) — a single parity test closes this. Already enumerated as P1 test #6 in the risk profile.

### Reliability — implicit-enable surprise (R2)

- AC-2 and AC-4 enable file tracing implicitly when `--trace-file` is supplied, even if the YAML opted out (`auto_trace: false` or `trace_exporter: console`). This is intended behavior, but `--help` text and CLI reference must spell it out so external runners don't perceive it as silent override. Already covered by AC-6 + AC-11; flagging here so docs reviewer doesn't gloss.

## Test Recommendations

The risk profile (already in the story) enumerates P0–P3 tests. NFR-specific must-haves:

| # | NFR | Test | Priority |
|---|-----|------|----------|
| T1 | Reliability | Bad path (`--trace-file /nonexistent/dir/foo.jsonl`) → typer.BadParameter, not traceback | **P1** (promote from P2) |
| T2 | Reliability | Unwritable parent dir → clean error referencing `--trace-file` | P2 |
| T3 | Security | Env-var parity: same `${VAR:-default}` resolves identically via YAML and CLI flag | P1 (already in profile as #6) |
| T4 | Maintainability | `tea run --help` snapshot includes `--trace-file` line (AC-6 smoke) | P1 (already in profile as #7) |
| T5 | Reliability | No-flag baseline: byte-identical pre-change behavior with `--quiet`/`--stream` | P0 (already in profile as #2) |
| T6 | Performance | Optional sanity: invocation overhead delta of adding the flag is <5% on a small graph (only if perf-sensitive callers exist) | P3 |

## Recommended Acceptance Criteria Addition

Add **AC-12** to close the reliability gap:

> **AC-12:** When `--trace-file` points to a path whose parent directory does not exist or is not writable, `tea run` exits with a clean error message referencing the `--trace-file` flag (e.g., via `typer.BadParameter`), not a raw Python traceback.

This converts R5 mitigation from advisory to required.

## Quick Wins

- Wrap exporter init in a `typer.BadParameter` guard: ~30 min, +T1 test.
- Add `--help` text spelling out implicit-enable (AC-2/AC-4): ~10 min in the same diff that adds the flag.
- Snapshot `tea run --help` in tests so AC-6 is mechanically enforced: ~15 min, +T4 test.

## Sign-off

NFR posture is **PASS with one CONCERNS** (reliability, error-surfacing on bad path). Story is safe to implement; promote risk-profile P2 test #9 to P1 / add AC-12 before merge to clear the CONCERNS.

NFR assessment: docs/qa/assessments/TEA-DX-001.2-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.2-cli-trace-file-flag.yml under `nfr_validation`
