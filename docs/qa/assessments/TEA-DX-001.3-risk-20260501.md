# Risk Profile: Story TEA-DX-001.3

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-DX-001.3 - Intermediate state dumps for debug
Mode: YOLO

## Executive Summary

- **Total Risks Identified:** 10
- **Critical Risks:** 0
- **High Risks:** 1 (SEC-001 — Secret leakage via debug dump)
- **Medium Risks:** 3
- **Low/Minimal Risks:** 6
- **Overall Risk Score:** 63/100 (CONCERNS — gate guidance)

The dominant risk is the security boundary between in-memory state and disk. The feature is fundamentally a developer-experience aid, but it introduces a brand new sink for state data that historically only lived in memory or in encrypted/redacted checkpoints. If the redaction reuse promised by AC-4 is incomplete, this becomes a credential-leak vector. Disk-fill and operator-misuse-on-prod risks are next-tier concerns.

## Risk Matrix

| Risk ID  | Category | Description                                                          | Probability | Impact     | Score | Priority |
| -------- | -------- | -------------------------------------------------------------------- | ----------- | ---------- | ----- | -------- |
| SEC-001  | security | Secret/PII leakage to disk if checkpoint redaction is incomplete     | Medium (2)  | High (3)   | 6     | High     |
| DATA-001 | data     | Disk fill from large state on long workflows (no size cap in v1)     | Medium (2)  | Medium (2) | 4     | Medium   |
| OPS-001  | ops      | Flag accidentally enabled in production / left on in CI              | Medium (2)  | Medium (2) | 4     | Medium   |
| TECH-001 | tech     | Parallel/dynamic_parallel branch interleaving violates AC-9 ordering | Medium (2)  | Medium (2) | 4     | Medium   |
| SEC-002  | security | Path traversal via crafted node names in `<NN>-after-<node>.json`    | Low (1)     | Medium (2) | 2     | Low      |
| TECH-002 | tech     | Synchronous I/O in event loop slows execution                        | Low (1)     | Medium (2) | 2     | Low      |
| TECH-003 | tech     | FAILED-state write itself fails (disk full, perms) and masks origin  | Low (1)     | Medium (2) | 2     | Low      |
| DATA-002 | data     | Stale dumps from prior run mix with new run (counter overlap)        | Medium (2)  | Low (1)    | 2     | Low      |
| OPS-002  | ops      | User confusion: empty `--output` plus dumps disagree on final state  | Low (1)     | Low (1)    | 1     | Minimal  |
| DATA-003 | data     | Filename injection via node names containing `/` or `..`             | Low (1)     | Medium (2) | 2     | Low      |

(Sorted by score desc.)

## Critical Risks Requiring Immediate Attention

None. No risk scored 9.

## High Risks

### 1. SEC-001: Secret/PII leakage via debug dump

**Score: 6 (High)**
**Probability:** Medium — the story explicitly defers redaction to "the same logic that excludes secrets from checkpoints" (AC-4). If that redaction has gaps (e.g., new secret types added since TEA-BUILTIN-012.3, nested dicts, secrets stored as plain string fields, exception traceback locals containing creds), debug dumps will surface them. Probability of *some* leak path existing in any realistic state is moderate.
**Impact:** High — credentials/PII written unencrypted to local disk are easy to commit, ship in bug reports, attach to tickets, or leave on shared dev machines. A single leak meaningfully damages trust.

**Mitigation:**
- **Preventive:** AC-4 requires reuse of checkpoint redaction — confirm by code path, not by faith. Add an integration test that constructs a state containing every redaction marker the checkpoint codepath claims to handle, runs `--debug-state`, and asserts none appear in the dumps.
- **Preventive:** AC-3's FAILED dump includes the *traceback*. Tracebacks can carry repr() of locals (e.g., `KeyError: 'sk-...'`). Audit that the traceback formatter does not emit unredacted secret material from exception args / `__cause__` chains.
- **Detective:** Add a test that a deliberate `BadSecret("sk-LEAK-CANARY-XYZ")` raised from inside a node never appears in the FAILED dump's traceback string.
- **Documentation (AC-12):** Make the README warning blunt — "this writes near-checkpoint-equivalent data to plaintext disk; do not enable in production or share output files without inspection."

**Testing Focus:** Dedicated SEC test class (see Priority 1 below).

## Medium Risks

### 2. DATA-001: Disk fill from large state

**Score: 4 (Medium)**
**Probability:** Medium — long-running workflows with large intermediate payloads (e.g., LLM context, Parquet rows passed in state, batch results) can each be MB-scale × N nodes × repeated runs.
**Impact:** Medium — fills CI runners and dev disks; can cascade into pytest fixtures or cause Docker layers to bloat.

**Mitigation:** Story explicitly defers the size cap. Acceptable for v1 but document the tradeoff. Consider follow-up TEA-DX-001.3.1 for a `--debug-state-max-bytes` cap.
**Testing Focus:** No explicit test required for v1; document expected behavior.

### 3. OPS-001: Flag accidentally enabled in production

**Score: 4 (Medium)**
**Probability:** Medium — a CLI flag is easy to leave in a startup script, Dockerfile, or shell alias.
**Impact:** Medium — sustained writes leak data and fill disk in environments where the operator isn't watching.

**Mitigation:**
- AC-12 doc warning is necessary but not sufficient. Consider emitting a single `WARNING` log line at startup ("--debug-state is enabled; intended for development only") that survives `--quiet`.
- Optionally check if running in a non-interactive/production-like env (no TTY) and elevate the warning.
**Testing Focus:** Test that the warning is emitted unconditionally when the flag is set (independent of `--quiet`).

### 4. TECH-001: Parallel branch ordering violates AC-9

**Score: 4 (Medium)**
**Probability:** Medium — TEA's parallel and `dynamic_parallel` engines emit completion events for child branches that may interleave with parent-node completion. AC-9 requires snapshots only at parent completion, not per branch — a naive `event.type == "node_complete"` listener will produce too many files.
**Impact:** Medium — incorrect dump count breaks user mental model and the unit test for AC-10. Could also race on the step counter if not atomically incremented.

**Mitigation:**
- Confirm event taxonomy in `stategraph.py`: which exact event represents parent-node completion vs. branch completion. The story handwaves this ("equivalent emitted by stategraph.py"); the implementer must pin it down.
- Add an integration test with an explicit parallel fan-out of 3 branches converging at a fan-in node and assert the file list is `01-after-fanout.json`, `02-after-fanin.json` — not 5 files.
**Testing Focus:** Priority 2 — see test-design.

## Low / Minimal Risks (consolidated)

- **SEC-002 / DATA-003 (path / filename injection):** Node names are author-controlled (YAML), but if any flow allows runtime node naming or includes `/`, `..`, or NUL, the file write could escape the dump dir. Mitigation: sanitize the node-name component (replace non-alphanumeric/`_-` with `_`) before joining the path. Easy preventive control; add one unit test.
- **TECH-002 (sync I/O latency):** JSON serialization + write per node is microseconds for small states, but multi-MB states could add 10-100ms. Acceptable for a debug feature; document, do not optimize.
- **TECH-003 (FAILED write fails):** Wrap the failure-write in its own try/except so the original exception is the one that surfaces. Add a test that simulates write failure (e.g., read-only dir) and verifies the original node exception is what propagates.
- **DATA-002 (stale files):** AC-5 explicitly defers cleanup to the user. Risk is real but documented. Add a doc note recommending `rm -rf <dir>/*` in helper aliases.
- **OPS-002 (UX confusion):** `--output` semantics are unchanged (final-only). When a run fails, `--output` is empty/missing and the dumps tell the truth. Document this clearly to avoid bug reports.

## Risk Distribution

### By Category

- Security: 2 risks (0 critical, 1 high)
- Performance: 0 risks
- Data: 3 risks (0 critical, 0 high)
- Business: 0 risks
- Operational: 2 risks (0 critical, 0 high)
- Technical: 3 risks (0 critical, 0 high)

### By Component

- CLI (`python/src/the_edge_agent/cli.py`): 8 risks (event loop integration, flag handling, redaction call site)
- StateGraph event taxonomy (`stategraph.py`): 1 risk (TECH-001)
- Filesystem I/O: 4 risks (path/perm/disk concerns)

## Risk-Based Testing Strategy

### Priority 1: High-Risk Tests (security)

P1 tests **must pass** before merge.

1. **SEC-001a — redaction reuse:** Construct a state with every secret marker covered by the existing checkpoint redaction (mirror TEA-BUILTIN-012.3 fixtures). Run `--debug-state`, assert no marker appears in any dump file.
2. **SEC-001b — traceback redaction:** Define a node that raises `RuntimeError("token=sk-CANARY-XYZ")`. Run with `--debug-state`. Assert the FAILED dump's traceback string does not contain the canary (or, if redaction at traceback level is out of scope, document this explicitly in AC-12 as a known gap).
3. **SEC-001c — nested-dict redaction:** State contains `{"config": {"api_key": "..."}}`. Assert key is redacted in dumps.
4. **SEC-002 — path traversal:** Node named `../../etc/passwd` should produce a sanitized filename inside the target dir, not escape it.

### Priority 2: Medium-Risk Tests

5. **TECH-001a — parallel fan-out:** 3-branch parallel + fan-in. Assert exactly `01-after-fanout.json` and `02-after-fanin.json` (not 5 files).
6. **TECH-001b — dynamic_parallel:** Same shape with `dynamic_parallel`. Assert AC-9 holds.
7. **OPS-001 — production warning:** With `--debug-state` set, verify a WARN line is emitted on stderr regardless of `--quiet`.

### Priority 3: Standard Functional Tests

8. **AC-10:** 3-node sequential workflow → 3 files in order with correct names.
9. **AC-11:** workflow that raises in node 2 → exactly `01-after-node1.json` + `02-FAILED-node2.json`, latter contains traceback string.
10. **AC-5:** dir doesn't exist → created. Pre-existing files in dir → not deleted.
11. **AC-6:** flag absent → no files written, byte-identical behavior to current `tea run`.
12. **AC-7/8 compatibility:** matrix of `{--debug-state} × {--quiet, --stream, --show-graph, --checkpoint}` smoke-tests, asserting no crash and dumps are produced.
13. **TECH-003:** read-only dir → original node exception surfaces, not the FAILED-write failure.

## Risk Acceptance Criteria

### Must Fix Before Merge

- All Priority 1 (SEC) tests passing.
- TECH-001 ordering verified (Priority 2 tests 5 & 6).
- AC-12 doc warning explicit about traceback redaction scope.

### Can Deploy with Mitigation

- DATA-001 (disk fill) — accepted for v1 with doc warning. Track follow-up.
- OPS-001 — mitigated by warning log + doc.

### Accepted Risks

- DATA-002 (stale files) — explicitly accepted in AC-5; user-driven cleanup.
- OPS-002 (UX confusion between `--output` and dumps) — mitigated by documentation.

## Monitoring / Operational Requirements

This is a CLI dev-tool feature with no production telemetry surface. No monitoring required. Recommend:
- A single startup WARN line when flag is active.
- Documentation paragraph in troubleshooting guide explaining when to use vs. `--checkpoint`.

## Risk Review Triggers

Re-run risk profile when:
- The checkpoint redaction implementation (TEA-BUILTIN-012.3 codepath) changes.
- The StateGraph event taxonomy in `stategraph.py` changes (could break TECH-001 mitigation).
- A user reports a leaked secret in dumps (SEC-001 materialized).
- Parallel/dynamic_parallel semantics evolve.

## Gate Mapping

- Highest risk score = 6 (SEC-001)
- Per gate algorithm: any score ≥ 6 → **Gate = CONCERNS**
- Gate would upgrade to PASS if SEC-001 is reduced (Priority 1 tests in place + traceback redaction confirmed) — Probability drops to Low (1), Impact stays High (3), score 3 (Low).

## risk_summary (paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 1
    medium: 3
    low: 6
  highest:
    id: SEC-001
    score: 6
    title: 'Secret/PII leakage via debug dump'
  recommendations:
    must_fix:
      - 'Add P1 redaction tests covering nested dicts and traceback strings (SEC-001)'
      - 'Sanitize node-name path component to prevent traversal (SEC-002)'
      - 'Confirm parent-vs-branch event semantics for parallel/dynamic_parallel (TECH-001) and add fan-out/fan-in tests'
    monitor:
      - 'Add startup WARN line when --debug-state is set, surviving --quiet'
      - 'Document v1 has no disk-size cap; track follow-up for --debug-state-max-bytes'
```

## Story Hook Line

Risk profile: docs/qa/assessments/TEA-DX-001.3-risk-20260501.md
