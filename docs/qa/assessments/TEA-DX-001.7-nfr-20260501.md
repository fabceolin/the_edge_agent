# NFR Assessment: TEA-DX-001.7

Date: 2026-05-01
Reviewer: Quinn
Mode: YOLO (non-interactive; default core four NFRs)
Story: `docs/stories/TEA-DX-001.7-quiet-mode-heartbeat.md`

## Summary

- Security: PASS — Opt-in additive flag; no new auth/input/secret surface
- Performance: PASS — Trivially negligible per-node overhead (dict + format + one stderr write)
- Reliability: CONCERNS — TECH-1 (no `node_start` event in CLI run loop) unresolved; failure-path duration not validated by ACs (TECH-3)
- Maintainability: CONCERNS — Required tests cover success path only; duration formatter, parallel-branch, and `--quiet` default-off regression are not promoted to ACs

## Quality Score: **80** (100 − 10 × 2 CONCERNS)

## Gate YAML Block

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'Opt-in flag, default off; no new auth/input surface; node-name exposure parallels existing --stream behavior (SEC-1 minimal).'
  performance:
    status: PASS
    notes: 'Per-node overhead is dict timestamp + duration format + single stderr write; no explicit target needed (PERF-1 minimal).'
  reliability:
    status: CONCERNS
    notes: 'TECH-1 unresolved — Technical Notes assume node_start/node_complete events that do not exist in current CLI loop (cli.py:2236+). Failure-path duration (AC-6) is not explicitly tested for accuracy when error path bypasses timing.'
  maintainability:
    status: CONCERNS
    notes: 'AC-10/AC-11 cover only 3-node success path + stderr stream isolation. Duration formatter unit tests, parallel-branch attribution test (AC-9), and --quiet default-off regression test should be promoted from QA recommendations into AC-level requirements.'
```

## Critical Issues

1. **TECH-1: Missing `node_start` event** (Reliability)
   - Risk: Implementation cannot follow Technical Notes verbatim; duration is unknowable between explicit start/finish events.
   - Fix: Resolve before Task 2 — either compute wall-clock delta between consecutive `state` events (lower risk, additive) or add `node_start` emission in the engine (cleaner, broader scope). Add a clarification note to the story.

2. **Test coverage gap: failure-path duration accuracy** (Reliability/Maintainability)
   - Risk: AC-6 says `FAILED in <duration>` is emitted, but no AC asserts the duration value is non-zero/sensible when the error path bypasses normal completion timing.
   - Fix: Add explicit assertion in the failure-path test that the emitted duration is > 0 and bounded by wall-clock test runtime.

3. **Test coverage gap: `--quiet` silence regression** (Maintainability)
   - Risk: Without an explicit no-heartbeat regression test, a future refactor could leak heartbeat lines when the flag is off (COMPAT-2).
   - Fix: Promote risk-profile test #3 to a P0 AC: "with `--quiet` and without `--heartbeat`, stderr contains zero heartbeat lines on a successful run."

## Quick Wins

- Add wall-clock delta clarification to Technical Notes: ~15 min
- Add duration formatter unit tests (`1.2s`, `45.3s`, `2m 18s`, sub-100ms, >1h boundaries): ~30 min
- Add `mix_stderr=False` guidance to Test Approach in Tasks: ~5 min
- Add parallel-branch attribution test (no ordering assertion): ~30 min

## Acceptance Criteria — Recommendations

Add (or promote from QA Notes):

- **AC-13:** Duration formatter unit tests cover `1.2s`, `45.3s`, `2m 18s`, sub-100ms, and >1h cases.
- **AC-14:** Regression test: `--quiet` alone (no `--heartbeat`) produces zero heartbeat lines on stderr for a successful 3-node run.
- **AC-15:** Failure-path test asserts the emitted duration is > 0 and ≤ test wall-clock bound.
- **AC-16:** Parallel-branch test asserts one heartbeat per branch with branch prefix; ordering across branches is NOT asserted.

## Test Recommendations (P0 → P3)

**P0 (must have before merge):**
- Heartbeat emission on 3-node sequential workflow (AC-2, AC-10)
- Stream isolation: stdout unchanged with `mix_stderr=False` (AC-4, AC-11)
- Default-off regression: zero heartbeat on `--quiet` alone (AC-5, COMPAT-2)
- Failure path: `FAILED in <duration>` emitted with duration > 0 (AC-6)

**P1 (should have):**
- Parallel branch attribution + non-deterministic ordering (AC-9, TECH-2)
- `--quiet --heartbeat` emits ONLY heartbeat lines (AC-3)
- Duration formatter unit tests at boundary cases (AC-2)

**P2 (nice to have):**
- Combined-flag matrix: `--heartbeat` + `--stream`, + `--debug-state` (AC-7, AC-8)
- Exit code preservation across heartbeat success/failure (Tech Note constraint)

**P3 (manual):**
- CI smoke run on a long workflow to confirm CI does not flag stderr lines as errors (OPS-1)

---

NFR assessment: docs/qa/assessments/TEA-DX-001.7-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.7-quiet-mode-heartbeat.yml under nfr_validation
