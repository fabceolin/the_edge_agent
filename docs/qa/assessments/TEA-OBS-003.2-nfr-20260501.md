# NFR Assessment: TEA-OBS-003.2

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.2 — Payload retention policy & cleanup
Mode: YOLO (non-interactive; default core four NFRs)

## Scope Assessed

- Security
- Performance
- Reliability
- Maintainability

(Usability, Compatibility, Portability, Functional Suitability not selected.)

## Summary

| NFR | Status | One-liner |
|---|---|---|
| Security | **CONCERNS** | Story is a security improvement (TTL+cleanup) but symlink-following is not gated by an AC; warning is a soft control with documented residual risk; pattern is TEA-specific by default. |
| Performance | **PASS** | Cleanup runs offline (cron/systemd); single-directory glob+stat per file is O(n) and irrelevant relative to disk I/O budget for daily/nightly runs at expected file counts. |
| Reliability | **CONCERNS** | Exit codes & per-file isolation are well-specified (AC-12/13), but silent-cron-failure mitigation (no `2>&1 >/dev/null` in docs example, summary line on zero matches) lives only in risk-profile recommendations, not ACs. |
| Maintainability | **PASS** | Tests cover all AC paths (AC-16/17), `tmp_path`+`os.utime` fixture pattern, single-file CLI surface, additive design — fully reversible if backed out. |

**Quality Score: 80/100** (100 − 2 × CONCERNS × 10)

**Recommended gate: CONCERNS** — converges to PASS once symlink-safety is codified as AC-18 (per risk profile recommendation), the documented cron example is verified to not redirect stderr, and the cleanup command is required to print its summary line even when zero files match.

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Story IS the security improvement for TEA-OBS-003.1 (retention TTL + cleanup command), but two gaps remain: (1) symlink-follow safety is in risk profile mitigation #2 as recommended AC-18 — not yet codified in ACs; in --recursive mode this is the difference between safe and unsafe; (2) AC-2 warning content needs explicit substring assertions (PII, trace_payload_retention_days, tea trace cleanup) so log search works in production. Default pattern *.llm.jsonl is TEA-specific (good). --dry-run safety net is required (AC-9, good).'
  performance:
    status: PASS
    notes: 'Cleanup is offline (cron/systemd, not in request path). Single-directory glob+stat is O(n_files); n is bounded by retention window × runs/day (~30,000 files for 1k runs/day × 30 days), well within seconds. PERF-001 in risk profile is Minimal. No latency target needed; non-recursive default avoids accidentally walking unrelated trees.'
  reliability:
    status: CONCERNS
    notes: 'Exit codes specified (AC-12 success / AC-13 partial-failure with continue-on-error). Per-file failures isolated and aggregated — good. But cron silent-failure prevention (do not redirect stderr in doc example) and summary-on-zero-matches are in risk profile recommendations, not ACs. Without these, cron jobs can fail silently for weeks. Suggest promoting to AC-15 sub-bullets (docs) and AC-10 amendment (always print summary).'
  maintainability:
    status: PASS
    notes: 'Tests required for all AC paths (AC-16: warning emit/suppress, --older-than, --dry-run, --pattern, --recursive, permission errors). Fixture pattern is standard (tmp_path + os.utime, AC-17). Additive design (no migration). Single Typer subcommand surface is small. Doc requirement is enforced by AC-15. Clear Definition of Done.'
```

## Detailed Findings

### Security — CONCERNS

**Evidence reviewed**
- AC-1/AC-4: Retention setting is policy-only at engine init; deletion is a separate explicit command. Good — no surprise data loss at engine startup.
- AC-2: Warning text is specific and names PII risk. Soft control acknowledged.
- AC-5/AC-8: Cleanup defaults to TEA-specific glob `*.llm.jsonl` and `*.llm.jsonl.gz` (forward-compat with 003.3).
- AC-9: `--dry-run` is the safety net.
- AC-11: Non-recursive default — avoids accidentally walking parent trees.
- Risk profile: SEC-001 (symlink-follow) Low score (1×3=3) but **High impact**, mitigation flagged as "must-fix, codify as AC-18".
- Risk profile: BUS-001 (PII retention compliance) and OPS-001 (warning fatigue) both High (score 6) — both inherent to soft-control design.

**Why CONCERNS, not PASS**
- **Symlink-safety not in ACs.** The risk profile recommends adding AC-18 ("cleanup must not follow symlinks; in `--recursive` mode, refuse to descend into symlinked directories; before `unlink()`, skip symlinks"). This is currently a recommendation, not a requirement. Without it, an attacker who can create a symlink in the trace directory (e.g., a misconfigured shared volume) can use `--recursive` cleanup to delete files outside the intended scope. P1 test #3 in risk profile already specifies the test, but the AC must exist for it to gate.
- **Warning content not asserted.** AC-2 quotes the warning text in the story body, but does not require any test that the runtime warning record actually contains the substrings `PII`, `trace_payload_retention_days`, and `tea trace cleanup`. Without explicit substring tests, the warning text can drift in implementation and operators won't be able to grep production logs for it. Risk profile P1 test #1 covers this; promote to AC.
- **Soft controls remain soft.** BUS-001 and OPS-001 cannot be technically eliminated by this story; they are accepted residual. This is correct design but caps the security score.

**Why not FAIL**
- This story IS the security mitigation for the retention gap left by TEA-OBS-003.1. Refusing to ship it because it's incomplete would extend the existing exposure.
- Default pattern is TEA-specific (`*.llm.jsonl`, `*.llm.jsonl.gz`). User-owned files unrelated to TEA capture won't match by default.
- `--dry-run` is documented and recommended in the risk profile mitigation as a required first-run discipline.
- Non-recursive default closes the worst symlink-walk vector for the typical user.

**Missing considerations / recommendations**
1. **Codify AC-18: symlink safety.** Verbatim from risk profile mitigation #2: "cleanup must not follow symlinks; in `--recursive` mode, refuse to descend into symlinked directories; before `unlink()`, skip symlinks (do not delete the link's target)". Without this AC, the test (P1 #3) is not formally required.
2. **Codify AC-19: warning substring assertion.** Test must assert the runtime warning record contains literal substrings `PII`, `trace_payload_retention_days`, and `tea trace cleanup`. This is what makes the warning operationally findable.
3. **Audit log for cleanup runs.** Consider writing a one-line summary to a separate audit log (`<trace_dir>/.tea-cleanup.log`): timestamp, files deleted, bytes freed. Not in current ACs. Useful for compliance audits asking "when did you last enforce retention?".
4. **Refuse `--older-than 0`** (risk profile mitigation #6) — prevents racing with active writers and accidentally deleting today's files. Should be added as AC.
5. **`--pattern` widening warning.** When user passes a non-default pattern (`*` or `*.json`), emit a warning that they are bypassing the TEA-specific safety filter. Defense in depth against DATA-001.

### Performance — PASS

**Evidence reviewed**
- AC-5: Cleanup is a CLI subcommand. Not in request path.
- AC-15: Cron and systemd timer examples. Designed for nightly/scheduled runs.
- AC-11: Non-recursive default; user must opt into `--recursive`.
- Risk profile PERF-001: "Slow cleanup on very large directories" — Minimal score (1×1=1).

**Why PASS**
- Workload sizing from story context: 1k runs/day × 150-300 KB = ~200 MB/day. At 30-day retention this is ~30k files. `glob` + `os.stat` per file completes in well under a second on any modern filesystem. Even at 100k files (large deployment, 100-day retention), expect <10s — irrelevant for a nightly cron.
- Non-recursive default prevents accidental walks of parent directories that could explode file counts.
- No memory pressure: streaming pathlib iteration, not a list-comprehension materialization.
- No I/O contention concerns: cleanup runs at off-peak hours by design (cron `0 2 * * *` example in AC-15).

**Why not CONCERNS**
- No latency target is needed because the operation is not latency-sensitive. "Target unknown" rule does not apply here — there is no latency budget being defended.
- The dry-run path is read-only stat work and equally cheap.

**Missing considerations / recommendations**
1. **Document expected runtime in observability.md** (one line): "Expected runtime: <1s per 10k files on local disk; networked filesystems may be 10-100x slower". Operators sizing cron windows benefit from this.
2. **Optional `--limit N`** (future, not blocking) for very large directories where the operator wants bounded-time runs (e.g., for SLA-critical cron windows). Defer to a future story.
3. **PERF-001 future mitigation: directory sharding by date.** Risk profile already lists this as "Monitor (future stories)". Not in scope.

### Reliability — CONCERNS

**Evidence reviewed**
- AC-12: Exit 0 on success including zero matches.
- AC-13: Exit 1 on any deletion error; continue processing other files; report failures at end.
- AC-9: `--dry-run` lists files without deletion (read-only safety).
- AC-7: Errors clearly when `--older-than` not specified anywhere.
- AC-15: Cron + systemd examples for scheduled execution.
- Risk profile OPS-002 (Medium, score 4): "Cron/systemd cleanup fails silently".
- Risk profile mitigations #4 ("Cron example MUST NOT redirect stderr to /dev/null") and #5 ("Cleanup must print summary line even when no files matched") — both currently in risk profile, NOT codified in ACs.

**Why CONCERNS, not PASS**
- **Silent cron failure is the dominant operational risk.** OPS-002 in risk profile is rated Medium specifically because once configured in cron and forgotten, a broken cleanup can run zero-effective for weeks before anyone notices PII accumulation. The two mitigations in the risk profile (no stderr redirect; always-print summary) are exactly the things cron uses to detect failure (mailing stderr; cron's success-vs-empty-output discipline). Both currently live as "should fix" recommendations, not requirements.
- **AC-10 says "Summary printed to stdout"** but does not specify behavior when zero files match. Strict reading: zero files → no summary line → cron has nothing to email → operator never sees "still working". This needs an AC clarification.
- **AC-15 cron example** is currently a story bullet, not yet written. There is risk that the implementer writes it with a `2>&1 >/dev/null` redirect (very common in cron tutorials) which destroys the failure-detection mechanism.
- **`--older-than 0` race** (risk profile OPS-003 / mitigation #6): not refused. A user setting this in a cron alongside an active capture writer can race the writer.

**Why not FAIL**
- Per-file error isolation (AC-13) is correctly specified — one bad file does not abort the batch.
- Exit code discipline (AC-12/13) is unambiguous.
- `--dry-run` is the universally-recommended pre-flight check.
- AC-7 explicit error on missing `--older-than` prevents accidental "delete everything" runs.

**Missing considerations / recommendations**
1. **Promote risk profile mitigation #4 to AC-15 sub-bullet:** "Cron example MUST NOT redirect stderr to /dev/null. Documentation must explicitly call out that cron emails stderr to root as the failure-detection mechanism."
2. **Amend AC-10:** "Summary line is printed even when zero files match (e.g., `Deleted 0 files, freed 0 MB`). This ensures cron has output for success-detection."
3. **Add AC: refuse `--older-than 0`** with a clear error message. Optionally also `--older-than` < 1 (negative or fractional).
4. **Lockfile or in-progress detection** for the writer (003.1 active capture) — not blocking, but consider documenting that cleanup should run during a quiescent window.
5. **AC-13 detail:** specify the failure summary format. Suggest: stderr `ERROR: failed to delete <path>: <reason>` per file, then final stderr line `Failed to delete N of M files. Exit 1.` Currently AC-13 says "report failures at end" without format.

### Maintainability — PASS

**Evidence reviewed**
- AC-16: Tests cover all AC paths (warning emit/suppress, `--older-than`, `--dry-run`, `--pattern`, `--recursive`, permission errors).
- AC-17: Fixture uses `tmp_path` + `os.utime` to control mtimes — standard pytest pattern, fully isolated.
- AC-15: Documentation enforced by AC (cron, systemd, TTL guidance).
- Story Tasks 1-5 break the work into well-scoped chunks (settings, cleanup, exit codes, tests, docs).
- Code surface: 1 Typer subcommand + 1 settings parse + 1 docs section. Small.
- Compatibility: "Fully additive. Engineers not using capture see nothing." (Risk and Compatibility section.)

**Why PASS**
- Test coverage is enforced by AC, not aspirational. All AC paths are required to have tests.
- Fixture pattern is standard and reproducible — `tmp_path` is already used throughout the test suite.
- The CLI surface is small and pinned by `tea trace cleanup --help` example (AC-14).
- Rollback is trivial (revert PR; existing capture continues to work).
- No state migration; no new dependencies.

**Missing considerations / recommendations**
1. **Pin the warning text.** AC-2 quotes it; suggest adding a constant in `yaml_engine.py` (`PII_RETENTION_WARNING = "..."`) so the test can `from ... import PII_RETENTION_WARNING` and assert. Otherwise the test asserts a substring, which is fine but loosely coupled.
2. **Pin the summary format.** AC-10 says `Deleted N files, freed X MB` — recommend exact format string in code (`SUMMARY_FORMAT = "Deleted {n} files, freed {mb:.2f} MB"`) to avoid drift between code and AC.
3. **`tea trace cleanup --help` snapshot test** (in addition to AC-14 substring check) — guards against accidental help text regression. Use `pytest`'s text snapshot or `assert "tea trace cleanup ./traces --older-than 30 --dry-run" in result.stdout`.
4. **TypedDict for cleanup result** in code — `CleanupResult(deleted: int, failed: int, bytes_freed: int, errors: list[tuple[Path, str]])`. Improves test assertion clarity over inspecting captured stdout.
5. **Test for forward-compat `.gz` pattern** (AC-8) — ensures TEA-OBS-003.3 is not accidentally broken when it lands.

## Critical Issues

1. **Security: symlink-follow not gated by AC** (SEC-001 from risk profile)
   - Risk: `tea trace cleanup --recursive` on a directory containing a symlink to `/etc/` could delete files outside the trace directory. P1 test exists in risk profile but is not gated by an AC.
   - Fix: Codify as AC-18 verbatim from risk profile mitigation #2. Implementation: use `Path.is_symlink()` check before `unlink()`; in `--recursive`, check `is_symlink()` on each subdirectory before descending.

2. **Reliability: silent cron failure** (OPS-002 from risk profile)
   - Risk: Cron job runs but cleanup is broken (permissions, missing `--older-than`, stale config) — cron stays silent for weeks because the doc example redirects stderr or cleanup prints nothing on zero matches.
   - Fix: (a) AC-15 sub-bullet: "Cron example MUST NOT redirect stderr". (b) Amend AC-10: summary printed even on zero matches. Both already in risk profile mitigations #4 and #5; promote to ACs.

3. **Security: warning operability** (BUS-001 / OPS-001 from risk profile)
   - Risk: Operator needs to grep production logs for the warning to find capture-enabled-without-retention deployments; without test-pinned substrings the warning text can drift.
   - Fix: AC-19 — test asserts warning record contains substrings `PII`, `trace_payload_retention_days`, `tea trace cleanup`. Pin warning text as a constant.

## Quick Wins

- Codify symlink safety as AC-18 — ~10 minutes (story update); implementation ~30 minutes.
- Amend AC-10 ("summary line printed even when zero files match") — ~5 minutes (story update); implementation trivial.
- Add AC-15 sub-bullet ("cron example must not redirect stderr") — ~5 minutes.
- Add `--older-than 0` rejection — ~15 minutes.
- Pin `PII_RETENTION_WARNING` constant + import in test — ~15 minutes.
- `--pattern` non-default warning — ~15 minutes.

## Test Recommendations (priority-ordered)

| # | Priority | Test | NFR Coverage |
|---|---|---|---|
| 1 | P1 | Warning emitted exactly once when capture on + retention unset; record contains `PII`, `trace_payload_retention_days`, `tea trace cleanup` | Security, Maintainability |
| 2 | P1 | Warning suppressed when `trace_payload_retention_days` set to positive integer | Security |
| 3 | P1 | Symlink to file outside trace dir → target untouched (skip on Windows) | Security |
| 4 | P1 | Symlinked subdirectory + `--recursive` → not descended (skip on Windows) | Security |
| 5 | P1 | `--dry-run` deletes nothing — byte-identical filesystem snapshot before/after | Security, Reliability |
| 6 | P1 | Permission error on one file does not abort batch; exit 1; per-file error on stderr; summary still printed | Reliability |
| 7 | P2 | `--older-than` mtime selection: 10/30/100-day fixtures via `os.utime`; `--older-than 30` deletes only 100-day file (boundary inclusive/exclusive specified) | Functional + Reliability |
| 8 | P2 | `--pattern` default matches `*.llm.jsonl` and `*.llm.jsonl.gz`; ignores `.json`, `.txt` | Security (scope discipline) |
| 9 | P2 | `--recursive` on nested directory tree | Functional |
| 10 | P2 | Exit 0 on zero matches; summary line printed (`Deleted 0 files, freed 0 MB`) | Reliability (cron failure detection) |
| 11 | P2 | Exit 1 on any deletion error | Reliability |
| 12 | P2 | `tea trace cleanup --help` contains literal example from AC-14 | Maintainability |
| 13 | P2 | `--older-than 0` rejected with clear error | Reliability (race prevention) |
| 14 | P3 | Cron example in `docs/python/observability.md` does NOT contain `2>&1 >/dev/null` (regex/grep assertion in a doc test) | Reliability (cron failure detection) |
| 15 | P3 | TTL guidance paragraph leads with "30 days for general use; 7 days for PII-heavy domains" (substring match) | Documentation completeness |
| 16 | P3 | Warning text from AC-2 reproduced verbatim in observability.md | Documentation parity |
| 17 | P3 | Forward-compat `.gz` pattern matches a `.llm.jsonl.gz` fixture (TEA-OBS-003.3 readiness) | Maintainability |

## Acceptance Criteria for Gate Pass

To move from **CONCERNS → PASS** at gate time:

1. **Security:**
   - AC-18 (symlink safety) codified in story and tests #3, #4 passing.
   - AC-19 (warning substring assertions) codified and test #1 passing.
   - `--pattern` non-default warning implemented (defense-in-depth, not gating).
2. **Performance:** Already PASS. Document expected runtime in observability.md as a one-line note.
3. **Reliability:**
   - AC-10 amended to require summary line on zero matches; test #10 passing.
   - AC-15 sub-bullet added forbidding `2>&1 >/dev/null` in cron example; doc test #14 passing.
   - `--older-than 0` rejection AC added; test #13 passing.
4. **Maintainability:** Already PASS. `PII_RETENTION_WARNING` and summary format pinned as code constants for test stability.

## Notes

- This NFR assessment complements the existing risk profile (`TEA-OBS-003.2-risk-20260501.md`). The three "Critical Issues" above all map to "Must Fix" mitigations in that risk profile — they are listed here to be NFR-gating, not duplicating.
- Story is in **Draft** status; no implementation yet. NFR conclusions are forward-looking and gated on implementation matching ACs.
- TEA-OBS-003.2 is the security follow-up to TEA-OBS-003.1 — the latter's NFR assessment ended with "Security CONCERNS until 003.2 lands". Closing that loop is the load-bearing reason this story exists.
- No test design or trace assessment files exist yet for TEA-OBS-003.2 (only risk profile). Recommend running `*test-design` and `*trace` before development begins.

---

NFR assessment: docs/qa/assessments/TEA-OBS-003.2-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-OBS-003.2-payload-retention-cleanup.yml under nfr_validation
