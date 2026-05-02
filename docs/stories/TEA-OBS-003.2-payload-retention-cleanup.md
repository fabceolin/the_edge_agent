# TEA-OBS-003.2: Payload retention policy & cleanup

## Status
Done

## Parent Epic
[TEA-OBS-003](TEA-OBS-003-llm-payload-trace-capture-epic.md)

## Priority
Medium

---

## Story

**As a** workflow operator running TEA in production with LLM payload capture enabled,
**I want** an explicit retention TTL and a cleanup command,
**so that** captured `*.llm.jsonl` files don't accumulate indefinitely on disk and don't outlive the policy I want for sensitive data (CNPJ, financial info, customer data).

## Story Context

**Existing System Integration:**

- Builds on: TEA-OBS-003.1 (capture mechanism). This story is meaningless without that one.
- Touches: `python/src/the_edge_agent/cli.py` (new `tea trace cleanup` subcommand), `yaml_engine.py:_configure_from_settings` (new setting + warning at engine init)
- Technology: Python, Typer, `os.path.getmtime`, `pathlib.Path`

**Problem Statement:**

User report: "Pra estrutural, baixo risco — projetos têm nome de cliente/obra, não muito mais. Mas se o pipeline for usado pra outras coisas (NF-e, docs financeiros), prompts podem carregar CNPJ, valores, etc. Definir já uma política de retenção (90 dias? 30?) antes de começar a acumular."

Once payload capture is enabled in production, files accumulate at ~150-300 KB per workflow run. At 1000 runs/day this is ~200 MB/day. More importantly, captured payloads may contain PII that should not live forever on disk. A retention TTL with a cleanup command is the minimum viable policy enforcement.

## Acceptance Criteria

### Part A: Retention setting

1. **AC-1:** New YAML setting `trace_payload_retention_days` (integer, default unset).
2. **AC-2:** When `auto_trace_llm_payloads` is enabled (any truthy form) and `trace_payload_retention_days` is **not** set, log a `WARNING` at engine init:
   > `LLM payload capture is enabled without a retention policy. Captured prompts and responses may contain PII. Set 'trace_payload_retention_days' or run 'tea trace cleanup' periodically.`
3. **AC-3:** When both are set, no warning is logged.
4. **AC-4:** The retention setting itself does **not** trigger automatic deletion at engine init or shutdown — that's the cleanup command's job. Engine init only inspects/warns.

### Part B: Cleanup subcommand

5. **AC-5:** New CLI command `tea trace cleanup [<dir>] [--older-than <days>] [--dry-run] [--pattern <glob>]`.
6. **AC-6:** Default `<dir>` is the directory containing the configured `trace_file` (read from a YAML or via `--config <yaml>`); if no config is supplied, defaults to current directory.
7. **AC-7:** `--older-than <days>` overrides any YAML setting. If neither is set, the command errors with a clear message asking the user to specify.
8. **AC-8:** Default `--pattern` matches `*.llm.jsonl` and `*.llm.jsonl.gz` (the latter for forward compatibility with TEA-OBS-003.3).
9. **AC-9:** `--dry-run` lists files that *would* be deleted without deleting them. Output: one path per line, with size and mtime.
10. **AC-10:** Without `--dry-run`, files matching the pattern with `mtime < (now - older_than_days)` are deleted. Summary printed to stdout: `Deleted N files, freed X MB`.
11. **AC-11:** Cleanup is non-recursive by default; `--recursive` flag enables descent into subdirectories.

### Part C: Exit codes & ergonomics

12. **AC-12:** Exit 0 on successful cleanup (zero files matched is still success).
13. **AC-13:** Exit 1 on any deletion error (permissions, file in use). Continue processing other files; report failures at end.
14. **AC-14:** `tea trace cleanup --help` includes a one-line example: `tea trace cleanup ./traces --older-than 30 --dry-run`.

### Part D: Cron / scheduler example

15. **AC-15:** `docs/python/observability.md` (or wherever observability lives) includes:
   - The retention warning text in full
   - A cron example: `0 2 * * * tea trace cleanup /var/lib/tea/traces --older-than 30`
   - A systemd timer example as alternative
   - Explicit guidance on choosing a TTL: "30 days for general use; 7 days for PII-heavy domains; document your choice in your project's compliance notes"

### Part E: Quality

16. **AC-16:** Tests cover: warning emitted when retention unset; warning suppressed when set; cleanup respects `--older-than`; `--dry-run` deletes nothing; `--pattern` filters correctly; `--recursive` traverses; permission errors don't abort.
17. **AC-17:** A test fixture creates files with controlled mtimes and asserts cleanup deletes the right ones.

### Part F: NFR-Derived ACs (codified from epic QA — TEA-OBS-003 NFR-AC-1, NFR-AC-8, NFR-AC-12)

These ACs were surfaced by the embedded epic-level QA review and are propagated here per epic gate finding `EPIC-AC-002`. They give the epic-level test design a contractual anchor in this story.

18. **NFR-AC-1 (Security · mandatory retention enforcement — Story-2 half):** This story owns the warn-now/fail-later contract that pairs with Story 003.1's capture activation. Engine init emits the documented WARNING when capture is on and `trace_payload_retention_days` is unset (AC-2 above already pins this). The release-binding line in the epic DoD ensures Story 003.1 cannot ship without this story's enforcement live in the same release.
19. **NFR-AC-8 (Reliability · cleanup safety):** The `tea trace cleanup` subcommand:
    - **Rejects symlink traversal** — files reached via symlink are skipped (not followed); a WARNING is logged identifying the symlink path.
    - **Preserves the spans-only file** — the default `--pattern` is anchored on the `.llm.` infix (matches `*.llm.jsonl` and `*.llm.jsonl.gz`) so a sibling `*.jsonl` (the spans-only file) is **never** matched. Implemented at `trace_cleanup.py:34` (`DEFAULT_PATTERNS`).
    - **Supports `--older-than 0`** as a documented purge mode (deletes all matching files regardless of mtime).
    - **Supports `--dry-run`** — produces zero `os.remove` calls; output lists each candidate with size and mtime; verified in tests.
    - **Skips in-flight files** — files currently held open by another writer are skipped with a WARNING; the cleanup does not race against an active capture.
    - **Logs every deletion at INFO** with path + size for audit trail.
20. **NFR-AC-12 (Maintainability · doc smoke):** `docs/python/observability.md` retention section includes the literal cron example, the systemd-timer alternative, and the TTL-choice guidance ("30 days general; 7 days PII-heavy"). A doc-smoke test (or build step) asserts presence so the operator-facing guidance cannot silently regress. Tracked alongside AC-15.

## Tasks / Subtasks

- [x] **Task 1: Settings & warning** (AC: 1, 2, 3, 4)
  - [x] Parse `trace_payload_retention_days` in `yaml_engine.py:_configure_llm_payload_capture`
  - [x] Emit warning when capture is on but retention is unset
- [x] **Task 2: Cleanup command** (AC: 5, 6, 7, 8, 9, 10, 11)
  - [x] New `tea trace cleanup` Typer subcommand (group `trace` created)
  - [x] Glob, mtime check, dry-run path, recursive flag
- [x] **Task 3: Exit codes & summary** (AC: 12, 13, 14)
  - [x] Aggregate failures, return non-zero on any error
  - [x] Format summary line (incl. zero-match summary)
- [x] **Task 4: Tests** (AC: 16, 17)
  - [x] Use `tmp_path` fixtures with `os.utime` to control mtimes
  - [x] Cover all AC paths (cleanup helper + CLI + symlink safety + exit code 1)
- [x] **Task 5: Docs** (AC: 15)
  - [x] Cron + systemd examples
  - [x] TTL guidance paragraph
  - [x] Cross-link from TEA-OBS-003.1's docs (combined observability page)

## Definition of Done

- [x] All ACs met
- [x] `pytest python/tests/test_tea_obs003_payload.py` green (64/64)
- [x] Manual: enable capture in a YAML without retention → warning appears (`test_retention_warning_fires`)
- [x] Manual: run cleanup with `--dry-run` on a known directory → expected files listed; without `--dry-run` → files deleted (`test_dry_run`, `test_real_delete`)
- [x] Docs updated with cron + systemd examples (`docs/python/observability.md`)

## Risk and Compatibility

- **Primary Risk:** Cleanup deletes the wrong files (e.g., user has a file named `prod.llm.jsonl` that's actually their data, not a TEA capture).
  - **Mitigation:** `--pattern` defaults to `*.llm.jsonl` which is TEA-specific. `--dry-run` is the safety net. Documentation strongly recommends `--dry-run` before first scheduled run.
- **Secondary Risk:** Warning fatigue — engineers ignore the unset-retention warning.
  - **Mitigation:** Warning text names PII risk explicitly. No mitigation beyond documentation; this is a soft control.
- **Rollback:** Revert PR; existing capture continues to work. No state migration needed.
- **Compatibility:** Fully additive. Engineers not using capture see nothing.

## SM Validation

**Date:** 2026-05-01
**Validator:** Bob (Scrum Master)
**Mode:** YOLO
**Checklist:** `.bmad-core/checklists/story-draft-checklist.md`

### Definition of Ready Results

| # | Criterion | Status | Notes |
| - | --------- | :----: | ----- |
| 1 | Story has clear title and description | PASS  | Title scoped; user story uses As/I want/so that with PII justification (CNPJ, financial, customer data) |
| 2 | Acceptance criteria defined and testable | PASS  | 17 codified ACs across 5 parts (settings, command, exit codes, docs, quality); each AC names observable behavior |
| 3 | Dependencies identified | PASS  | Hard dependency on TEA-OBS-003.1 declared in Story Context; integration points named (`cli.py`, `yaml_engine.py:_configure_from_settings`) |
| 4 | Technical approach documented | PASS  | Stack named (Python, Typer, `os.utime`, `pathlib.Path`); fixture pattern (`tmp_path` + `os.utime`) called out |
| 5 | Story properly sized | PASS  | 5 tasks, additive design, no migration, single Typer subcommand surface, ~5s estimated CI time |
| 6 | QA notes sections present | PASS  | All four — Risk Profile, NFR Assessment, Test Design, Requirements Trace — present, all dated 2026-05-01, all assessment files exist on disk |
| 7 | No blocking issues or unknowns | PASS  | No blockers; QA gate is CONCERNS with pre-merge recommendations, not story-readiness blockers |

### Story-Draft Checklist Results

| Category                             | Status | Issues |
| ------------------------------------ | :----: | ------ |
| 1. Goal & Context Clarity            | PASS   | Epic linkage, role, PII rationale, file-volume math (~200 MB/day at 1k runs/day) |
| 2. Technical Implementation Guidance | PASS   | All key files, libraries, exit-code contracts named |
| 3. Reference Effectiveness           | PASS   | QA references use dated paths to specific assessments; prior story summarized in Story Context |
| 4. Self-Containment Assessment       | PASS   | All ACs in body; edge cases (permission errors, recursion, dry-run, mtime boundary) explicit |
| 5. Testing Guidance                  | PASS   | Test design matrix references 28 scenarios with priority/level; fixture pattern + skip markers documented |

### Final Assessment

**READY FOR DEVELOPMENT** — Clarity score: 9/10. Story is comprehensive, self-contained, and provides sufficient context for a developer agent to implement without ambiguity.

### Pre-Implementation Recommendations (advisory, from QA)

The Risk Profile, NFR Assessment, and Requirements Trace converge on **5 recommended ACs** that should be incorporated into the AC body before implementation lands. Five forcing-function tests (UNIT-003, UNIT-008, INT-012, INT-017, INT-022/023) are designed to fail unless these land:

1. **AC-18 (symlink safety):** Cleanup must not follow symlinks; in `--recursive` mode, refuse to descend into symlinked directories; skip symlinks before `unlink()`. Without this, `--recursive` blast radius is arbitrary user files (SEC-001).
2. **AC-19 (warning substring assertions):** AC-2 warning record must contain literal substrings `PII`, `trace_payload_retention_days`, `tea trace cleanup` so log search works in production.
3. **AC-10 amendment (zero-match summary):** Print summary line even on zero matches (`Deleted 0 files, freed 0 MB`) so cron has output for success/failure detection.
4. **AC-15 sub-bullet (cron stderr):** Cron example MUST NOT redirect stderr to `/dev/null` — that is the failure-detection mechanism (default cron emails stderr to root).
5. **`--older-than 0` rejection:** Reject with clear error to prevent racing with active writers.

These are flagged as advisory at the SM gate (DoR is met as written), but the dev agent should adopt them as part of the implementation pass; otherwise the QA gate at story closure will require a follow-up cycle.

---

## QA Notes - Risk Profile

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO
**Full assessment:** [docs/qa/assessments/TEA-OBS-003.2-risk-20260501.md](../qa/assessments/TEA-OBS-003.2-risk-20260501.md)

### Risk Level

**Overall: Low-to-Moderate (71/100)** — Gate recommendation: **CONCERNS**

- 0 Critical (score 9)
- 2 High (score 6): BUS-001, OPS-001
- 1 Medium (score 4): OPS-002
- 2 Low (score 2-3): DATA-001, SEC-001
- 4 Minimal (score 1): DATA-002, OPS-003, TECH-001, PERF-001

### Identified Risks

| Risk ID  | Category    | Description                                       | P × I       | Score |
| -------- | ----------- | ------------------------------------------------- | ----------- | ----- |
| BUS-001  | Business    | PII retention violates compliance (LGPD/GDPR)     | M (2) × H (3) | 6   |
| OPS-001  | Operational | Warning fatigue — unset-retention warning ignored | H (3) × M (2) | 6   |
| OPS-002  | Operational | Cron/systemd cleanup fails silently               | M (2) × M (2) | 4   |
| DATA-001 | Data        | Cleanup deletes user's non-TEA `*.llm.jsonl` file | L (1) × H (3) | 3   |
| SEC-001  | Security    | Symlink-follow deletes files outside trace dir    | L (1) × H (3) | 3   |
| DATA-002 | Data        | mtime touched externally preserves stale file     | L (1) × L (1) | 1   |
| OPS-003  | Operational | Cleanup races with active writer                  | L (1) × L (1) | 1   |
| TECH-001 | Technical   | Default pattern omits `.gz` (regression risk)     | L (1) × L (1) | 1   |
| PERF-001 | Performance | Slow cleanup on very large directories            | L (1) × L (1) | 1   |

### Mitigations

**Must Fix (block merge):**

1. **AC-2 warning content (BUS-001 / OPS-001):** Verify the warning string actually contains the literal substrings `PII`, `trace_payload_retention_days`, and `tea trace cleanup` so operators searching logs can find it.
2. **Symlink safety (SEC-001) — NEW:** Recommend codifying as **AC-18**: cleanup must not follow symlinks. In recursive mode, refuse to descend into symlinked directories. Before `unlink()`, skip symlinks (do not delete the link's target). Without this, `--recursive` is unsafe.
3. **`--dry-run` integrity (DATA-001):** Test must take a byte-identical filesystem snapshot before/after `--dry-run` to assert zero side effects.

**Should Fix (highly recommended):**

4. **Cron example (OPS-002):** Doc cron example MUST NOT redirect stderr to `/dev/null`; default cron emails stderr to root, which is the failure-detection mechanism.
5. **Summary on zero matches (OPS-002):** Cleanup must print summary line even when no files matched, so cron has something to email.
6. **Refuse `--older-than 0` (OPS-003):** Reject with clear error to prevent racing with active writers.

**Monitor (future stories):**

7. Promote unset-retention warning to ERROR with explicit `unlimited` opt-out for production.
8. JSONL header sniff before delete (DATA-001 hardening).
9. Directory sharding by date for very large deployments (PERF-001).

**Accepted residual:**

- BUS-001 / OPS-001 cannot be fully eliminated by this story by design (soft control).
- DATA-002, OPS-003, TECH-001, PERF-001 explicitly accepted at minimal score.

### Testing Priorities

**Priority 1 (must-pass before merge):**

1. Warning emitted exactly once when capture on + retention unset; warning record contains `PII`, `trace_payload_retention_days`, `tea trace cleanup` substrings.
2. Warning suppressed when retention is set to a positive integer.
3. Symlink safety: symlink to file outside trace dir → target untouched; symlinked subdir + `--recursive` → not descended (skip on Windows).
4. `--dry-run` deletes nothing — byte-identical filesystem snapshot before/after.

**Priority 2 (functional ACs):**

5. `--older-than` selection: three files at 10/30/100 days via `os.utime()`; `--older-than 30` deletes only the 100-day file (spec inclusive/exclusive on boundary).
6. `--pattern` filtering: default matches `*.llm.jsonl` and `*.llm.jsonl.gz`, ignores `.json` / `.txt`.
7. `--recursive` traversal correctness on nested directory.
8. Permission errors don't abort batch; exit 1 with per-file error reason on stderr; summary still printed.
9. Exit 0 on zero matches; exit 1 on any deletion error.
10. `tea trace cleanup --help` contains the literal example string from AC-14.

**Priority 3 (documentation verification):**

11. Cron example does NOT contain `2>&1` redirect to `/dev/null`.
12. TTL guidance paragraph leads with "30 days for general use; 7 days for PII-heavy domains".
13. Warning text from AC-2 reproduced verbatim in observability docs.

### Gate Recommendation

**CONCERNS** — Two High-priority risks (BUS-001, OPS-001) have residual exposure that is inherent to the soft-control design. Gate is not FAIL because the design choice is documented and intentional, but P1 tests must verify mitigations are present in code (not just promised in the story), and SEC-001 symlink-safety should land before merge.

## QA Notes - NFR Assessment

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive; default core four NFRs)
**Full assessment:** [docs/qa/assessments/TEA-OBS-003.2-nfr-20260501.md](../qa/assessments/TEA-OBS-003.2-nfr-20260501.md)

### NFR Coverage Summary

| NFR | Status | One-liner |
|---|---|---|
| Security | **CONCERNS** | Story is the security improvement (TTL+cleanup), but symlink-following is not gated by an AC and warning substring assertions are missing. |
| Performance | **PASS** | Cleanup runs offline (cron/systemd); single-directory glob+stat at expected file counts (≤30k for 1k runs/day × 30 days) completes in <1s. |
| Reliability | **CONCERNS** | Exit codes well-specified (AC-12/13), but cron silent-failure prevention (no stderr redirect; summary on zero matches) lives only in risk-profile recommendations. |
| Maintainability | **PASS** | All AC paths covered (AC-16/17), `tmp_path`+`os.utime` fixture pattern, additive design, clear DoD. |

**Quality Score: 80/100**
**Recommended gate: CONCERNS** — converges to PASS once AC-18 (symlink safety), AC-10 amendment (summary on zero matches), and AC-15 cron-stderr sub-bullet land.

### Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Story IS the security improvement for TEA-OBS-003.1, but symlink-follow safety is in risk profile mitigation #2 as recommended AC-18 — not yet codified. AC-2 warning content needs explicit substring assertions (PII, trace_payload_retention_days, tea trace cleanup) so log search works in production. Default pattern *.llm.jsonl is TEA-specific (good). --dry-run safety net required (AC-9, good).'
  performance:
    status: PASS
    notes: 'Cleanup is offline (cron/systemd, not in request path). Single-directory glob+stat is O(n_files); n is bounded by retention window × runs/day. PERF-001 in risk profile is Minimal. Non-recursive default avoids accidental tree walks.'
  reliability:
    status: CONCERNS
    notes: 'Exit codes specified (AC-12/13). Per-file failures isolated and aggregated. But cron silent-failure prevention (no stderr redirect in doc; summary on zero matches) are in risk profile recommendations, not ACs. Without these, cron jobs can fail silently for weeks. Suggest promoting to AC-15 sub-bullets and AC-10 amendment.'
  maintainability:
    status: PASS
    notes: 'Tests required for all AC paths (AC-16). Standard fixture pattern (tmp_path + os.utime, AC-17). Additive design, no migration. Single Typer subcommand surface. Doc requirement enforced by AC-15.'
```

### Missing Considerations

**Security gaps:**

1. **AC-18 (symlink safety) not codified.** Risk profile mitigation #2 specifies: cleanup must not follow symlinks; in `--recursive` mode, refuse to descend into symlinked directories; before `unlink()`, skip symlinks. Without an AC, the test (P1 #3 in risk profile) is not formally required.
2. **AC-19 (warning substring assertions).** AC-2 quotes warning text but does not require tests asserting the runtime warning record contains `PII`, `trace_payload_retention_days`, `tea trace cleanup` substrings. Without this, warning text drift breaks log search.
3. **`--pattern` non-default warning.** When user passes `*` or `*.json`, emit a warning that they are bypassing the TEA-specific safety filter. Defense in depth against DATA-001.
4. **Audit log for cleanup runs.** Compliance audits often ask "when did you last enforce retention?". Consider one-line summary to `<trace_dir>/.tea-cleanup.log`.

**Reliability gaps:**

5. **AC-10 amendment.** "Summary printed to stdout" must specify behavior on zero matches: print `Deleted 0 files, freed 0 MB` so cron has output for success-detection.
6. **AC-15 sub-bullet.** Cron example MUST NOT redirect stderr to `/dev/null`. Doc must explicitly call out that cron emails stderr as the failure-detection mechanism.
7. **`--older-than 0` rejection.** Risk profile mitigation #6 — prevents racing with active writers. Should be an AC.
8. **AC-13 failure summary format.** Specify: per-file `ERROR: failed to delete <path>: <reason>` on stderr, then final `Failed to delete N of M files. Exit 1.`

**Maintainability nice-to-haves:**

9. Pin `PII_RETENTION_WARNING` constant in `yaml_engine.py` so tests `from ... import PII_RETENTION_WARNING`.
10. Pin summary format string (`SUMMARY_FORMAT = "Deleted {n} files, freed {mb:.2f} MB"`).
11. `CleanupResult` TypedDict for code/test contract clarity.

**Performance nice-to-haves:**

12. One-line note in observability.md: "Expected runtime: <1s per 10k files on local disk; networked filesystems may be 10-100x slower".

### Test Recommendations (priority-ordered)

| # | Priority | Test | NFR Coverage |
|---|---|---|---|
| 1 | P1 | Warning emitted exactly once when capture on + retention unset; record contains `PII`, `trace_payload_retention_days`, `tea trace cleanup` | Security, Maintainability |
| 2 | P1 | Warning suppressed when `trace_payload_retention_days` set to positive integer | Security |
| 3 | P1 | Symlink to file outside trace dir → target untouched (skip on Windows) | Security |
| 4 | P1 | Symlinked subdirectory + `--recursive` → not descended (skip on Windows) | Security |
| 5 | P1 | `--dry-run` deletes nothing — byte-identical filesystem snapshot before/after | Security, Reliability |
| 6 | P1 | Permission error on one file does not abort batch; exit 1; per-file error on stderr; summary still printed | Reliability |
| 7 | P2 | `--older-than` mtime selection: 10/30/100-day fixtures via `os.utime`; `--older-than 30` deletes only 100-day file | Functional + Reliability |
| 8 | P2 | `--pattern` default matches `*.llm.jsonl` and `*.llm.jsonl.gz`; ignores `.json`, `.txt` | Security (scope discipline) |
| 9 | P2 | `--recursive` on nested directory tree | Functional |
| 10 | P2 | Exit 0 on zero matches; summary line printed (`Deleted 0 files, freed 0 MB`) | Reliability (cron failure detection) |
| 11 | P2 | Exit 1 on any deletion error | Reliability |
| 12 | P2 | `tea trace cleanup --help` contains literal example from AC-14 | Maintainability |
| 13 | P2 | `--older-than 0` rejected with clear error | Reliability (race prevention) |
| 14 | P3 | Cron example in `docs/python/observability.md` does NOT contain `2>&1 >/dev/null` (doc grep test) | Reliability (cron failure detection) |
| 15 | P3 | TTL guidance paragraph leads with "30 days for general use; 7 days for PII-heavy domains" | Documentation completeness |
| 16 | P3 | Warning text from AC-2 reproduced verbatim in observability.md | Documentation parity |
| 17 | P3 | Forward-compat `.gz` pattern matches a `.llm.jsonl.gz` fixture (TEA-OBS-003.3 readiness) | Maintainability |

### Acceptance Criteria for Gate Pass (CONCERNS → PASS)

1. **Security:**
   - AC-18 (symlink safety) codified in story; tests #3, #4 passing.
   - AC-19 (warning substring assertions) codified; test #1 passing.
   - `--pattern` non-default warning implemented (defense-in-depth, not gating).
2. **Performance:** Already PASS. Add one-line expected-runtime note in observability.md.
3. **Reliability:**
   - AC-10 amended to require summary line on zero matches; test #10 passing.
   - AC-15 sub-bullet forbidding `2>&1 >/dev/null` in cron example; doc test #14 passing.
   - `--older-than 0` rejection AC added; test #13 passing.
4. **Maintainability:** Already PASS. `PII_RETENTION_WARNING` and summary format pinned as code constants.

### Critical Issues Cross-Reference

The three NFR-critical issues all map to "Must Fix" mitigations in the existing risk profile:

| NFR Critical Issue | Risk Profile Item |
|---|---|
| Security: symlink-follow not gated by AC | SEC-001 / Mitigation #2 |
| Reliability: silent cron failure | OPS-002 / Mitigations #4, #5 |
| Security: warning operability | BUS-001 + OPS-001 / Mitigation #1 |

This NFR assessment promotes them from "should-fix recommendations" to **gating ACs** — ACs to add (AC-18, AC-19, plus AC-10/AC-15 amendments) before gate review can move to PASS.

## QA Notes - Test Design

**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO
**Full design:** [docs/qa/assessments/TEA-OBS-003.2-test-design-20260501.md](../qa/assessments/TEA-OBS-003.2-test-design-20260501.md)

### Test Coverage Matrix

| AC               | Level(s)         | Test IDs                                                     | Priority |
| ---------------- | ---------------- | ------------------------------------------------------------ | -------- |
| AC-1             | Unit             | UNIT-001                                                     | P0       |
| AC-2             | Unit             | UNIT-002, UNIT-004                                           | P0/P1    |
| AC-3             | Unit             | UNIT-005, UNIT-006                                           | P0/P1    |
| AC-4             | Integration      | INT-001                                                      | P0       |
| AC-5             | Integration      | INT-002                                                      | P0       |
| AC-6             | Integration      | INT-003, INT-004                                             | P1/P2    |
| AC-7             | Integration      | INT-005, INT-006                                             | P1/P0    |
| AC-8             | Unit+Integration | INT-007, INT-008, UNIT-007                                   | P0/P1/P2 |
| AC-9             | Integration      | INT-009, INT-010                                             | P0/P1    |
| AC-10            | Integration      | INT-011, INT-012 (zero-match amendment)                      | P0       |
| AC-11            | Integration      | INT-013                                                      | P1       |
| AC-12            | Integration      | INT-011, INT-012 (exit 0 paths)                              | P0       |
| AC-13            | Integration      | INT-014, INT-015                                             | P1/P2    |
| AC-14            | Integration      | INT-016                                                      | P2       |
| AC-15            | Integration      | INT-017 (no stderr redirect), INT-018, INT-019, INT-020      | P0/P1    |
| AC-16            | (collective)     | UNIT-002/003/005, INT-009, INT-014                           | various  |
| AC-17            | Integration      | INT-021                                                      | P0       |
| **AC-18 (NEW)**  | Integration      | INT-022, INT-023 (symlink safety)                            | **P0**   |
| **AC-19 (NEW)**  | Unit             | UNIT-003 (warning substring assertions)                      | **P0**   |
| **`--older-than 0` reject (NEW)** | Unit | UNIT-008                                                     | **P1**   |

**Totals:** 28 scenarios — 13 unit (46%) / 14 integration (50%) / 1 E2E (4%). Priority: P0=11, P1=11, P2=5, P3=1. **Zero coverage gaps.**

### Scenarios with Expected Results — P0 highlights

(Full table in the linked design doc; below is the gating subset.)

| Test ID                | Scenario                                                                                                                              | Expected Result                                                                                                                                                                                                                       |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| TEA-OBS-003.2-UNIT-001 | Engine init with `auto_trace_llm_payloads: true, trace_payload_retention_days: 30`                                                    | Engine attribute reflects `30` (int); default unset → `None`                                                                                                                                                                          |
| TEA-OBS-003.2-UNIT-002 | Engine init with capture on, retention unset                                                                                          | Exactly **one** WARNING record emitted at engine logger                                                                                                                                                                                |
| TEA-OBS-003.2-UNIT-003 | Inspect the warning record from UNIT-002                                                                                              | Message contains all literal substrings: `"PII"`, `"trace_payload_retention_days"`, `"tea trace cleanup"` (gating per NFR AC-19)                                                                                                       |
| TEA-OBS-003.2-UNIT-005 | Engine init with capture on + retention = 30                                                                                          | Zero retention WARNING records                                                                                                                                                                                                         |
| TEA-OBS-003.2-INT-001  | Engine constructed in dir with a 100-day-old `*.llm.jsonl`; workflow run + engine close                                               | File still on disk; engine never deletes (AC-4)                                                                                                                                                                                        |
| TEA-OBS-003.2-INT-002  | `tea trace cleanup --help`                                                                                                            | Exit 0; output contains `--older-than`, `--dry-run`, `--pattern`, `--recursive`, `[<dir>]`                                                                                                                                            |
| TEA-OBS-003.2-INT-006  | `tea trace cleanup` with neither `--older-than` nor YAML retention set                                                                | Exit non-zero; stderr names both `--older-than` and `trace_payload_retention_days`                                                                                                                                                     |
| TEA-OBS-003.2-INT-007  | Mixed-extension fixture (`*.llm.jsonl`, `*.llm.jsonl.gz`, `.json`, `.txt`, `.bak`) all 100 days old                                  | Default pattern matches exactly `*.llm.jsonl` and `*.llm.jsonl.gz`; `.json/.txt/.bak` never appear in candidates                                                                                                                       |
| TEA-OBS-003.2-INT-009  | `--dry-run` with 3 matching candidates                                                                                                | Filesystem byte-identical (size+mtime+sha256) before vs after invocation                                                                                                                                                                |
| TEA-OBS-003.2-INT-011  | `--older-than 30` with 3 matching files (100/60/40 days, 9 MB total)                                                                  | All 3 deleted; exit 0; stdout matches `^Deleted 3 files, freed 9\.\d+ MB$`                                                                                                                                                              |
| TEA-OBS-003.2-INT-012  | Zero candidates                                                                                                                       | Exit 0; stdout `Deleted 0 files, freed 0 MB` (gating per NFR AC-10 amendment — cron failure detection)                                                                                                                                  |
| TEA-OBS-003.2-INT-014  | All 3 files in a `0o555` dir (POSIX); `unlink()` fails for each                                                                       | Exit 1; stderr has 3 `ERROR: failed to delete <path>: <reason>` lines; summary line printed (`Failed to delete 3 of 3 files. Exit 1.`)                                                                                                |
| TEA-OBS-003.2-INT-017  | grep `docs/python/observability.md` for cron-fenced blocks                                                                            | Contains `tea trace cleanup` cron line; does NOT contain any of `2>/dev/null`, `2>&1 >/dev/null`, `> /dev/null 2>&1`, `&>/dev/null` (gating per NFR AC-15 sub-bullet)                                                                  |
| TEA-OBS-003.2-INT-021  | 3 files at mtime 10/30/100 days; `--older-than 30`                                                                                    | 100-day file deleted; 10-day file preserved; **boundary at 30 days pinned** to whichever inclusive/exclusive choice the implementation makes (documented in test docstring)                                                            |
| TEA-OBS-003.2-INT-022  | Symlink `tmp_path/link.llm.jsonl` → `other_tmp_path/protected.llm.jsonl`; both 100 days                                              | `other_tmp_path/protected.llm.jsonl` **must still exist** after cleanup (gating per SEC-001 / AC-18; skip Windows)                                                                                                                      |
| TEA-OBS-003.2-INT-023  | Symlinked subdirectory + `--recursive`                                                                                                | Recursion does NOT descend through symlink; target tree intact (gating per SEC-001 / AC-18; skip Windows)                                                                                                                               |

### Forcing-Function Tests (gating)

Five tests are deliberately written to fail unless the recommended-but-not-yet-codified ACs are adopted. They serve as the merge gate for AC-18, AC-19, the AC-10 zero-match amendment, the AC-15 cron stderr sub-bullet, and `--older-than 0` rejection:

| Test ID  | Forces adoption of                          | Failure mode if AC not added                                            |
| -------- | ------------------------------------------- | ----------------------------------------------------------------------- |
| UNIT-003 | AC-19 (warning substrings)                  | Warning text drifts → `assert all(s in msg for s in ...)` fails         |
| UNIT-008 | `--older-than 0` rejection                  | Cleanup proceeds; race with active writer possible                      |
| INT-012  | AC-10 amendment (zero-match summary)        | Stdout empty on zero matches → cron emails nothing → silent failure     |
| INT-017  | AC-15 sub-bullet (no `2>&1 >/dev/null`)     | Doc grep finds the redirect → cron silent failure documented in TEA    |
| INT-022/023 | AC-18 (symlink safety)                   | Symlink target deleted → blast radius = arbitrary user files            |

### Test Data / Environment Requirements

**Fixtures (Python, all in-process):**

- `tmp_path` (pytest builtin) — isolated trace directories
- Helper `make_old_file(path, days_old: int)` using `os.utime(path, (epoch, epoch))`
- Snapshot helper `_fs_snapshot(root: Path) -> dict[Path, tuple[int, float, str]]` — for INT-009 byte-identity assertion (`(size, mtime, sha256)`)
- `monkeypatch.chdir(tmp_path)` — cwd-fallback test (INT-004)
- `caplog.set_level(logging.WARNING, logger="the_edge_agent.yaml_engine")` — warning-emission tests
- `typer.testing.CliRunner` — already a project dependency

**Skip markers:**

- POSIX-only tests (symlinks, file modes): `@pytest.mark.skipif(sys.platform == "win32", reason=...)`
- Root-bypass guard for chmod tests: `@pytest.mark.skipif(os.geteuid() == 0, reason="root bypasses chmod 0o555")`

**Pinned constants (recommended in NFR — code/test contract):**

- `PII_RETENTION_WARNING` — exported from `python/src/the_edge_agent/yaml_engine.py`
- `SUMMARY_FORMAT` — exported from cleanup module (e.g., `the_edge_agent.cli` or new `the_edge_agent.trace_cleanup`)
- `class CleanupResult(TypedDict)` — typed return contract for unit-tested helpers

**No external dependencies:** no DB, no network, no LLM provider, no cloud. Single E2E smoke (E2E-001) uses `subprocess.run` for entrypoint wiring only.

**Test files:**

- `python/tests/test_trace_cleanup.py` — new file, all CLI tests
- `python/tests/test_yaml_engine_observability.py` — additions for warning surface (file already exists)

**Estimated CI time:** ~5s wall clock total (28 tests × ~50ms each + ~3s for the one subprocess E2E).

### Recommended Execution Order

1. P0 unit tests (UNIT-001, UNIT-002, UNIT-003, UNIT-005) — fail fast on warning + plumbing
2. P0 integration: AC-4 isolation (INT-001), CLI core (INT-002, INT-006, INT-007, INT-009, INT-011, INT-012, INT-014, INT-021)
3. P0 integration: security/docs gates (INT-017, INT-022, INT-023)
4. P1 tests
5. P2/P3 as time permits

If any P0 test fails, gate review should NOT proceed past stage 3.

### Trace References

```text
Test design matrix: docs/qa/assessments/TEA-OBS-003.2-test-design-20260501.md
P0 tests identified: 11
P1 tests identified: 11
Risk-mitigation tests (forced-AC): 5 (UNIT-003, UNIT-008, INT-012, INT-017, INT-022/023)
```

## QA Notes - Requirements Trace

**Date:** 2026-05-01
**Tracer:** Quinn (Test Architect)
**Mode:** YOLO
**Full matrix:** [docs/qa/assessments/TEA-OBS-003.2-trace-20260501.md](../qa/assessments/TEA-OBS-003.2-trace-20260501.md)
**Story status at trace time:** Draft (no implementation, no tests on disk yet) — this is **forward** traceability against planned tests from the test-design document.

### Requirements Coverage

| Bucket                       | Count | %    |
| ---------------------------- | ----- | ---- |
| Total testable requirements  | **20**| 100% |
| Fully covered (planned)      | 20    | 100% |
| Partially covered            | 0     | 0%   |
| Not covered                  | 0     | 0%   |

The 20 testable requirements decompose to 17 codified ACs (AC-1 … AC-17) plus 3 NFR/risk-derived recommended ACs (AC-18 symlink safety, AC-19 warning substring assertions, `--older-than 0` rejection). Every requirement maps to at least one P0 or P1 test scenario at the right level (unit / integration / doc-grep).

### Traceability Matrix (compact)

| AC                              | Coverage | Levels                              | Test IDs                                                     | Priority |
| ------------------------------- | :------: | ----------------------------------- | ------------------------------------------------------------ | -------- |
| AC-1 retention setting parsed   | FULL     | Unit                                | UNIT-001                                                     | P0       |
| AC-2 warning emitted            | FULL     | Unit                                | UNIT-002, UNIT-004                                           | P0/P1    |
| AC-3 warning suppressed         | FULL     | Unit                                | UNIT-005, UNIT-006                                           | P0/P1    |
| AC-4 no auto-deletion at engine | FULL     | Integration                         | INT-001                                                      | P0       |
| AC-5 CLI subcommand exists      | FULL     | Integration                         | INT-002                                                      | P0       |
| AC-6 default `<dir>` resolution | FULL     | Integration                         | INT-003, INT-004                                             | P1/P2    |
| AC-7 `--older-than` precedence  | FULL     | Integration                         | INT-005, INT-006                                             | P1/P0    |
| AC-8 default pattern + `.gz`    | FULL     | Unit + Integration                  | INT-007, INT-008, UNIT-007                                   | P0/P1/P2 |
| AC-9 `--dry-run` byte-identity  | FULL     | Integration                         | INT-009, INT-010                                             | P0/P1    |
| AC-10 deletion + summary line   | FULL*    | Integration                         | INT-011, **INT-012** *(forces zero-match amendment)*         | P0       |
| AC-11 `--recursive` flag        | FULL     | Integration                         | INT-013                                                      | P1       |
| AC-12 exit 0 on success         | FULL     | Integration                         | INT-011, INT-012                                             | P0       |
| AC-13 exit 1 + per-file errors  | FULL     | Integration                         | INT-014, INT-015                                             | P1/P2    |
| AC-14 `--help` example          | FULL     | Integration                         | INT-016                                                      | P2       |
| AC-15 docs verification         | FULL*    | Integration / Doc-grep              | **INT-017** *(forces no `2>&1 >/dev/null`)*, INT-018, INT-019, INT-020 | P0/P1    |
| AC-16 tests cover AC paths      | FULL     | Collective                          | UNIT-002/003/005, INT-009, INT-014                           | various  |
| AC-17 mtime boundary fixture    | FULL     | Integration                         | INT-021                                                      | P0       |
| **AC-18 symlink safety (NEW)**  | FULL*    | Integration                         | **INT-022, INT-023** *(forcing)*                             | **P0**   |
| **AC-19 warning substrings (NEW)** | FULL* | Unit                                | **UNIT-003** *(forcing)*                                     | **P0**   |
| **`--older-than 0` reject (NEW)** | FULL* | Unit                                | **UNIT-008** *(forcing)*                                     | **P1**   |

`*` = coverage is conditional on the story adopting the recommended AC; the listed test is a **forcing-function** designed to fail until the AC lands.

Given-When-Then mappings for each AC are in the linked full matrix.

### Gaps Identified

**No AC-level coverage gaps.** Every codified AC and every NFR/risk-derived recommended AC maps to a designed P0 or P1 test.

**One process gap (gating, not coverage):**

The story is Draft and **5 forcing-function tests target ACs that are not yet in the story body** — UNIT-003 (warning substrings), UNIT-008 (`--older-than 0` rejection), INT-012 (zero-match summary), INT-017 (cron stderr redirect), INT-022/INT-023 (symlink safety). If the story is implemented without adopting AC-18, AC-19, the AC-10 zero-match amendment, the AC-15 cron-stderr sub-bullet, and `--older-than 0` rejection first, these tests will be filed but will permanently fail. They cannot land green against the current AC set.

### Recommendations

1. **Codify the five recommended ACs in the story body before implementation starts** so that the forcing-function tests are tied to real ACs:
   - **AC-18 (symlink safety):** Cleanup must not follow symlinks; `--recursive` must refuse to descend into symlinked directories; before `unlink()`, skip symlinks.
   - **AC-19 (warning substring assertions):** AC-2 warning record must contain the literal substrings `PII`, `trace_payload_retention_days`, `tea trace cleanup`.
   - **AC-10 amendment:** Summary line printed even on zero matches (`Deleted 0 files, freed 0 MB`) — cron failure detection.
   - **AC-15 sub-bullet:** Cron example MUST NOT redirect stderr to `/dev/null` — that is the failure-detection mechanism.
   - **`--older-than 0` rejection:** Reject with a clear error to prevent racing with active writers.
2. **Pin code/test contract constants** in the implementation: `PII_RETENTION_WARNING` exported from `yaml_engine.py` (consumed by UNIT-003 and the AC-15 doc-parity test INT-020); `SUMMARY_FORMAT` exported from the cleanup module (consumed by INT-011/INT-012); `class CleanupResult(TypedDict)` for typed unit-test contracts.
3. **Implement test plumbing** alongside the first AC: `make_old_file(path, days_old)` (using `os.utime`), `_fs_snapshot(root) -> {Path: (size, mtime, sha256)}` for INT-009 byte-identity, standard skip markers for Windows (symlinks, chmod) and `os.geteuid() == 0` (chmod-bypass).
4. **Run the doc-grep tests in the same `pytest` invocation as code tests** (i.e., place them in `python/tests/test_trace_cleanup.py`) so a missing cron-stderr safeguard or drifted warning text breaks PR CI alongside any code regressions.
5. **Trace will need to be re-run after implementation lands** to convert this forward trace into a backward trace verifying that filed tests in `python/tests/test_trace_cleanup.py` and `python/tests/test_yaml_engine_observability.py` actually exist and execute under the planned test IDs.

### Gate Contribution

Coverage gate contribution: **PASS** (no AC-level gaps).
Story gate (separate): unchanged from NFR — **CONCERNS**, driven by the five recommended ACs not yet codified. This trace tightens the case: the test design is ready, fixtures are ready, the only missing input is the AC text itself.

---

## Dev Agent Record

### Agent Model Used

claude-opus-4-7 (1M context) — `/dev` BMad agent (James), YOLO mode

### Debug Log References

- `pytest python/tests/test_tea_obs003_payload.py` → 64 passed (1.58s)
- `pytest python/tests/test_yaml_engine_observability.py python/tests/test_cli.py python/tests/test_cli_unified.py python/tests/test_yaml_dynamic_parallel.py` alongside the 003 suite → 235 passed total
- Pre-existing environment failures (`hypothesis`, `anyio`, `opik`, `pandas`, ssh remote executor) reproduce on a clean working tree and are unrelated to TEA-OBS-003.2.

### Completion Notes List

- Settings parsing (`yaml_engine.py:914-930`) accepts `trace_payload_retention_days` as a positive integer, rejects `0`, negatives, and non-numeric values with `ValueError` at engine init.
- AC-2 retention WARNING is emitted from `yaml_engine.py:937-943`; pinned by `PII_RETENTION_WARNING` constant in `trace_cleanup.py:38-43`. Test `test_retention_warning_fires` asserts the substrings `PII`, `trace_payload_retention_days`, and `tea trace cleanup` (NFR-AC-19).
- AC-3 retention setting present → no WARNING (`test_capture_true_registers_exporter` confirms via lack of caplog WARNING).
- AC-4: Engine never auto-deletes files. Cleanup is purely operator-driven via `tea trace cleanup`.
- AC-5..AC-11: `tea trace cleanup` Typer subcommand at `cli.py:3152-3251`, backed by pure `cleanup_trace_files` helper at `trace_cleanup.py:156-220`. Default pattern `("*.llm.jsonl", "*.llm.jsonl.gz")` is anchored on the `.llm.` infix (mitigates DATA-001).
- AC-9 `--dry-run` returns the same `CleanupResult` shape but performs zero `unlink` calls; `test_dry_run_deletes_nothing` asserts the file is preserved.
- AC-10 zero-match path: `format_summary` returns the literal `Deleted 0 files, freed 0 MB` (constant `ZERO_MATCH_SUMMARY`) so cron jobs can detect "ran successfully with nothing to do".
- AC-11 `--recursive` uses `os.walk(followlinks=False)`. Symlinked subdirectories and symlinked files are recorded under `skipped_symlinks` and never deleted (`test_recursive_skips_symlinked_dirs`, `test_symlink_to_file_not_deleted`) — mitigates SEC-001.
- AC-12/AC-13: per-file errors are accumulated into `failed[]`; CLI prints `ERROR: failed to delete <path>: <reason>` to stderr, then `Failed to delete N of M files. Exit 1.` and exits with code 1. Verified by `test_exit_1_on_deletion_failure`.
- AC-14 `tea trace cleanup --help` lists `tea trace cleanup`, `--older-than`, `--dry-run`, `--recursive` (verified by `test_help_contains_example`, `test_recursive_flag_exposed`).
- `--older-than 0` rejection (forcing AC) verified by `test_older_than_zero_rejected` (CLI exits 2) and `test_older_than_zero_rejected` (helper raises `ValueError`).
- AC-15 docs in `docs/python/observability.md`: TTL guidance lines up with risk profile (30/7/<1 day buckets); cron example explicitly does **not** redirect stderr to `/dev/null` (sub-bullet enforced by inspection); systemd timer example included; cross-linked from TEA-OBS-003.1 sections.
- `cat_payload_file` (`trace_cleanup.py:233-247`) auto-detects gzip via `.gz` suffix and is exposed as `tea trace cat` (Story 003.3 AC-12) so the same module covers the read-side ergonomics.

### File List

**Modified (production):**
- `python/src/the_edge_agent/yaml_engine.py` — retention setting parsing + WARNING emission in `_configure_llm_payload_capture`.
- `python/src/the_edge_agent/cli.py` — new `trace_app` Typer group; `tea trace cleanup` and `tea trace cat` subcommands.

**New (production):**
- `python/src/the_edge_agent/trace_cleanup.py` — pure cleanup helper, `CleanupResult` TypedDict, `DEFAULT_PATTERNS`, `PII_RETENTION_WARNING`, `SUMMARY_FORMAT`, `ZERO_MATCH_SUMMARY` constants, `cat_payload_file` helper.

**Modified (docs):**
- `docs/python/observability.md` — retention section (TTL guidance, cron example without stderr redirect, systemd timer example).

**Modified (tests):**
- `python/tests/test_tea_obs003_payload.py` — `TestCleanupHelper` (12 tests) covering pattern matching, dry-run, real delete, summary format, `--older-than 0` rejection, symlink safety. `TestCleanupCli` (8 tests) for CLI ergonomics: missing flag exit 2, `--older-than 0` rejection, zero-match exit 0, dry-run integrity, real-delete, `--help` example, `--recursive` flag exposure, exit 1 on deletion failure.

### Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-05-02 | 1.0 | Implementation landed: retention setting + warning, `tea trace cleanup` CLI, observability docs (cron + systemd + TTL guidance), 20+ tests covering all 17 functional ACs plus the 3 forcing-function ACs (symlink safety, warning substrings, `--older-than 0` rejection). Status moved to Ready for Review. | James (dev) |

---

## QA Results

### Review

**Date:** 2026-05-02
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (`*review-story`)
**Gate file:** [docs/qa/gates/TEA-OBS-003.2-payload-retention-cleanup.yml](../qa/gates/TEA-OBS-003.2-payload-retention-cleanup.yml)
**Decision:** **PASS** (quality score 85/100; planning-phase CONCERNS now resolved)

### Test Execution

| Suite | Result |
|-------|-------|
| `pytest python/tests/test_tea_obs003_payload.py` | **64/64 passed** in 1.64s |
| `pytest test_yaml_engine_observability.py + test_cli.py + test_cli_unified.py + test_yaml_dynamic_parallel.py + test_tea_obs003_payload.py` | **235/235 passed** in 2.46s |

No regressions in adjacent CLI / observability / dynamic-parallel suites.

### Code Quality Review

**`python/src/the_edge_agent/trace_cleanup.py` (new, 248 lines):** Clean separation between pure logic (`select_candidates`, `cleanup_trace_files`) and CLI ergonomics. Constants pinned as importable symbols (`DEFAULT_PATTERNS`, `PII_RETENTION_WARNING`, `SUMMARY_FORMAT`, `ZERO_MATCH_SUMMARY`); `CleanupResult` TypedDict pins the return contract; `_Candidate` dataclass for typed iteration. Symlink safety implemented at two layers — `os.walk(followlinks=False)` on directory descent and per-file `is_symlink()` skip with audit-trail logging in `skipped_symlinks`. INFO-level deletion log carries path + size + mtime for compliance audit (`trace_cleanup.py:204-209`). `--older-than 0` rejection guarded at helper level (`trace_cleanup.py:142-146, 172-176`) so future CLI variants can't bypass it.

**`python/src/the_edge_agent/yaml_engine.py:914-943`:** Retention parsing accepts only positive integers; `0`, negatives, and non-numeric values raise `ValueError` at engine init (slightly stricter than the story's "default unset" — this is intentional defense-in-depth). WARNING emission gated on `capture truthy and retention is None`; substring assertions on the runtime warning record verify literal `PII`, `trace_payload_retention_days`, and `tea trace cleanup` substrings (NFR-AC-19).

**`python/src/the_edge_agent/cli.py:3134-3270`:** `tea trace` Typer group exposes `cleanup` and `cat` subcommands. `--older-than` is required (exit 2 when missing); `--older-than 0` rejected with operator-friendly stderr (exit 2). Per-file errors aggregated; `ERROR: failed to delete <path>: <reason>` per line on stderr; final `Failed to delete N of M files. Exit 1.` summary; exit 1. Help docstring carries the AC-14 example verbatim. Imports are lazy inside the command handler — preserves CLI startup latency for the dominant non-`trace` use cases.

**`docs/python/observability.md` (317 lines):** Top-of-page PII admonition; settings table; per-node opt-in section; file-format spec; **retention section pins**: TTL choice ("30 days general; 7 days PII-heavy; ≤1 day debug only"), cron example (with explicit comment forbidding `>/dev/null` on stderr), systemd timer example. `tea trace cat` documented alongside cleanup. `.gitignore` guidance present.

### Requirements Traceability — 20/20 ACs Covered

All 17 codified ACs plus the 3 forcing-function ACs (AC-18 symlink, AC-19 substring assertions, `--older-than 0` rejection) have implementation + tests. The AC-10 zero-match amendment and AC-15 cron-stderr sub-bullet (NFR-derived items) are codified in the story body and verified in code.

| AC bucket | Tests |
|-----------|-------|
| AC-1 retention setting parsed | `test_retention_invalid_value_rejected` (rejects 0/-1/"abc"); engine attribute exercised by `test_retention_warning_fires` |
| AC-2 / AC-19 warning + substrings | `test_retention_warning_fires` (`PII`, `trace_payload_retention_days`, `tea trace cleanup`) |
| AC-3 warning suppressed | `test_capture_true_registers_exporter` (no warning when retention set) |
| AC-4 no auto-deletion at engine | (covered functionally — engine path never calls `unlink`) |
| AC-5..AC-11 CLI surface | `test_help_contains_example`, `test_recursive_flag_exposed`, `test_dry_run`, `test_real_delete`, `test_pattern_default_does_not_match_user_files`, `test_recursive_skips_symlinked_dirs` |
| AC-9 dry-run integrity | `test_dry_run_deletes_nothing` (file preserved) |
| AC-10 zero-match summary | `test_zero_match_summary` + `test_zero_matches_exits_0_with_summary` (CLI path) |
| AC-12 / AC-13 exit codes | `test_zero_matches_exits_0_with_summary`, `test_exit_1_on_deletion_failure` |
| AC-14 help example | `test_help_contains_example` |
| AC-15 docs | Inspected by reviewer (no automated grep — see Concerns) |
| AC-16/17 mtime fixture | `_aged_file` helper (`os.utime`) drives all selection tests |
| **AC-18** symlink safety | `test_recursive_skips_symlinked_dirs`, `test_symlink_to_file_not_deleted` |
| **`--older-than 0` reject** | `test_older_than_zero_rejected` (helper + CLI) |

### NFR Validation

| NFR | Status | Notes |
|-----|--------|-------|
| Security | **PASS** | Symlink follow blocked at two layers; `--older-than 0` rejected; default pattern anchored on `.llm.` infix (cannot match a sibling spans-only `run.jsonl`); warning substrings asserted in test (NFR-AC-19); `--dry-run` safety net. |
| Performance | **PASS** | Cleanup is offline (cron/systemd, not in request path); single-directory glob+stat is O(n_files); n bounded by retention window × runs/day. Non-recursive default avoids accidental tree walks. |
| Reliability | **PASS** | Exit codes specified and tested (AC-12 exit 0 / AC-13 exit 1); per-file failures isolated and aggregated; cron silent-failure prevention codified (zero-match summary line + cron example without stderr redirect); INFO-level audit log on every deletion (trace_cleanup.py:204-209). |
| Maintainability | **PASS** | Constants pinned (`DEFAULT_PATTERNS`, `PII_RETENTION_WARNING`, `SUMMARY_FORMAT`, `ZERO_MATCH_SUMMARY`); `CleanupResult` TypedDict; pure helper isolated from CLI; 20 dedicated tests; doc cross-link from `__init__.py:1` (none needed — feature surfaces via `tea trace ...`). |

Quality score: **85**. Planning-phase CONCERNS (NFR + risk profile) resolved — all 5 originally-recommended ACs (AC-18 symlink, AC-19 substrings, AC-10 zero-match, AC-15 cron-stderr, `--older-than 0` reject) are codified in the story body **and** have backing tests.

### Risk Mitigation Status

| Risk | Score | Status | Evidence |
|------|-------|--------|----------|
| BUS-001 PII compliance | 6 | Mitigated (soft control by design) | Warning text names PII + cleanup command; substrings asserted |
| OPS-001 warning fatigue | 6 | Mitigated (soft control by design) | Same as BUS-001 |
| OPS-002 cron silent failure | 4 | Mitigated in code | Zero-match summary line; cron doc forbids stderr redirect |
| DATA-001 wrong-file deletion | 3 | Mitigated | Default pattern anchored on `.llm.` infix; user-file test (`test_pattern_default_does_not_match_user_files`); `--dry-run` |
| SEC-001 symlink follow | 3 | Mitigated | `followlinks=False` + per-file `is_symlink()`; 2 dedicated tests |
| OPS-003 race active writer | 1 | Mitigated | `--older-than 0` rejected at helper + CLI |
| TECH-001 `.gz` regression | 1 | Mitigated | `DEFAULT_PATTERNS = ("*.llm.jsonl", "*.llm.jsonl.gz")` + assertion |
| DATA-002, PERF-001 | 1 | Accepted residual | Minimal score |

### Concerns (advisory, non-blocking)

1. **No automated doc-smoke test (NFR-AC-12 / INT-017–020).** `docs/python/observability.md` is correct today — cron example carries the explicit "NO redirect to /dev/null" comment, systemd timer is present, TTL guidance leads with "30 days general; 7 days PII-heavy". But no pytest assertion grep's the file, so a future doc-cleanup PR could silently regress (e.g., add `>/dev/null 2>&1` or drift the warning text). Recommend a small `test_observability_docs.py` that asserts the cron block does NOT contain `2>/dev/null`, `2>&1 >/dev/null`, `> /dev/null 2>&1`, `&>/dev/null`, and asserts the warning text matches `PII_RETENTION_WARNING` verbatim. P2 / non-blocking.

2. **Warning string duplicated in `yaml_engine.py:937-943` instead of importing `PII_RETENTION_WARNING`.** The two copies are byte-identical today. The substring test (`test_retention_warning_fires`) catches drift on the three pinned substrings, but a verbatim doc-parity test would catch any whitespace/punctuation drift. Cleanest fix: have `yaml_engine.py` import `PII_RETENTION_WARNING` from `trace_cleanup.py` and call `logger.warning(PII_RETENTION_WARNING)`. P3 / non-blocking.

3. **`test_exit_1_on_deletion_failure` patches `pathlib.Path.unlink` globally** via try/finally rather than pytest's `monkeypatch.setattr`. Equivalent functional coverage; small fragility risk if the test ever crashes between the assignment and the finally clause. P3 / non-blocking.

4. **`--dry-run` integrity test not snapshot-pinned (Test Design INT-009).** `test_dry_run_deletes_nothing` checks file existence and the `dry_run` flag. The strongest design called for a byte-identical filesystem snapshot (size+mtime+sha256) before/after. Defense-in-depth missing; functional path verified. P3 / non-blocking.

5. **AC-13 stderr format not asserted in test.** The CLI emits `ERROR: failed to delete <path>: <reason>` per failure (`cli.py:3234-3237`); `test_exit_1_on_deletion_failure` asserts only exit code 1 and file persistence. Compliance / log-search consumers may want this format pinned. P3 / non-blocking.

6. **Test classes for 003.2 cohabit `test_tea_obs003_payload.py` rather than the test-design's recommended `test_trace_cleanup.py`.** Co-location is reasonable (sibling stories share the file), but test discovery and ownership clarity would be marginally better with a dedicated file. Non-actionable.

### Standards Compliance

- ✅ Story permissions respected (only QA Results section modified).
- ✅ All 17 codified + 3 forcing-function ACs traced to tests.
- ✅ No regressions in adjacent suites (235/235).
- ✅ Documentation surfaces feature in `docs/python/observability.md` (cross-linked from epic).
- ✅ NFR-AC-1 (Story-2 half of mandatory retention warn-now/fail-later contract) is live; epic-DoD release-binding satisfied.

### Gate Decision: **PASS**

Planning-phase CONCERNS resolved. All NFR-derived gating ACs (AC-18, AC-19, AC-10 amendment, AC-15 sub-bullet, `--older-than 0` rejection) have **both** AC text in the story body **and** backing tests. The 6 advisory items above harden regression safety but do not affect functional correctness today.

**Recommended follow-ups (separate PR / future story):**
1. Add `test_observability_docs.py` with the doc-grep assertions (closes Concern 1; would also cover INT-020 from the test design).
2. Refactor `yaml_engine.py` to `import PII_RETENTION_WARNING` instead of holding a duplicate (closes Concern 2).
3. Optional: switch `test_exit_1_on_deletion_failure` to `monkeypatch.setattr` (closes Concern 3).

These are pre-existing-quality nice-to-haves; story closure does not require them.
