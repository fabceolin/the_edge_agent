# Risk Profile: Story TEA-OBS-003.2

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.2 — Payload retention policy & cleanup
Mode: YOLO (single-pass risk profile)

## Executive Summary

- Total Risks Identified: 9
- Critical Risks: 0
- High Risks: 2 (BUS-001, OPS-001)
- Medium Risks: 1 (OPS-002)
- Low Risks: 2 (DATA-001, SEC-001)
- Minimal Risks: 4 (DATA-002, OPS-003, TECH-001, PERF-001)
- Overall Risk Score: **71/100** (low-to-moderate; gate recommendation = **CONCERNS**)

This story is the policy-and-enforcement counterpart to TEA-OBS-003.1's capture mechanism. It is **fully additive** (engineers not using capture see no behavior change) and contains no destructive default — `tea trace cleanup` is invoked explicitly. The two High-priority risks live in human-process territory, not code territory: warning fatigue (OPS-001) and the consequential downstream regulatory exposure (BUS-001) when an operator enables capture and then never schedules cleanup. Both are partially addressed by the story (warning text names PII explicitly; docs include cron + systemd examples), but neither can be fully eliminated by this story alone — they are residual by design.

The Primary Risk called out in the story itself ("cleanup deletes the wrong files") is correctly scoped as Low here because of the TEA-specific default pattern (`*.llm.jsonl`) plus `--dry-run` safety net plus documentation. That risk exists but its probability is well-controlled by the ACs.

## Critical Risks Requiring Immediate Attention

None at score 9.

## High Risks (Score 6)

### 1. BUS-001: PII retention violates compliance (LGPD / GDPR / sectoral policy)

**Score: 6 (High)** — Probability: Medium (2) × Impact: High (3)

- **Probability rationale:** AC-2 emits a *warning*, not a hard error. Engineers routinely ignore warnings, especially when the feature works without retention set. The user-quoted Portuguese context explicitly anticipates NF-e and financial-document use cases where CNPJ + valores will land in payloads. Once payload capture is on without cleanup, the PII clock starts.
- **Impact rationale:** LGPD/GDPR audit findings, contractual compliance breaches with customers (especially in Brazil-domiciled enterprise contracts), and potential disclosure if a snapshot of the directory is shipped to a log aggregator, backup, or developer laptop.
- **Mitigation:**
  - **Hard:** AC-2 warning text (already in spec) — keep it explicit about PII, do not soften.
  - **Recommend (advisory, beyond spec):** Consider promoting the warning to **ERROR** with an opt-out (`trace_payload_retention_days: unlimited` or similar sentinel) for production deployments. A warning lets engineers ship; an error forces a decision. Out of scope for this story but worth a follow-up ticket.
  - **Recommend:** Doc copy in AC-15 must lead with "30 days for general use; 7 days for PII-heavy domains" so the operator's first reading anchors them on a low TTL, not a high one.
- **Testing focus:**
  - Verify warning is emitted exactly once at engine init (not per-invocation) when capture is on and retention is unset.
  - Verify warning text contains the strings "PII" and "trace_payload_retention_days" so operators searching logs can find it.
  - Verify warning is suppressed when retention is set to any positive integer.
- **Residual risk:** Medium — operational discipline required; this story is a soft control by design.
- **Owner:** dev + docs + security/compliance

### 2. OPS-001: Warning fatigue — operators ignore the unset-retention warning

**Score: 6 (High)** — Probability: High (3) × Impact: Medium (2)

- **Probability rationale:** This is the inverse framing of BUS-001. The story itself names this risk. Python `logging.warning` output gets buried in CI logs, dev consoles, and aggregated stdout. There is no enforcement.
- **Impact rationale:** Medium because the immediate consequence is ops/process, not data-loss or system-down. The downstream regulatory consequence is captured in BUS-001 and not double-counted here.
- **Mitigation:**
  - Per AC-2, the warning text already names PII risk explicitly. No code mitigation beyond that.
  - **Recommend (advisory):** Emit the warning to **stderr** (not just `logging.warning`) so it shows up even when application logging is reconfigured. `logger.warning(...)` plus `sys.stderr.write(...)` belt-and-suspenders pattern.
  - **Recommend (advisory):** Include in the warning text the literal command `tea trace cleanup --help` so the operator has a copy-pasteable next step.
- **Testing focus:**
  - Emit-once-not-spam test: instantiate engine twice in same process; ensure warning fires twice (or once, with explicit decision documented). Default Python logging will dedupe at handler level depending on config; spec the chosen behavior.
  - String-content test: assert "PII" and "trace_payload_retention_days" both appear in the captured warning record.
- **Residual risk:** Medium-High — soft control, no enforcement.
- **Owner:** dev + docs

## Medium Risks (Score 4)

### 3. OPS-002: Cron / systemd cleanup job fails silently

**Score: 4 (Medium)** — Probability: Medium (2) × Impact: Medium (2)

- **Probability rationale:** Cron jobs that fail (binary missing, PATH wrong, permission denied on `/var/lib/tea/traces`) commonly leave no trace if stderr is redirected to `/dev/null`. The example in AC-15 should not redirect stderr.
- **Impact rationale:** Medium because the failure mode is "files accumulate forever" — same as not setting retention at all. This collapses BUS-001's mitigation but the user is no worse off than before this story.
- **Mitigation:**
  - **Doc-level (AC-15):** The cron example must NOT use `>/dev/null 2>&1`. Default cron behavior is to email cron output to root, which is exactly what an admin needs to see failures.
  - **Recommend:** Show a systemd timer example with `OnFailure=` directive, pointing to a status email or monitoring webhook.
  - **Recommend:** Cleanup command should print summary to stdout even on zero matches (AC-12 says zero is success — make sure summary line is still emitted, e.g., `Deleted 0 files, freed 0 MB`). This gives cron something to email.
- **Testing focus:**
  - Test that summary line is printed to stdout even when no files matched.
  - Test that AC-13 partial-failure path exits 1 *and* prints the per-file error reason to stderr.
- **Residual risk:** Low after doc fix.
- **Owner:** docs + dev

## Low Risks (Score 2-3)

### 4. DATA-001: Cleanup deletes a user's non-TEA file matching `*.llm.jsonl`

**Score: 3 (Low)** — Probability: Low (1) × Impact: High (3)

- **Probability rationale:** The story author flagged this as Primary Risk, but mitigations are strong: default pattern is TEA-specific (`*.llm.jsonl` is unambiguous), `--dry-run` is documented as the first action, cleanup is non-recursive by default. A user would have to (a) name a file `*.llm.jsonl` themselves, (b) place it in the trace directory, and (c) skip `--dry-run`. All three.
- **Impact rationale:** High — irreversible deletion of user data.
- **Mitigation (already in spec):**
  - Default pattern is TEA-specific (AC-8).
  - `--dry-run` is a first-class flag (AC-9).
  - Non-recursive default (AC-11).
  - Documentation strongly recommends `--dry-run` before scheduled run (story Risk section).
- **Mitigation (recommend, advisory):**
  - Reject patterns that would match any non-`.llm.jsonl[.gz]` shape — for example, refuse `--pattern '*'` or `--pattern '*.json'` with an error like *"Pattern is too broad and may match non-TEA files. Use --force-pattern to override."* This is paranoid but cheap to add and matches the spirit of the story author's concern.
  - Add a JSONL header sniff: before deleting any file, peek the first line and verify it parses as JSON with the schema produced by TEA-OBS-003.1 (e.g., presence of `event` / `payload` keys). If the sniff fails, skip with a warning. Optional `--no-sniff` to bypass.
- **Testing focus (priority):**
  - `--dry-run` deletes nothing (filesystem unchanged before/after).
  - Custom pattern containing `*` rejected unless `--force-pattern` (if implemented).
  - Files outside default pattern in same dir not touched.
  - Symlinked target not followed (see SEC-001).
- **Residual risk:** Low.
- **Owner:** dev

### 5. SEC-001: Symlink-follow during cleanup deletes files outside trace directory

**Score: 3 (Low)** — Probability: Low (1) × Impact: High (3)

- **Probability rationale:** Most users won't have symlinks in their trace dir. But sophisticated deployments using bind-mounts or shared NFS may. Python's `pathlib.Path.unlink()` follows symlinks by default — **deletes the target, not the link**. Combined with `--recursive`, a malicious or accidental symlink to `/var/log/` could be devastating.
- **Impact rationale:** High — deletion outside intended scope.
- **Mitigation (must implement, not in spec):**
  - In recursive mode, refuse to descend into symlinks (`Path.iterdir()` + `Path.is_symlink()` check).
  - Before `unlink()`, verify file is not a symlink, OR if it is, skip with a warning (do not delete the link's target).
  - Document this behavior in `--help` and observability docs.
- **Mitigation (advisory):**
  - Resolve the cleanup directory to its real path at startup; if any candidate file's `resolve()` is outside that real path, skip it.
- **Testing focus:**
  - Construct a tmpfs with symlink pointing outside the trace dir; run cleanup; assert target is not deleted.
  - Construct symlinked subdirectory; run with `--recursive`; assert subdir target is not descended.
- **Residual risk:** Low if mitigation implemented; otherwise High.
- **Owner:** dev (NEW requirement — recommend adding AC-18 to story)

## Minimal Risks (Score 1)

### 6. DATA-002: mtime-based logic preserves files whose mtime was touched

**Score: 1 (Minimal)** — Probability: Low (1) × Impact: Low (1)

- An external process (backup tool, `rsync -a` without `-t`, indexer) that touches mtimes can keep files alive beyond TTL. Acceptable — the story explicitly chose mtime, and the alternative (parsing JSONL timestamps from inside the file) is significantly more complex and out of scope.
- **Mitigation:** Document mtime-based behavior in `--help` and observability docs. No code change.

### 7. OPS-003: Cleanup races with an active capture writer

**Score: 1 (Minimal)** — Probability: Low (1) × Impact: Low (1)

- A workflow currently writing `current.llm.jsonl` while cleanup runs. mtime is fresh (within `--older-than` window) so file is preserved by selection logic. The race is essentially impossible to hit unless `--older-than 0`.
- **Mitigation:** Refuse `--older-than 0` with a clear message. (Recommend; not in spec.)

### 8. TECH-001: Default pattern omits `.gz` (forward-compatibility with TEA-OBS-003.3)

**Score: 1 (Minimal)** — Probability: Low (1) × Impact: Low (1)

- AC-8 explicitly includes `*.llm.jsonl.gz` in the default pattern. Confirmed in spec; risk exists only if implementer drops the `.gz` glob. Test will catch.
- **Mitigation:** Test asserts both patterns match. (AC-16 covers this implicitly.)

### 9. PERF-001: Cleanup slow on directories with many files

**Score: 1 (Minimal)** — Probability: Low (1) × Impact: Low (1)

- At ~200 MB/day target volume, 90 days = ~18 GB across maybe 90,000 files. `Path.glob()` + `Path.stat()` per file = O(n) syscalls. On a fast disk this is seconds; on slow NFS it could be minutes. Acceptable for a cron-driven cleanup.
- **Mitigation:** None required for this story. Future improvement: directory sharding by date.

## Risk Distribution

### By Category

- Security: 1 risk (0 critical, 0 high, 0 medium, 1 low)
- Performance: 1 risk (all minimal)
- Data: 2 risks (0 critical, 0 high, 0 medium, 1 low, 1 minimal)
- Business: 1 risk (0 critical, 1 high)
- Operational: 3 risks (0 critical, 1 high, 1 medium, 1 minimal)
- Technical: 1 risk (all minimal)

### By Component

- CLI command (`tea trace cleanup`): 4 risks (DATA-001, SEC-001, OPS-002, OPS-003)
- YAML engine init (warning emission): 2 risks (BUS-001, OPS-001)
- Documentation: 2 risks (BUS-001, OPS-002 — share doc mitigations)
- Filesystem interaction: 2 risks (DATA-002, PERF-001)

## Detailed Risk Register

| Risk ID  | Category    | Description                                          | Probability | Impact     | Score | Priority |
| -------- | ----------- | ---------------------------------------------------- | ----------- | ---------- | ----- | -------- |
| BUS-001  | Business    | PII retention violates compliance                    | Medium (2)  | High (3)   | 6     | High     |
| OPS-001  | Operational | Warning fatigue — unset-retention warning ignored    | High (3)    | Medium (2) | 6     | High     |
| OPS-002  | Operational | Cron/systemd cleanup fails silently                  | Medium (2)  | Medium (2) | 4     | Medium   |
| DATA-001 | Data        | Cleanup deletes user's non-TEA `*.llm.jsonl` file    | Low (1)     | High (3)   | 3     | Low      |
| SEC-001  | Security    | Symlink-follow deletes files outside trace dir       | Low (1)     | High (3)   | 3     | Low      |
| DATA-002 | Data        | mtime touched by external tool preserves stale file  | Low (1)     | Low (1)    | 1     | Minimal  |
| OPS-003  | Operational | Cleanup races with active writer                     | Low (1)     | Low (1)    | 1     | Minimal  |
| TECH-001 | Technical   | Default pattern omits `.gz` (regression risk only)   | Low (1)     | Low (1)    | 1     | Minimal  |
| PERF-001 | Performance | Slow cleanup on very large directories               | Low (1)     | Low (1)    | 1     | Minimal  |

## Risk-Based Testing Strategy

### Priority 1: High-risk regression-blocking tests

Map directly to BUS-001 and OPS-001 (warning emission) and the DATA-001/SEC-001 safety nets:

1. **Warning emitted when retention unset** (AC-2, BUS-001/OPS-001)
   - Capture enabled, retention unset → exactly one warning record
   - Warning record contains substrings: `PII`, `trace_payload_retention_days`, `tea trace cleanup`
2. **Warning suppressed when retention set** (AC-3)
   - Capture enabled, retention=30 → zero warning records matching the above pattern
3. **Symlink safety** (SEC-001 — NEW recommendation)
   - tmp_path with symlink to file outside trace dir → cleanup leaves symlink target intact
   - tmp_path with symlinked subdirectory + `--recursive` → cleanup does not descend
4. **`--dry-run` deletes nothing** (AC-9, DATA-001)
   - Snapshot directory contents before/after — assert byte-identical

### Priority 2: Functional ACs

5. **`--older-than` selection logic** (AC-7, AC-10)
   - Use `os.utime()` to set mtimes precisely. Three files: 10/30/100 days old. `--older-than 30` deletes only the 100-day file; 30-day file is *exactly on boundary* — spec the inclusive/exclusive choice.
6. **`--pattern` filters correctly** (AC-8, AC-16)
   - Mixed dir with `.llm.jsonl`, `.llm.jsonl.gz`, `.json`, `.txt`. Default pattern matches first two only.
7. **`--recursive` traversal** (AC-11)
   - Nested dir; without `--recursive` only top-level deleted; with it, all matching files deleted.
8. **Permission errors don't abort** (AC-13, AC-16)
   - One unwritable file in batch → cleanup deletes the others, returns exit 1, prints summary including failed file.
9. **Exit codes** (AC-12, AC-13)
   - Zero matches → exit 0. Any deletion error → exit 1.
10. **Help output** (AC-14)
    - `tea trace cleanup --help` contains the literal example string.

### Priority 3: Doc verification

11. Markdown lint of `docs/python/observability.md` (or chosen location).
12. Cron example does NOT contain `2>&1` redirect to `/dev/null` (OPS-002 mitigation).
13. TTL guidance paragraph leads with "30 days for general use; 7 days for PII-heavy domains".

### Test data requirements

- `tmp_path` fixture with `os.utime()` to control mtimes deterministically.
- Per AC-17: explicit fixture creating files with controlled mtimes is required. Use `pytest.fixture` returning a directory factory.
- For symlink tests: skip on Windows (`pytest.mark.skipif(sys.platform == "win32")`) — symlink semantics differ.

## Risk Acceptance Criteria

### Must Fix Before Merge

- BUS-001 / OPS-001 mitigations as specified by AC-2 (warning text) — must be verified by P1 tests.
- SEC-001 symlink safety — recommend adding as an explicit AC (call it AC-18). Without it, the recursive flag is unsafe.
- DATA-001 `--dry-run` behavior verified by P1 byte-identical test.

### Can Deploy with Mitigation

- OPS-002 silent-cron-failure: mitigated by doc copy. Acceptable.
- OPS-001 warning fatigue: residual; cannot be fully fixed in this story. Acceptable, document.

### Accepted Risks

- DATA-002, OPS-003, PERF-001 — explicitly accepted, low impact.

## Monitoring Requirements

Post-deployment monitoring (operational, not in story scope):

- Track cron job exit codes: alert if `tea trace cleanup` exits non-zero for N consecutive runs.
- Track `*.llm.jsonl` directory size: alert if size grows monotonically for >7 days (cleanup not running).
- Log retention-warning emissions: dashboard count of "capture-on-without-retention" warnings; should trend to zero.

## Risk Review Triggers

Re-run risk profile when:

- TEA-OBS-003.3 (gzip + async exporter) lands — `*.llm.jsonl.gz` becomes the dominant artifact and pattern coverage matters.
- Storage backend changes (e.g., trace files written to S3 instead of disk) — cleanup mechanism would need redesign.
- A real PII incident is reported in a TEA deployment — re-evaluate hardness of retention setting (warning → error promotion).

## Recommendations Summary

### Must Fix (block merge)

- Verify AC-2 warning string contains "PII" and "trace_payload_retention_days".
- Add symlink-safety behavior to cleanup command (SEC-001). Recommend codifying as new AC-18.
- Verify `--dry-run` is byte-identical filesystem snapshot test.

### Should Fix (highly recommended)

- Cron example in docs MUST NOT redirect stderr to `/dev/null` (OPS-002).
- Cleanup command should print summary line even on zero matches (OPS-002).
- Refuse `--older-than 0` with clear message (OPS-003).

### Monitor (advisory, future stories)

- Promote unset-retention warning to ERROR with explicit `unlimited` opt-out for production deployments.
- Add JSONL header sniff before delete (DATA-001 hardening).
- Directory sharding by date for very large deployments (PERF-001).

## Gate Recommendation

**Gate = CONCERNS**

Rationale: 2 risks at score 6 (BUS-001, OPS-001). Both have mitigations called out in the story but residual risk is non-zero and inherent to the soft-control design. CONCERNS is appropriate (not FAIL) because the residual risk is by-design and documented; the gate should pass once P1 tests verify the mitigations are actually present in code, and once the SEC-001 symlink-safety hardening lands.

## risk_summary (gate paste block)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 2
    medium: 1
    low: 2
  highest:
    id: BUS-001
    score: 6
    title: 'PII retention violates compliance (LGPD/GDPR)'
  recommendations:
    must_fix:
      - 'Verify AC-2 warning string contains "PII" and "trace_payload_retention_days"'
      - 'Add symlink-safety to cleanup (SEC-001) — codify as AC-18'
      - '--dry-run byte-identical filesystem snapshot test'
    monitor:
      - 'Cron example must not redirect stderr to /dev/null'
      - 'Cleanup prints summary on zero matches'
      - 'Track directory size monotonically growing (cleanup-not-running indicator)'
```

## Story Hook Line

```text
Risk profile: docs/qa/assessments/TEA-OBS-003.2-risk-20260501.md
```
