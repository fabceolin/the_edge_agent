# Requirements Traceability Matrix

## Story: TEA-OBS-003.2 — Payload retention policy & cleanup

**Date:** 2026-05-01
**Tracer:** Quinn (Test Architect)
**Mode:** YOLO
**Story status at trace time:** Draft (no implementation, no tests on disk yet)
**Inputs:**
- Story: [`docs/stories/TEA-OBS-003.2-payload-retention-cleanup.md`](../../stories/TEA-OBS-003.2-payload-retention-cleanup.md)
- Risk profile: [`TEA-OBS-003.2-risk-20260501.md`](TEA-OBS-003.2-risk-20260501.md)
- NFR assessment: [`TEA-OBS-003.2-nfr-20260501.md`](TEA-OBS-003.2-nfr-20260501.md)
- Test design: [`TEA-OBS-003.2-test-design-20260501.md`](TEA-OBS-003.2-test-design-20260501.md)

> **Forward traceability:** Story is in Draft; `python/tests/test_trace_cleanup.py` does not yet exist. This matrix maps every AC to **planned** test scenarios from the test-design document. `python/tests/test_yaml_engine_observability.py` already exists and will receive AC-2/AC-3/AC-19 unit tests as additions.

---

## Coverage Summary

| Bucket                             | Count | %     |
| ---------------------------------- | ----- | ----- |
| Total testable requirements        | **20**| 100%  |
| Fully covered (planned)            | 20    | 100%  |
| Partially covered                  | 0     | 0%    |
| Not covered                        | 0     | 0%    |

**Requirement decomposition:** 17 codified ACs (AC-1 … AC-17) + 3 NFR/risk-derived recommended ACs (AC-18 symlink safety, AC-19 warning substring assertions, `--older-than 0` rejection) — all currently mapped to designed tests.

**Coverage caveat:** "Full" here means *every AC has at least one designed test scenario at a level appropriate to the AC's risk class* (per test-design priority assignment). It does **not** mean the tests exist on disk. Story is Draft.

---

## Requirement Mappings

### AC-1 — `trace_payload_retention_days` setting parsed

**Coverage: FULL (unit, P0)**

- **Unit Test (planned)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-001`
  - **Given:** YAML with `auto_trace_llm_payloads: true, trace_payload_retention_days: 30`
  - **When:** `YAMLEngine` is constructed and `_configure_from_settings` runs
  - **Then:** Engine attribute reflects `30` (int); when the setting is omitted, attribute is `None`

---

### AC-2 — Warning emitted when capture on + retention unset (text per spec)

**Coverage: FULL (unit, P0/P1)**

- **Unit Test (planned)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-002`
  - **Given:** YAML with capture on, retention unset; `caplog.set_level(WARNING, logger="the_edge_agent.yaml_engine")`
  - **When:** Engine init runs
  - **Then:** Exactly **one** WARNING record at the engine logger — emission cardinality and level

- **Unit Test (planned)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-004`
  - **Given:** Same as UNIT-002, repeated engine inits in one process
  - **When:** Engine init runs N times
  - **Then:** Warning emitted **per init** (no global de-dup that would mask future config drift)

> Warning *content* (PII / setting-name / cleanup-command substrings) is gated by **AC-19 / UNIT-003** (see below) — risk-profile mitigation #1 + NFR security recommendation #2.

---

### AC-3 — Warning suppressed when retention is set

**Coverage: FULL (unit, P0/P1)**

- **Unit Test (planned)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-005`
  - **Given:** YAML with capture on + retention = 30
  - **When:** Engine init runs
  - **Then:** Zero retention WARNING records on the engine logger

- **Unit Test (planned)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-006`
  - **Given:** Capture **off**, retention set to any value (or unset)
  - **When:** Engine init runs
  - **Then:** Zero retention warnings (warning is conditional on capture being enabled)

---

### AC-4 — Retention setting alone never triggers automatic deletion

**Coverage: FULL (integration, P0)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-001`
  - **Given:** Trace dir with a 100-day-old `*.llm.jsonl`; YAML with capture on + retention set; engine constructed
  - **When:** A workflow runs and engine close is invoked
  - **Then:** File is still on disk byte-for-byte; engine never calls `unlink()` itself

---

### AC-5 — `tea trace cleanup` subcommand exists with expected flags

**Coverage: FULL (integration, P0)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-002`
  - **Given:** Project installed with the CLI entrypoint
  - **When:** `tea trace cleanup --help` is invoked via `CliRunner`
  - **Then:** Exit 0; help text contains `--older-than`, `--dry-run`, `--pattern`, `--recursive`, `[<dir>]`

---

### AC-6 — Default `<dir>` resolves from configured `trace_file` or falls back to cwd

**Coverage: FULL (integration, P1/P2)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-003`
  - **Given:** YAML with `trace_file: /tmp/x/run.llm.jsonl` and matching old fixtures in `/tmp/x/`
  - **When:** `tea trace cleanup --config <yaml> --older-than 30` is invoked (no `<dir>` arg)
  - **Then:** Cleanup operates on `/tmp/x/`; cwd untouched

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-004`
  - **Given:** No `--config`, no `<dir>` arg, with `monkeypatch.chdir(tmp_path)`
  - **When:** Cleanup runs with `--older-than 30`
  - **Then:** Operates on cwd (`tmp_path`); does not error

---

### AC-7 — `--older-than` overrides YAML; error when neither is set

**Coverage: FULL (integration, P0/P1)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-005`
  - **Given:** YAML retention = 30; CLI passes `--older-than 7`
  - **When:** Cleanup runs against a 10-day-old fixture
  - **Then:** File is deleted (CLI value wins, 10 > 7)

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-006`
  - **Given:** No CLI `--older-than`, no YAML retention
  - **When:** Cleanup runs
  - **Then:** Exit ≠ 0; stderr names both `--older-than` and `trace_payload_retention_days` so the user knows both knobs

---

### AC-8 — Default `--pattern` matches `*.llm.jsonl` and `*.llm.jsonl.gz`

**Coverage: FULL (unit + integration, P0/P1/P2)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-007`
  - **Given:** Mixed-extension fixtures (`*.llm.jsonl`, `*.llm.jsonl.gz`, `.json`, `.txt`, `.bak`), all 100 days old
  - **When:** `tea trace cleanup --older-than 30` runs (default pattern)
  - **Then:** Only `*.llm.jsonl` and `*.llm.jsonl.gz` deleted; `.json/.txt/.bak` survive

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-008`
  - **Given:** A `*.llm.jsonl.gz` fixture (forward-compat with TEA-OBS-003.3)
  - **When:** Default-pattern cleanup runs
  - **Then:** `.gz` file is selected (regression guard for TECH-001)

- **Unit Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-UNIT-007`
  - **Given:** Pattern-matching helper called with each extension above
  - **When:** Helper invoked directly
  - **Then:** True only for the two TEA-specific patterns

---

### AC-9 — `--dry-run` lists candidates without deleting

**Coverage: FULL (integration, P0/P1)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-009`
  - **Given:** 3 matching candidates and a byte-identity snapshot helper `_fs_snapshot(root) -> {path: (size, mtime, sha256)}`
  - **When:** Cleanup runs with `--dry-run`
  - **Then:** Filesystem **byte-identical** before vs after (defends DATA-001 / risk-profile mitigation #3)

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-010`
  - **Given:** Same fixtures
  - **When:** `--dry-run` is run
  - **Then:** Stdout lists each candidate path with size and mtime, one per line

---

### AC-10 — Real cleanup deletes; summary `Deleted N files, freed X MB`

**Coverage: FULL with amendment (integration, P0)** — *one mapped test is a forcing-function for the recommended zero-match amendment*

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-011`
  - **Given:** 3 matching files (100/60/40 days old, total ~9 MB) and `--older-than 30`
  - **When:** Cleanup runs without `--dry-run`
  - **Then:** All 3 deleted; exit 0; stdout matches `^Deleted 3 files, freed 9\.\d+ MB$`

- **Integration Test (planned, forcing AC amendment)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-012`
  - **Given:** Empty directory or no candidates over the cutoff
  - **When:** Cleanup runs
  - **Then:** Exit 0; stdout `Deleted 0 files, freed 0 MB`
  - **Note:** **Currently AC-10 does not specify zero-match output.** Test fails unless the NFR-recommended amendment is adopted; without it, cron has no stdout on success and silent failure (OPS-002) is undetectable.

---

### AC-11 — Cleanup non-recursive by default; `--recursive` enables descent

**Coverage: FULL (integration, P1)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-013`
  - **Given:** `tmp_path/old.llm.jsonl` (100d) and `tmp_path/sub/older.llm.jsonl` (100d)
  - **When (a):** Cleanup without `--recursive`
  - **Then (a):** Only top-level file deleted; `sub/` untouched
  - **When (b):** Same fixture, cleanup with `--recursive`
  - **Then (b):** Both files deleted

---

### AC-12 — Exit 0 on success (incl. zero matches)

**Coverage: FULL (integration, P0)**

Covered by INT-011 (3 deletions → exit 0) and INT-012 (zero matches → exit 0). No additional tests needed.

---

### AC-13 — Exit 1 on any deletion error; continue processing

**Coverage: FULL (integration, P1/P2)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-014`
  - **Given:** 3 matching files in a `0o555` (read+execute, no write) directory on POSIX; `unlink()` will fail for each
  - **When:** Cleanup runs
  - **Then:** Exit 1; stderr emits 3 `ERROR: failed to delete <path>: <reason>` lines; final summary `Failed to delete 3 of 3 files. Exit 1.` printed
  - **Skip:** Windows + `os.geteuid() == 0`

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-015`
  - **Given:** Mixed run: 2 deletable, 1 non-deletable
  - **When:** Cleanup runs
  - **Then:** 2 succeed, 1 reported on stderr; exit 1; deletable files don't depend on the failed file

---

### AC-14 — `--help` contains the literal example

**Coverage: FULL (integration, P2)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-016`
  - **Given:** CLI installed
  - **When:** `tea trace cleanup --help` is invoked
  - **Then:** Output contains the literal substring `tea trace cleanup ./traces --older-than 30 --dry-run`

---

### AC-15 — `docs/python/observability.md` documents warning, cron, systemd, TTL guidance

**Coverage: FULL (integration / doc-grep, P0/P1)** — *one test forces the cron-stderr sub-bullet*

- **Integration Test (planned, forcing AC sub-bullet)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-017`
  - **Given:** Repo file `docs/python/observability.md`
  - **When:** Test reads cron-fenced blocks and greps for stderr-redirect patterns
  - **Then:** Doc contains a `tea trace cleanup` cron line **and** does NOT contain any of `2>/dev/null`, `2>&1 >/dev/null`, `> /dev/null 2>&1`, `&>/dev/null` (cron silent-failure prevention; OPS-002 mitigation #4)

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-018`
  - **Given:** Same doc
  - **When:** Test searches for the systemd alternative
  - **Then:** Doc contains a `[Timer]` section example for cleanup

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-019`
  - **Given:** Same doc
  - **When:** Test reads TTL guidance prose
  - **Then:** Leads with `30 days for general use; 7 days for PII-heavy domains` (verbatim)

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-020`
  - **Given:** Same doc + the live `PII_RETENTION_WARNING` constant from `yaml_engine.py`
  - **When:** Test compares verbatim
  - **Then:** Warning text reproduced exactly in observability.md (parity test — drift breaker)

---

### AC-16 — Tests cover all listed AC paths

**Coverage: FULL (collective, P0–P2)**

This is a meta-AC; satisfied by aggregate coverage of UNIT-001…UNIT-008 and INT-001…INT-023 across this trace. Specifically:
- Warning emitted: UNIT-002
- Warning suppressed: UNIT-005
- `--older-than` selection: INT-021
- `--dry-run` deletes nothing: INT-009
- `--pattern` filters correctly: INT-007
- `--recursive` traverses: INT-013
- Permission errors don't abort: INT-014, INT-015

---

### AC-17 — Test fixture controls mtimes via `os.utime`; deletion boundary asserted

**Coverage: FULL (integration, P0)**

- **Integration Test (planned)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-021`
  - **Given:** 3 files at mtime 10 / 30 / 100 days old via `os.utime(path, (epoch, epoch))`
  - **When:** `tea trace cleanup --older-than 30` runs
  - **Then:** 100-day file deleted; 10-day file preserved; **boundary at exactly 30 days pinned** to whichever inclusive/exclusive choice the implementation makes (documented in test docstring so the contract is explicit, not accidental)

---

### AC-18 — Symlink safety **(NEW, recommended; not yet codified)**

**Coverage: FULL (integration, P0) — gating on AC adoption**

> Source: risk-profile mitigation #2 (SEC-001) + NFR security recommendation #1. **Forcing-function tests** below fail unless the AC is adopted into the story.

- **Integration Test (planned, forcing)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-022`
  - **Given:** `tmp_path/link.llm.jsonl` symlinks to `other_tmp_path/protected.llm.jsonl`; both 100 days old
  - **When:** Cleanup runs over `tmp_path` with `--older-than 30`
  - **Then:** `other_tmp_path/protected.llm.jsonl` **must still exist** (cleanup skips symlinks at `unlink()` time)
  - **Skip:** Windows

- **Integration Test (planned, forcing)** — `test_trace_cleanup.py::TEA-OBS-003.2-INT-023`
  - **Given:** A symlinked subdirectory under `tmp_path/` pointing to a tree of `*.llm.jsonl` files
  - **When:** Cleanup runs with `--recursive`
  - **Then:** Recursion does NOT descend through the symlink; target tree intact
  - **Skip:** Windows

---

### AC-19 — Warning substring assertions **(NEW, recommended; not yet codified)**

**Coverage: FULL (unit, P0) — gating on AC adoption**

> Source: risk-profile mitigation #1 (BUS-001/OPS-001) + NFR security recommendation #2. Without this AC, AC-2 quotes a warning string but no test asserts the runtime record contains the operator-discoverable substrings.

- **Unit Test (planned, forcing)** — `test_yaml_engine_observability.py::TEA-OBS-003.2-UNIT-003`
  - **Given:** The single WARNING record captured in UNIT-002
  - **When:** Test inspects `record.getMessage()`
  - **Then:** Message contains all literal substrings: `"PII"`, `"trace_payload_retention_days"`, `"tea trace cleanup"` so operators searching production logs find it
  - **Drift breaker:** The matching constant `PII_RETENTION_WARNING` is recommended to be exported from `yaml_engine.py` and imported by both the runtime emit-site and AC-15 doc parity test (INT-020).

---

### `--older-than 0` rejection **(NEW, recommended; not yet codified)**

**Coverage: FULL (unit, P1) — gating on AC adoption**

> Source: risk-profile mitigation #6 (OPS-003). Prevents racing with an active writer.

- **Unit Test (planned, forcing)** — `test_trace_cleanup.py::TEA-OBS-003.2-UNIT-008`
  - **Given:** CLI invocation with `--older-than 0`
  - **When:** Cleanup parses arguments
  - **Then:** Exit ≠ 0 with a clear error explaining that 0 would race with the active writer; cleanup never runs

---

## Coverage Heatmap (planned)

| AC          | Unit | Integration | E2E | Priority | Notes                                                              |
| ----------- | :--: | :---------: | :-: | :------: | ------------------------------------------------------------------ |
| AC-1        | ✅   |             |     | P0       | Setting plumbed                                                    |
| AC-2        | ✅   |             |     | P0/P1    | Warning emission cardinality                                       |
| AC-3        | ✅   |             |     | P0/P1    | Warning suppression                                                |
| AC-4        |      | ✅          |     | P0       | No auto-deletion at engine init/shutdown                           |
| AC-5        |      | ✅          |     | P0       | CLI surface                                                        |
| AC-6        |      | ✅          |     | P1/P2    | `<dir>` resolution                                                 |
| AC-7        |      | ✅          |     | P0/P1    | `--older-than` precedence + missing-config error                   |
| AC-8        | ✅   | ✅          |     | P0/P1/P2 | Default pattern + `.gz` forward-compat                             |
| AC-9        |      | ✅          |     | P0/P1    | `--dry-run` byte-identity                                          |
| AC-10       |      | ✅          |     | P0       | Summary format + zero-match (forced amendment)                     |
| AC-11       |      | ✅          |     | P1       | `--recursive`                                                      |
| AC-12       |      | ✅          |     | P0       | Exit 0 paths                                                       |
| AC-13       |      | ✅          |     | P1/P2    | Per-file failure + exit 1 aggregate                                |
| AC-14       |      | ✅          |     | P2       | `--help` contains example                                          |
| AC-15       |      | ✅          | ✅  | P0/P1    | Doc-grep tests; cron-stderr forcing                                |
| AC-16       | (collective)               | various  | Meta                                                               |
| AC-17       |      | ✅          |     | P0       | mtime boundary pinned                                              |
| **AC-18**   |      | ✅          |     | **P0**   | **Forcing test for symlink safety**                                |
| **AC-19**   | ✅   |             |     | **P0**   | **Forcing test for warning substrings**                            |
| **`--older-than 0`** | ✅ |          |     | **P1**   | **Forcing test for race prevention**                               |

**Levels:** 13 unit / 14 integration / 1 E2E smoke.
**Priority:** P0 = 11, P1 = 11, P2 = 5, P3 = 1.

---

## Gaps Identified

**No AC-level coverage gaps.** Every AC — including the three NFR/risk-derived recommended ACs — has at least one designed P0/P1 test.

**Process gap (gating, not coverage):**

1. **Story is Draft and 5 forcing-function tests target ACs that are not yet in the story body.** UNIT-003, UNIT-008, INT-012, INT-017, INT-022/023 are designed to fail unless the story adopts AC-18, AC-19, the AC-10 zero-match amendment, the AC-15 cron-stderr sub-bullet, and `--older-than 0` rejection. If the story is implemented without adopting these ACs first, these five tests will be filed but will permanently fail — which would be misleading. **Resolution: codify the recommended ACs in the story before implementation begins.**

**No coverage-level gaps to flag at this time.** The matrix is fully covered conditional on the story adopting the recommended ACs.

---

## Test Design Recommendations

The test-design document already specifies fixtures, skip markers, pinned constants, file locations, and execution order. Re-summarizing here only the items load-bearing for the gate:

1. **Pin code/test contract constants** in implementation:
   - `PII_RETENTION_WARNING` exported from `python/src/the_edge_agent/yaml_engine.py` (consumed by UNIT-003 and INT-020)
   - `SUMMARY_FORMAT = "Deleted {n} files, freed {mb:.2f} MB"` exported from the cleanup module (consumed by INT-011/INT-012)
   - `class CleanupResult(TypedDict)` typed return for unit-tested cleanup helpers
2. **Skip markers** standardized:
   - `@pytest.mark.skipif(sys.platform == "win32", ...)` for symlink + chmod tests
   - `@pytest.mark.skipif(os.geteuid() == 0, ...)` to guard against root bypassing `chmod 0o555` (relevant for INT-014)
3. **Fixtures** factored into module-scoped helpers:
   - `make_old_file(path, days_old)` (`os.utime`)
   - `_fs_snapshot(root)` returning `{Path: (size, mtime, sha256)}` (used by INT-009 byte-identity assertion)
4. **Doc-grep tests live next to CLI tests** (`test_trace_cleanup.py`) so the pre-merge run trips the cron-stderr / TTL-guidance / warning-parity gates simultaneously with code tests.
5. **No external dependencies** beyond `typer.testing.CliRunner`, already a project dependency. Estimated CI wall-clock: ~5s.

---

## Risk Assessment

- **High Risk requirements (currently):** None at trace level — all ACs are designed to a P0/P1 test. The two High risks from the risk profile (BUS-001 PII compliance, OPS-001 warning fatigue) are *inherent to the soft-control design*, not coverage gaps.
- **Medium Risk requirements:** None.
- **Low Risk requirements:** All; coverage is full unit + integration where appropriate.
- **Process risk:** The five forcing-function tests will fail at PR time unless AC-18, AC-19, AC-10 amendment, AC-15 sub-bullet, and `--older-than 0` rejection are codified in the story before implementation.

---

## Gate YAML Block

```yaml
# Gate YAML (paste under `trace`):
trace:
  totals:
    requirements: 20
    full: 20
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-OBS-003.2-test-design-20260501.md'
  uncovered: []
  notes: |
    Forward trace against Draft story (no implementation, no tests on disk yet).
    20 testable requirements = 17 codified ACs (AC-1…AC-17) + 3 recommended-but-not-yet-codified ACs
    (AC-18 symlink safety, AC-19 warning substring assertions, --older-than 0 rejection) plus
    the AC-10 zero-match-summary amendment and AC-15 cron-stderr sub-bullet.
    All 20 are mapped to designed P0/P1 tests in the test-design document.
    Five tests (UNIT-003, UNIT-008, INT-012, INT-017, INT-022/023) are forcing-functions
    that fail unless the story body adopts the recommended ACs before merge.
    See: docs/qa/assessments/TEA-OBS-003.2-trace-20260501.md
```

---

## Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-OBS-003.2-trace-20260501.md
```
