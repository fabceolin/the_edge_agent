# Requirements Traceability Matrix

## Story: TEA-OBS-003.3 — Async exporter + optional gzip

- **Date:** 2026-05-01
- **Tracer:** Quinn (Test Architect)
- **Mode:** YOLO (non-interactive)
- **Story status at trace time:** Draft (no implementation, no tests in `python/tests/`)
- **Sources cross-referenced:**
  - Test Design: [`TEA-OBS-003.3-test-design-20260501.md`](TEA-OBS-003.3-test-design-20260501.md)
  - Risk Profile: [`TEA-OBS-003.3-risk-20260501.md`](TEA-OBS-003.3-risk-20260501.md)
  - NFR Assessment: [`TEA-OBS-003.3-nfr-20260501.md`](TEA-OBS-003.3-nfr-20260501.md)

> **Trace mode:** Forward-looking. Because the story is Draft, no test files exist yet under `python/tests/`. Coverage in this matrix reflects **planned** coverage from the test-design document — i.e. whether each AC has at least one designed test scenario that fully validates it. A second pass after implementation should re-trace test IDs to actual test files and update coverage from `planned-full` to `full`.

---

## Coverage Summary

| Metric | Count | % |
|---|---|---|
| Total Acceptance Criteria | 19 | 100% |
| Planned-Full (≥1 designed test fully validates) | 18 | 94.7% |
| Planned-Partial (designed tests cover most but not all aspects) | 1 | 5.3% |
| Not Covered (no designed test) | 0 | 0% |
| **Implemented today** | **0** | **0%** |

**Designed test scenarios in test-design doc:** 38 total (6 unit · 30 integration · 2 doc) — P0: 14 · P1: 13 · P2: 11.

**Coverage of risks (Risk Profile):** Every High and Medium risk is backed by ≥1 P0 designed test. OPS-001 (introspection metrics) is the lone partial — covered only by INT-034, which is conditional on proposed AC-20 being folded into scope.

---

## Requirement Mappings

### Part A — Async exporter

#### AC-1: `AsyncFileExporter(path, queue_size=1000, flush_interval_s=5)` exists in `tracing.py`

**Coverage: PLANNED-FULL**

Given-When-Then mappings:

- **Unit Test:** `UNIT-001` — constructor signature & defaults
  - Given: `AsyncFileExporter` class imported
  - When: Instance created with no kwargs vs. all kwargs
  - Then: Default `queue_size == 1000`, `flush_interval_s == 5`; explicit overrides accepted

- **Unit Test:** `UNIT-002` — constructor input bounds
  - Given: Invalid args (`queue_size=0`, `queue_size=-1`, `flush_interval_s=0`)
  - When: Constructor invoked
  - Then: Raises `ValueError` with descriptive message (also addresses SEC-003)

#### AC-2: Implements `TraceExporter` Protocol (`export(span) -> None`)

**Coverage: PLANNED-FULL**

- **Unit Test:** `UNIT-003` — Protocol conformance
  - Given: `AsyncFileExporter` instance
  - When: `isinstance(exp, TraceExporter)` checked, `export()` called with a Span
  - Then: Protocol check passes; `export()` returns `None`

#### AC-3: `export()` returns immediately (target <1 ms)

**Coverage: PLANNED-FULL**

- **Unit Test:** `UNIT-004` — p99 export latency
  - Given: Constructed `AsyncFileExporter` with worker stalled (slow-disk fixture, 50 ms/write)
  - When: 1000 `export()` calls measured
  - Then: p99 wall-clock < 1 ms (caller path is enqueue-only; never touches disk)

#### AC-4: Background thread reads queue and writes in batches (time **or** size trigger; one open/write/close per flush)

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-001` — size trigger drives a single flush
  - Given: `AsyncFileExporter` with `flush_interval_s=60` (effectively disabled)
  - When: 50 spans exported in tight loop
  - Then: `mock_open.call_count == 1`; one batched open/write/close cycle; all 50 spans on disk

- **Integration Test:** `INT-002` — time trigger drives a flush
  - Given: `AsyncFileExporter` with `flush_interval_s=0.1` and small batch
  - When: < 50 spans exported, then time advances
  - Then: Flush observed within ~0.1 s; all spans on disk

- **Integration Test:** `INT-003` — size threshold flushes mid-window
  - Given: `flush_interval_s=60`, `batch_threshold=50`
  - When: 50 spans exported
  - Then: Flush triggered immediately on threshold, not on timer

- **Integration Test:** `INT-004` — "whichever comes first" arbitration
  - Given: Mixed bursts vs. trickles
  - When: Both triggers race
  - Then: Earlier trigger wins; no double-flush; no missed flush

#### AC-5: Drain on shutdown with configurable timeout (default 30 s); WARN log on drops

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-005` — happy-path drain
  - Given: 200 spans enqueued, `close()` called with default timeout
  - When: Worker completes normally
  - Then: All 200 spans on disk; thread joined cleanly; no WARN

- **Integration Test:** `INT-006` — slow-disk timeout exhaustion
  - Given: 50 ms/write fixture; 200 spans; `shutdown_timeout=0.5`
  - When: `close()` called
  - Then: Returns within ~0.5 s; WARN log emitted with structured fields (dropped count, final queue depth, drain duration); already-flushed batches valid (mitigates DATA-001/TECH-004)

- **Integration Test:** `INT-007` — configurable timeout honored
  - Given: Custom `shutdown_timeout=2.0` and slow worker
  - When: Drain forced
  - Then: Timeout respected; WARN includes the configured value

#### AC-6: Bounded queue with `overflow_policy: drop_newest` (default) | `block`

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-008` — `drop_newest` drops new spans
  - Given: `queue_size=10`, worker stalled, 50 spans exported
  - When: Queue saturates
  - Then: 10 buffered, 40 dropped, drop counter == 40, `export()` never raises

- **Integration Test:** `INT-009` — WARN log throttled (≤1/min)
  - Given: Sustained drops over simulated 5 minutes
  - When: Drops occur every flush window
  - Then: WARN log emitted at most ~5 times (once per minute), counter still increments per drop

- **Integration Test:** `INT-010` — `block` policy backpressures producer
  - Given: `overflow_policy=block`, queue full
  - When: Producer calls `export()`
  - Then: Call blocks until worker drains a slot, unblocks within next flush cycle (mitigates SEC-002 — bounded by flush cadence)

#### AC-7: YAML setting `trace_payload_async: true` (default `false`) wires `AsyncFileExporter`

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-011` — `trace_payload_async: true` produces async exporter
  - Given: YAML with `trace_payload_async: true`
  - When: Engine starts, span emitted
  - Then: Exporter type is `AsyncFileExporter`; span eventually flushed asynchronously

- **Integration Test:** `INT-012` — default config produces sync exporter (also AC-14)
  - Given: YAML without flag
  - When: Engine starts
  - Then: Exporter is sync `FileExporter`; behavior matches TEA-OBS-003.1

---

### Part B — Gzip compression

#### AC-8: YAML setting `trace_payload_compress: gzip` (other values reserved)

**Coverage: PLANNED-FULL**

- **Unit Test:** `UNIT-005` — codec parsing accepts `gzip`
  - Given: Settings parser
  - When: `trace_payload_compress: gzip` provided
  - Then: Codec resolved to gzip implementation

- **Unit Test:** `UNIT-006` — unknown codec rejected
  - Given: `trace_payload_compress: zstd` (or any unsupported value)
  - When: Settings parser invoked
  - Then: Startup ERROR raised with descriptive message

#### AC-9: Output extension becomes `.llm.jsonl.gz`; writes go through `gzip.open(..., 'at')`

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-013` — extension flips to `.gz`
  - Given: `compress=gzip` enabled
  - When: Workflow runs
  - Then: Output file matches `*.llm.jsonl.gz`; no `*.llm.jsonl` produced

- **Integration Test:** `INT-014` — interop with `gzip.open` and system `gunzip`
  - Given: `compress=gzip`, N spans flushed
  - When: File decompressed via Python `gzip.open` and shell `gunzip`
  - Then: Both readers produce identical newline-delimited JSON; line count == N

#### AC-10: Gzip writes flush at batch boundaries (per-batch open/write/close, never per-span append)

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-015` — open/close count matches flush count
  - Given: N forced flushes
  - When: Workflow runs to completion
  - Then: File opened/closed exactly N times; per-flush span count > 1 (mitigates DATA-002 gzip-stream-framing)

- **Integration Test:** `INT-016` — multi-batch gzip remains valid
  - Given: 5 batches × 20 spans, gzip enabled
  - When: File round-tripped through `gzip.open`
  - Then: Decompresses to exactly 100 newline-delimited JSON records, in order

#### AC-11: Sync gzip works without `trace_payload_async`; latency cost documented

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-017` — sync gzip happy path
  - Given: `compress=gzip`, `async=false`
  - When: 50 spans exported
  - Then: Single valid `.jsonl.gz` produced; all spans readable

- **Integration Test:** `INT-018` — sync gzip latency benchmark (TECH-002 mitigation)
  - Given: 50 ms/write fixture, sync gzip vs sync text
  - When: 100 spans exported each
  - Then: Multiplier captured in test output and documented in observability docs (proposed AC-24)

#### AC-12: `tea trace cat <file.jsonl.gz>` decompresses to stdout

**Coverage: PLANNED-PARTIAL** *(see gap notes)*

- **Integration Test:** `INT-019` — gzip cat happy path
  - Given: A valid `.llm.jsonl.gz` file
  - When: `tea trace cat <path>` invoked
  - Then: Stdout contains decompressed JSON, one line per span; exit 0

- **Integration Test:** `INT-020` — plain `.jsonl` cat
  - Given: A non-compressed `.llm.jsonl` file
  - When: `tea trace cat <path>` invoked (extension auto-detect)
  - Then: Stdout contains the file verbatim; exit 0

- **Integration Test:** `INT-021` — corrupt-gzip handling
  - Given: A truncated/corrupt `.gz` file
  - When: `tea trace cat <path>` invoked
  - Then: Non-zero exit, stderr error message, no stack trace leaks (proposed AC-spec for stderr/exit-code conventions still open per NFR-Maintainability gap #9)

#### AC-13 (Part C): `tea trace cleanup` matches both `*.jsonl` and `*.jsonl.gz`

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-022` — cleanup glob includes `.gz`
  - Given: Mixed `.jsonl` and `.jsonl.gz` files older than retention
  - When: `tea trace cleanup` invoked
  - Then: Both file types removed identically; retention rules applied uniformly

---

### Part D — Behavioral guarantees

#### AC-14: Default behavior is byte-identical to TEA-OBS-003.1 sync writer

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-023` — checksum equivalence
  - Given: Same workflow run under TEA-OBS-003.1 baseline and TEA-OBS-003.3 default config
  - When: Output `.llm.jsonl` files compared
  - Then: SHA-256 checksums match byte-for-byte (caps blast radius — mitigates DATA-001 regression risk)

#### AC-15: `async=true` + `compress=gzip` produces async-flushed `.jsonl.gz`

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-024` — combined config produces correct artifact
  - Given: Both flags enabled, 100 spans
  - When: Workflow runs and shuts down cleanly
  - Then: Exactly one `.jsonl.gz` file produced; decompresses to 100 valid JSON lines

- **Integration Test:** `INT-025` — round-trip integrity under combined config
  - Given: 100 enqueued spans with stable ids and ordering
  - When: Async + gzip path used, graceful shutdown
  - Then: 100 read-back == 100 enqueued; intra-batch order preserved

#### AC-16: Background-thread failure is non-fatal; ERROR logged; subsequent enqueues land in queue and eventually trigger overflow policy

**Coverage: PLANNED-FULL** *(but with a known semantic gap flagged in NFR — see gaps)*

- **Integration Test:** `INT-026` — `OSError(ENOSPC)` mid-flush
  - Given: Disk-full injected on flush
  - When: Workflow continues exporting
  - Then: Workflow completes; one ERROR log (not per-span); no exception escapes

- **Integration Test:** `INT-027` — permission revoked mid-run
  - Given: `OSError(EACCES)` injected
  - When: Subsequent `export()` calls made
  - Then: Spans still enqueue; no crash; same ERROR semantics

- **Integration Test:** `INT-028` — terminal-state overflow after worker death
  - Given: Worker dead, queue accumulating
  - When: Queue saturates
  - Then: Overflow policy kicks in (drops counted, WARN throttled); no crash

---

### Part E — Quality

#### AC-17: Tests cover async happy path, queue overflow (both policies), shutdown drain, sync gzip, async gzip, background-thread failure recovery

**Coverage: PLANNED-FULL** *(meta-AC — satisfied by the union of tests above)*

Mapping by topic:
- Async happy path → INT-001, INT-005, INT-024, INT-025
- Queue overflow `drop_newest` → INT-008, INT-009
- Queue overflow `block` → INT-010
- Shutdown drain → INT-005, INT-006, INT-007
- Sync gzip → INT-017, INT-018
- Async gzip → INT-024, INT-025, INT-014, INT-015, INT-016
- Background-thread failure → INT-026, INT-027, INT-028

#### AC-18: Benchmark — async ≥ 5× faster on 50 ms-per-write fixture; documented

**Coverage: PLANNED-FULL**

- **Integration Test:** `INT-029` — speedup benchmark
  - Given: 100-LLM-call workflow on 50 ms/write fixture
  - When: Run sync vs async
  - Then: Async ≥ 5× faster; ratio captured in test output

- **Doc Test:** `DOC-001` — benchmark documented
  - Given: `INT-029` results
  - When: Docs reviewed
  - Then: Actual measured speedup recorded in `docs/python/observability.md`

#### AC-19: Documentation in `docs/python/observability.md` covers when to enable async/gzip, trade-offs, worked example

**Coverage: PLANNED-FULL**

- **Doc Test:** `DOC-002` — observability docs include all required sections
  - Given: `docs/python/observability.md` after the change
  - When: Reviewed
  - Then: Includes "when to enable async", "when to enable gzip", trade-offs (async = potential data-loss on hard crash, mitigates DATA-001), and a combined async+gzip worked example

---

### Cross-cutting (not tied to a single AC)

| Test ID | Purpose | Risk addressed |
|---|---|---|
| `INT-030` | 10 concurrent producers × 100 `export()` each → `delivered + dropped == 1000`, no duplicate span-ids | TECH-001 thread sync |
| `INT-031` | Subprocess + `SIGKILL` between flushes — pre-crash batches valid JSONL/gzip | DATA-001 hard-crash semantics |
| `INT-032` | Fork-after-construction (gunicorn pre-fork) — at minimum no deadlock; ideally lazy-start works | TECH-003 |
| `INT-033` | Idempotent `close()` — safe to call twice | NFR-Reliability gap #10 |
| `INT-034` | Counter attributes (`drops`, `queue_depth`, `flush_count`, `last_flush_ts`, `worker_alive`) increment correctly | OPS-001 — **conditional**, see gaps |

---

## Critical Gaps

> **Reading note:** every AC-level requirement has a designed test. The gaps below are coverage holes against the **risk register** and **NFR findings**, not against the literal AC text.

### Gap 1 — OPS-001 introspection metrics (Medium severity)

- **Requirement context:** Risk Profile flagged OPS-001 (operators have no programmatic signal to tune `queue_size` or detect storage issues). NFR Maintainability is **CONCERNS** primarily because of this.
- **Coverage status:** INT-034 is the only backing test, and it is **conditional on proposed AC-20 being folded into scope**. If AC-20 is deferred, OPS-001 has effectively zero test coverage.
- **Risk:** Operators cannot tune `queue_size` without log scraping; post-mortems lose introspection signal.
- **Suggested test (if AC-20 is folded in):** make INT-034 P1 unconditional, asserting all five attributes increment correctly under TC-2 (overflow drops) and TC-3 (slow-disk drain).
- **Suggested test (if AC-20 is deferred):** mark INT-034 `xfail-strict` with the deferral story id so it converts to a real failure once AC-20 lands.

### Gap 2 — AC-12 `tea trace cat` UX semantics underspecified (Low severity)

- **Requirement context:** Story spec for AC-12 covers gzip decompression to stdout but not extension auto-detect, partial-file behavior, or stderr/exit-code conventions (NFR Maintainability gap #9).
- **Coverage status:** PLANNED-PARTIAL — INT-019/020/021 designed scenarios cover happy path + plain `.jsonl` + corrupt `.gz`, but assertion text for stderr message format and exit code is not pinned in either AC or test design.
- **Suggested action:** before INT-021 is implemented, story owner should confirm: corrupt → exit code (e.g., 2), stderr line format, no stack-trace leakage. Lock these in test fixtures so the contract does not drift.

### Gap 3 — AC-16 worker-death semantics need tightening (Low severity)

- **Requirement context:** AC-16 says "spans accumulate without being written" after worker death — implies a terminal state with no retry/escalation (NFR Reliability gap #2).
- **Coverage status:** INT-026/027/028 cover the happy non-fatal path and terminal-state overflow, but no test exercises a `worker_alive` health flag because the AC does not require one.
- **Suggested action:** confirm whether `worker_alive` becomes part of AC-20 (proposed). If yes, fold it into INT-034. If no, accept the gap explicitly; operators will detect worker death only via overflow drops.

### Gap 4 — Shutdown WARN log structure not asserted (Low severity)

- **Requirement context:** AC-5 requires "a warning if any are dropped" but does not pin the log line's structured fields (NFR Reliability gap #4). NFR proposes AC-22 to require dropped count, queue depth, drain duration, and total flush count.
- **Coverage status:** INT-006 expected result *does* include "WARN log line includes dropped count + final queue depth", but only because the test author chose to add it. The AC text does not require the assertion to exist.
- **Suggested action:** fold proposed AC-22 in, or accept that INT-006 enforces a stricter contract than the AC requires.

### Gap 5 — `tea trace cat` Windows behavior (Minimal severity)

- **Requirement context:** INT-031 (`SIGKILL` harness) is POSIX-only; AC-12 implies cross-platform CLI.
- **Coverage status:** No designed test exercises `tea trace cat` on Windows.
- **Suggested action:** decide whether `tea trace cat` is supported on Windows. If yes, add a smoke test on Windows CI lane. If no, document explicitly and reject Windows in CLI startup.

---

## Test Design Recommendations

These complement the test-design document and address the gaps above.

1. **Decide AC-20 (introspection metrics) before code lands.** The OPS-001 coverage gap is the only substantive hole in the matrix. Either fold AC-20 in (preferred — closes both NFR Maintainability CONCERNS and the gap), or formally defer with `xfail-strict` markers and a follow-up story id.
2. **Pin the `tea trace cat` exit-code/stderr contract** in fixtures. Even if AC-12 stays minimal, INT-021 needs deterministic assertions to remain stable.
3. **Adopt the "no `time.sleep()` in assertion paths" rule** universally for the concurrency tests (INT-005, INT-006, INT-008, INT-010, INT-024, INT-025, INT-030, INT-031). This is the proposed AC-23 from the NFR review and mitigates OPS-003 flakiness.
4. **Make INT-029 a CI gate.** If the ≥5× speedup regresses, the entire opt-in story loses its justification — the test should fail the build, not just the report.
5. **Add a minimal `worker_alive` smoke test** if proposed AC-20 is folded in (AC-25 in NFR proposed list addresses lazy-start and at-fork).
6. **Keep the SIGKILL test (INT-031) hermetic.** Use `inotify`/`os.fstat`-based ready-signal rather than `time.sleep` to avoid CI flakiness when killing the child.

---

## Risk Assessment

| Risk class | Coverage status |
|---|---|
| **High-risk** ACs (none with zero coverage) | All ACs have at least planned-full or planned-partial coverage. |
| **Medium-risk** gaps | OPS-001 (introspection) — conditional on AC-20 fold-in. |
| **Low-risk** gaps | AC-12 stderr/exit-code contract, AC-16 worker-death health flag, AC-5 WARN log structure, Windows `tea trace cat`. |

**Overall trace risk:** **MEDIUM** — driven entirely by AC-20 still being a *proposal*. If AC-20 is folded into scope, trace risk drops to LOW. If AC-20 is deferred, trace risk stays MEDIUM until a follow-up story closes the introspection gap.

---

## Gate YAML Block (paste into qa-gate)

```yaml
trace:
  totals:
    requirements: 19
    full: 18
    partial: 1
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-OBS-003.3-test-design-20260501.md'
  uncovered:
    - ac: 'AC-12'
      reason: 'tea trace cat exit-code and stderr contract not pinned in story or test design (planned-partial via INT-019/020/021).'
  notes: 'See docs/qa/assessments/TEA-OBS-003.3-trace-20260501.md. OPS-001 introspection coverage (INT-034) is conditional on proposed AC-20.'
  story_status_caveat: 'Story is in Draft; matrix reflects PLANNED coverage from test-design only. Re-trace after implementation to lift planned-* to actual full/partial.'
```

---

## Story Hook Line (for review task)

```text
Trace matrix: docs/qa/assessments/TEA-OBS-003.3-trace-20260501.md
```

---

## Key Principles Honored

- Every AC is mapped to at least one designed test; no AC has zero designed coverage.
- Given-When-Then describes WHAT each test validates, not how it is coded.
- Both presence (planned coverage) and absence (gaps vs. risk and NFR) are explicit.
- Recommendations are actionable: each is a concrete decision (fold AC-20, pin INT-021 contract, adopt no-sleep rule, gate INT-029) rather than a vague aspiration.
- The Draft-status caveat is called out repeatedly — readers will not mistake this for an as-implemented trace.
