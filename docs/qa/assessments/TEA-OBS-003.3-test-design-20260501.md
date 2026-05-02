# Test Design: Story TEA-OBS-003.3 — Async exporter + optional gzip

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO (non-interactive)
Inputs leveraged:
- Story: `docs/stories/TEA-OBS-003.3-async-exporter-gzip.md`
- Risk Profile: `docs/qa/assessments/TEA-OBS-003.3-risk-20260501.md` (Overall risk 54/100, CONCERNS)
- NFR Assessment: `docs/qa/assessments/TEA-OBS-003.3-nfr-20260501.md` (Quality 70/100, CONCERNS)

## Test Strategy Overview

- **Total test scenarios:** 38
- **Unit tests:** 6 (16%)
- **Integration tests:** 30 (79%)
- **Doc/Bench checks:** 2 (5%)
- **Priority distribution:** P0: 14 · P1: 13 · P2: 11
- **No E2E tests:** the feature surface is library + CLI; integration tests at the `TraceContext` / `YAMLEngine` boundary cover end-to-end behavior with full fidelity. Spinning up a workflow runner adds no incremental confidence.

### Distribution rationale

The bulk of risk lives at component-interaction seams (queue ↔ worker thread, exporter ↔ filesystem, YAML ↔ exporter wiring), so integration tests dominate. Unit tests cover only pure-logic surfaces (constructor validation, codec parsing, Protocol conformance, and the `export()` <1 ms latency claim). The two doc-level checks are explicit because AC-18 and AC-19 are gating outputs in their own right.

### Risk-coverage philosophy

Every Priority-1 risk from the Risk Profile (DATA-001, TECH-001, TECH-004, PERF-001, OPS-001, DATA-002) is mapped to ≥2 P0 tests. Concurrency tests follow proposed AC-23: event-driven synchronization (`thread.join`, `queue.empty()`, `Event.wait`) is mandatory; `time.sleep()` is permitted only inside slow-disk fixtures, never in assertion paths.

---

## Test Scenarios by Acceptance Criteria

### AC-1 — `AsyncFileExporter(path, queue_size=1000, flush_interval_s=5)` exists

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-UNIT-001 | Unit | P2 | Constructor accepts `path`, `queue_size`, `flush_interval_s`; defaults are 1000 / 5.0 | Pure signature/default contract | — |
| OBS-003.3-UNIT-002 | Unit | P2 | Constructor rejects `queue_size <= 0` and `flush_interval_s <= 0` with `ValueError` | Input bounds; cheap to assert in isolation | SEC-003 |

### AC-2 — Implements `TraceExporter` Protocol (`export(span) -> None`)

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-UNIT-003 | Unit | P1 | Static-duck-type check: `AsyncFileExporter` satisfies `TraceExporter` Protocol; `export()` returns `None` | Protocol conformance is API-contract surface | — |

### AC-3 — `export()` returns in <1 ms

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-UNIT-004 | Unit | P1 | Microbenchmark: 1000 `export()` calls with worker stalled on a blocking sentinel; assert p99 wall-clock < 1 ms | Latency claim is core to the feature value-prop; isolating worker via blocked sentinel keeps it a unit-style test | DATA-001 (perf justifies opt-in) |

### AC-4 — Background thread reads queue and writes in batches

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-001 | Integration | P0 | After 50 `export()` calls, worker performs **one** `open/write/close` cycle (assert via `mock_open` call_count == 1 or fs-watcher) | Defines correctness of the batch-flush primitive | TECH-001, DATA-002 |
| OBS-003.3-INT-002 | Integration | P1 | With 1 span enqueued and `flush_interval_s=0.1`, span lands on disk within ≤2× the interval | Time-based flush trigger | — |
| OBS-003.3-INT-003 | Integration | P1 | With `flush_interval_s=600` and batch threshold 50, enqueue 50 spans → flush fires immediately | Size-based flush trigger | — |
| OBS-003.3-INT-004 | Integration | P2 | "Whichever comes first": at threshold 50 with 49 enqueued, time elapses → flush fires; conversely 50 enqueued before time → flush fires | Confirms either trigger is sufficient | — |

### AC-5 — Drain on `TraceContext.close()` with configurable timeout

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-005 | Integration | P0 | Enqueue 200 spans, call `close()`; all 200 land on disk; worker thread joined within default 30 s | Core shutdown drain contract | DATA-001, TECH-004 |
| OBS-003.3-INT-006 | Integration | P0 | With 50 ms/write slow-disk fixture and `shutdown_timeout=0.5`, enqueue 200 spans → shutdown returns; WARN log line emitted with dropped count + final queue depth | Drop-on-timeout path; structured log assertions | TECH-004 |
| OBS-003.3-INT-007 | Integration | P2 | `close(timeout=2.0)` honored; `close(timeout=0)` does not block beyond best-effort flush | Configurable timeout contract | — |

### AC-6 — Overflow policy (`drop_newest` default, `block` opt-in)

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-008 | Integration | P0 | `queue_size=10`, worker stalled, enqueue 50 spans with `drop_newest`: 10 buffered, 40 dropped, drop counter == 40, `export()` never raises | Default overflow path | PERF-001, TECH-001 |
| OBS-003.3-INT-009 | Integration | P2 | Continuous overflow over 90 s emits ≤2 WARN log lines (throttled "once per minute") | Log-spam guard | OPS-003 |
| OBS-003.3-INT-010 | Integration | P0 | `overflow_policy=block` + queue full: `export()` blocks until worker drains one slot, then unblocks within the next flush cycle | Backpressure path | PERF-001, SEC-002 |

### AC-7 — YAML `trace_payload_async: true` wires `AsyncFileExporter`

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-011 | Integration | P1 | YAML with `trace_payload_async: true` → `*.llm.jsonl` exporter is an `AsyncFileExporter` instance | Settings → wiring contract | — |
| OBS-003.3-INT-012 | Integration | P0 | YAML with no async flag → exporter is sync `FileExporter` (preserves 003.1 behavior) | Default-off invariant | DATA-001 (default safe) |

### AC-8 — YAML `trace_payload_compress: gzip`

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-UNIT-005 | Unit | P1 | YAML parser accepts `trace_payload_compress: gzip` and exposes it on settings | Config surface | — |
| OBS-003.3-UNIT-006 | Unit | P2 | Unknown codec (e.g., `zstd`, `lz4`) raises a startup error with a clear "reserved/unsupported" message | Forward-compat hygiene | SEC-003 |

### AC-9 — Output extension `.llm.jsonl.gz` via `gzip.open(..., 'at')`

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-013 | Integration | P1 | With `compress=gzip`, file path ends with `.llm.jsonl.gz` | Filename contract — affects cleanup glob and tooling | AC-13 |
| OBS-003.3-INT-014 | Integration | P0 | Captured `.llm.jsonl.gz` decompresses cleanly via stdlib `gzip.open` **and** via system `gunzip` CLI; line count matches enqueued span count | Stream framing must satisfy both readers | DATA-002 |

### AC-10 — Gzip flushes at batch boundaries (open/write/close per flush)

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-015 | Integration | P0 | Across N flushes, file is opened/closed exactly N times; per-flush span count > 1 | Mandatory batch-flush invariant | DATA-002 |
| OBS-003.3-INT-016 | Integration | P0 | 5 batches × 20 spans → file decompresses to exactly 100 newline-delimited records, all parseable as JSON | Multi-batch gzip integrity | DATA-002 |

### AC-11 — Sync gzip path

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-017 | Integration | P1 | `compress=gzip` + `async=false`: each `export()` performs open/write/close; resulting file decompresses to all spans in order | Sync + gzip happy path | — |
| OBS-003.3-INT-018 | Integration | P2 | Micro-benchmark: sync-gzip vs sync-text 1000-span append; multiplier recorded in test output and surfaced in observability docs | Quantifies the silent latency tax | TECH-002 |

### AC-12 — `tea trace cat <file>` helper

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-019 | Integration | P1 | `tea trace cat sample.llm.jsonl.gz` prints decompressed lines to stdout, exit 0 | Primary helper UX | — |
| OBS-003.3-INT-020 | Integration | P2 | `tea trace cat sample.llm.jsonl` (plain) works identically; auto-detect by extension | Avoids format surprise | — |
| OBS-003.3-INT-021 | Integration | P2 | Truncated/corrupt gzip → non-zero exit, error to stderr, prints all lines successfully read before the corruption | Partial-recovery semantics | OPS-001 |

### AC-13 — `tea trace cleanup` handles `.jsonl.gz`

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-022 | Integration | P1 | With mixed `.jsonl` + `.jsonl.gz` files older than retention, both are pruned; younger files of both extensions survive | Cross-story compatibility with 003.2 | — |

### AC-14 — Default-off byte equivalence to 003.1

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-023 | Integration | P0 | Same input workflow run under 003.1 baseline and 003.3 default config; SHA-256 checksum of output `.llm.jsonl` matches | Hard guarantee that opt-in feature is invisible by default | DATA-001 (no regression for non-adopters) |

### AC-15 — Combined async + gzip

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-024 | Integration | P0 | `async=true` + `compress=gzip`, 100 spans → exactly one `.jsonl.gz` file, decompresses to 100 valid JSON lines | Headline integration of both flags | DATA-002, TECH-001 |
| OBS-003.3-INT-025 | Integration | P0 | Round-trip: 100 enqueued spans equal 100 read-back spans (order preserved within each batch; cross-batch order best-effort) | End-to-end no-loss under graceful shutdown | DATA-001 |

### AC-16 — Background-thread failure is non-fatal

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-026 | Integration | P0 | Inject `OSError(ENOSPC)` on flush: workflow run completes successfully; one ERROR log emitted (not per span); no exception propagates to caller | Crash-resistance contract | DATA-001 |
| OBS-003.3-INT-027 | Integration | P1 | Inject `PermissionError` on flush: same recovery semantics as ENOSPC | Symmetric path | — |
| OBS-003.3-INT-028 | Integration | P1 | After worker dies, subsequent `export()` calls accumulate until queue full → overflow policy from AC-6 fires (drop counter increments OR `block` blocks) | Documents "spans accumulate without being written" terminal state | OPS-001 |

### AC-17 — Test coverage (meta-AC)

Satisfied by the rest of this matrix; no standalone test ID. Verify in QA gate that the seven enumerated scenarios in AC-17 (async happy path, drop_newest overflow, block overflow, shutdown drain, sync gzip, async gzip, background-thread failure recovery) each correspond to ≥1 entry above. Mapping:

| AC-17 enumerated scenario | Backing test IDs |
|---|---|
| Async happy path | INT-001, INT-005 |
| Drop_newest overflow | INT-008 |
| Block overflow | INT-010 |
| Shutdown drain | INT-005, INT-006 |
| Sync gzip | INT-017 |
| Async gzip | INT-024, INT-025 |
| Background-thread failure recovery | INT-026, INT-027, INT-028 |

### AC-18 — Benchmark ≥5× speedup on simulated slow disk

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-029 | Integration | P0 | 100 LLM-call workflow against 50 ms-per-write fixture: async ≥ 5× faster than sync; ratio captured in test output | Headline performance claim | Performance NFR |
| OBS-003.3-DOC-001 | Doc | P1 | Measured ratio is recorded verbatim in `docs/python/observability.md` (matches benchmark test output) | Story owner committed to documenting the measurement | — |

### AC-19 — Documentation in `docs/python/observability.md`

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-DOC-002 | Doc | P1 | `observability.md` contains all four sections: when to enable async, when to enable gzip, trade-offs (incl. "async = potential data loss on hard crash"), worked combined example | Doc-as-AC | DATA-001 (documented trade-off) |

---

## Cross-cutting Risk-Driven Tests (not tied to a single AC)

These exist to close risk-profile items that span multiple ACs.

| ID | Level | Priority | Test | Justification | Mitigates |
|---|---|---|---|---|---|
| OBS-003.3-INT-030 | Integration | P0 | 10 concurrent producer threads × 100 `export()` calls each: assert `delivered + dropped == 1000`; no duplicates by span-id | Concurrency correctness — most subtle bug class | TECH-001 |
| OBS-003.3-INT-031 | Integration | P0 | Subprocess-based test: parent forks child running async exporter; child receives `SIGKILL` between flushes; parent inspects file → all already-flushed batches valid JSONL/gzip | Hard-crash data-loss bound | DATA-001 |
| OBS-003.3-INT-032 | Integration | P2 | `os.fork()` after exporter construction (gunicorn-style pre-fork): no deadlock; either child writes successfully (lazy-start) or fails fast with a documented error | Cloud-deploy footgun | TECH-003 |
| OBS-003.3-INT-033 | Integration | P2 | `close()` called twice → idempotent; second call is a no-op, no exception, no double-WARN | Common shutdown-sequencing footgun | — |
| OBS-003.3-INT-034 | Integration | P1 | Counter attributes (`drops`, `queue_depth`, `flush_count`, `last_flush_ts`) increment correctly under INT-008 and INT-006 scenarios — **applies only if proposed AC-20 is folded in**; otherwise mark "deferred" | Operator observability | OPS-001 |

---

## Risk Coverage Matrix

| Risk ID | Severity | Backing P0/P1 tests |
|---|---|---|
| **DATA-001** Data loss on hard crash | High (6) | INT-005, INT-023, INT-031, DOC-002 |
| **TECH-001** Background-thread sync correctness | Medium (4) | INT-001, INT-008, INT-030 |
| **TECH-004** Shutdown drain timeout exhaustion | Medium (4) | INT-005, INT-006 |
| **PERF-001** Queue overflow under sustained slow disk | Medium (4) | INT-008, INT-010 |
| **OPS-001** No metrics for exporter health | Medium (4) | INT-034 (deferred unless AC-20 folded in) |
| **DATA-002** Gzip stream framing | Low | INT-014, INT-015, INT-016 |
| **TECH-002** Sync-gzip silent latency tax | Low | INT-018 |
| **TECH-003** Fork-after-thread interaction | Low | INT-032 |
| **PERF-004** Single-writer throughput ceiling | Low | INT-029 (benchmark surfaces ceiling) |
| **SEC-002** `block`-policy DoS | Low | INT-010 (asserts unblock path exists) |
| **SEC-003** Configuration bounds | Low | UNIT-002, UNIT-006 |
| **OPS-003** Test flakiness | Low | Test-design rule: no `sleep` in assertion paths (applies to all concurrency tests) |

**Coverage gap (acknowledged):** OPS-001 has no live test unless the proposed AC-20 (introspection counters) is folded into scope. If deferred, INT-034 should be tagged `xfail-strict` with the deferral story ID.

---

## Test Data and Environment Requirements

### Slow-disk fixture (used by INT-006, INT-008, INT-010, INT-018, INT-029)

A pytest fixture that wraps a tmpdir with a write-delay shim. Two implementations acceptable, in priority order:

1. **Preferred:** `unittest.mock.patch` on the exporter's `open` to wrap `write()` in a deterministic `time.sleep(0.05)`. Fast, hermetic, no FS dependency.
2. **Fallback (Linux-only smoke):** `fusepy`-backed slow filesystem at a tmpfs mount; only used in a single integration smoke test if mock approach proves insufficient. Skip on non-Linux.

The fixture must expose a configurable per-write delay so the same code drives 50 ms (AC-18 target) and ~5 ms (faster CI lanes).

### Span fixture

A canned `Span` factory producing realistic ~25 KB payloads (matches story's "25 KB JSONL append" assumption). Must be deterministic by seed so order/round-trip assertions are stable.

### YAML fixture matrix

Six minimal configs covering the cartesian of `{async ∈ {off, on}}` × `{compress ∈ {off, gzip}}`, plus one with `overflow_policy: block` and one with explicit `shutdown_timeout: 0.5`. Stored under `python/tests/fixtures/yaml/obs_003_3/`.

### Subprocess + SIGKILL harness (INT-031)

`subprocess.Popen` of a tiny Python entry point that imports the exporter, calls `export()` 200 times spaced over multiple flush windows, then sleeps. Parent test sends `SIGKILL` after the second observed flush (detected by `inotify` on the output file or polling file size with a tight bound). Must run only on POSIX; skip on Windows.

### Concurrency-test rule (proposed AC-23)

**Mandatory for INT-005, INT-008, INT-010, INT-030:** assertions must use `thread.join(timeout)`, `queue.empty()`, `Event.wait(timeout)`, or `concurrent.futures.Future.result(timeout)`. `time.sleep()` is permitted only inside the slow-disk fixture body, never in the assertion path. Reviewers should reject PRs that violate this rule.

### Logging assertions

WARN/ERROR-line assertions (INT-006, INT-009, INT-026) must use pytest `caplog` with structured-field assertions on `record.getMessage()` and on extras (`record.dropped`, `record.queue_depth`). Do not regex on raw text alone — formatting can drift.

### CI lanes

- **Fast lane (every PR):** all UNIT + all P0/P1 INT scenarios except INT-029 (benchmark) and INT-031 (SIGKILL).
- **Slow lane (nightly):** full matrix including INT-029 and INT-031. Benchmark INT-029 publishes a metric to the run summary; failure threshold is `< 5×` ratio.

---

## Recommended Execution Order

1. **P0 Unit** (UNIT-002 only — input validation; ~milliseconds)
2. **P0 Integration — happy paths** (INT-001, INT-012, INT-023, INT-024, INT-025) — establish baseline correctness before stressing failure modes
3. **P0 Integration — failure modes** (INT-005, INT-006, INT-008, INT-010, INT-014, INT-015, INT-016, INT-026, INT-030, INT-031) — most-likely-to-fail bucket; running these next means a broken async impl gets caught fast
4. **P0 Integration — performance** (INT-029) — last in the P0 set because it is the slowest
5. **P1 set** (INT-002, INT-003, INT-011, INT-013, INT-017, INT-019, INT-022, INT-027, INT-028, INT-034 if applicable, UNIT-003, UNIT-004, UNIT-005, DOC-001, DOC-002)
6. **P2 set** (INT-004, INT-007, INT-009, INT-018, INT-020, INT-021, INT-032, INT-033, UNIT-001, UNIT-006)

---

## Quality Checklist

- [x] Every AC (1–19) has at least one test
- [x] Test levels are appropriate — unit only where logic is pure; no E2E given non-user-facing surface
- [x] No duplicate coverage across levels (INT-014/015/016 differ in framing/multi-batch/integrity)
- [x] Priorities align with risk (every High/Medium risk has ≥1 P0 backing test or explicit deferral)
- [x] Test IDs follow `OBS-003.3-{LEVEL}-{NNN}` convention
- [x] Scenarios are atomic and independent (each can be skipped without invalidating others)
- [x] Concurrency-test design rule (no `sleep` in asserts) stated explicitly
- [x] Coverage gap (OPS-001 / INT-034) explicitly called out, not silently dropped

---

## Gate YAML Block

```yaml
test_design:
  story: TEA-OBS-003.3
  scenarios_total: 38
  by_level:
    unit: 6
    integration: 30
    doc: 2
  by_priority:
    p0: 14
    p1: 13
    p2: 11
  coverage_gaps:
    - id: OPS-001
      note: "Introspection counters (drops/queue_depth/...) require proposed AC-20 to be folded into scope; INT-034 deferred otherwise"
  fixtures_required:
    - slow_disk (mock-based, configurable per-write delay)
    - span_factory (deterministic 25 KB payloads)
    - yaml matrix (6 configs)
    - sigkill subprocess harness (POSIX-only)
  ci_lanes:
    fast: "All UNIT + P0/P1 INT except INT-029 and INT-031"
    nightly: "Full matrix incl. benchmark and SIGKILL"
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-OBS-003.3-test-design-20260501.md
P0 tests identified: 14
```
