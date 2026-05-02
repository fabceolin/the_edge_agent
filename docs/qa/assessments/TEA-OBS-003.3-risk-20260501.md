# Risk Profile: Story TEA-OBS-003.3

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.3 — Async exporter + optional gzip
Mode: YOLO

## Executive Summary

- Total Risks Identified: 18
- Critical Risks (score 9): 0
- High Risks (score 6): 1
- Medium Risks (score 4): 4
- Low Risks (score 2-3): 8
- Minimal Risks (score 1): 5
- Overall Risk Score: **54/100** (Moderate)
- Recommended Gate: **CONCERNS** — driven by DATA-001 (acknowledged data-loss-on-crash trade-off; opt-in by design and documented)

The story is intentionally low-priority and additive, with all risky behavior gated behind opt-in flags (`trace_payload_async`, `trace_payload_compress`). Default-off behavior is byte-identical to 003.1 (AC-14), keeping blast radius small. The dominant residual risk is the well-known async-buffering trade-off (data loss on hard crash), which the story already discloses and mitigates through documentation and bounded queues.

## Critical Risks Requiring Immediate Attention

None identified (no risks scored 9).

## High Risks

### 1. DATA-001: Data loss on hard crash with async exporter

**Score: 6 (High)**
**Probability**: High (3) — A `kill -9`, OOM-kill, or power loss between batch flushes will lose any spans still buffered in the in-memory queue. With `flush_interval_s=5` default, up to 5 seconds of spans are at risk on every crash.
**Impact**: Medium (2) — Lost spans degrade observability and debugging post-incident, but the workflow's primary outputs (state, checkpoints, logs) are unaffected. This is a quality-of-debugging issue, not a correctness issue.
**Affected Components**: `AsyncFileExporter`, `TraceContext.close()`
**Mitigation**:
- AC-5 mandates drain-on-shutdown with configurable timeout (default 30s) for clean shutdowns
- Document explicitly in `docs/python/observability.md` (AC-19) that async = potential loss on hard crash
- Default is sync (AC-14); operators must explicitly opt in
- Consider documenting a recommended `flush_interval_s` ceiling (e.g., 5s) with rationale
**Testing Focus**:
- Test 1: Graceful shutdown drains queue completely (assert all N spans on disk)
- Test 2: Simulated SIGKILL mid-batch — verify pre-batch spans are intact (gzip valid, JSONL parseable)
- Test 3: Documented loss budget: with `flush_interval_s=5`, no more than ~5s of spans should be missing
**Residual Risk**: Medium — Inherent to any async buffering scheme. Acceptable given opt-in default.

## Medium Risks

### 2. TECH-001: Background thread synchronization correctness

**Score: 4 (Medium)** — Probability: Medium (2), Impact: Medium (2)
**Description**: Race conditions between producer (`export()`), consumer (worker thread), and shutdown signaler can cause spans to be silently dropped, written twice, or written after the file is closed.
**Affected Components**: `AsyncFileExporter` queue + worker thread + shutdown signal
**Mitigation**:
- Use `queue.Queue` (thread-safe) rather than rolling a custom buffer
- Use a sentinel value (`None` or a `_StopMarker`) on the queue for clean shutdown signaling instead of a parallel flag
- Worker thread must check shutdown after every `get()` and drain remaining items
- Acquire-release ordering on file handle: open inside worker, close inside worker only
**Testing Focus**: Concurrent producers (multiple `export()` from different threads), shutdown-while-flushing race, double-shutdown idempotency.

### 3. TECH-004: Shutdown race / drain timeout exhaustion

**Score: 4 (Medium)** — Probability: Medium (2), Impact: Medium (2)
**Description**: If the slow disk fixture or a real slow NFS exceeds the 30s drain timeout, spans accumulated in the queue are lost without operator awareness beyond a single warning log.
**Mitigation**:
- AC-5 requires WARN log when spans dropped at shutdown timeout — must include count
- Make timeout configurable (already in AC-5)
- Emit final statistics (queue depth, dropped count) at shutdown to aid post-mortem
**Testing Focus**: Drain timeout exceeded → exactly the documented count is logged; partial results on disk are valid.

### 4. PERF-001: Queue overflow under sustained slow disk

**Score: 4 (Medium)** — Probability: Medium (2), Impact: Medium (2)
**Description**: If write throughput < produce throughput for an extended period, the bounded queue fills. `drop_newest` silently loses recent spans; `block` defeats the latency benefit and risks deadlocking the producer.
**Mitigation**:
- AC-6 already requires throttled WARN logging (once per minute) on drop
- Add a counter of total drops since process start
- Document operator guidance: monitor drop counter, raise `queue_size` or move to faster disk
**Testing Focus**:
- `drop_newest`: under fixture with 50ms-per-write disk and 100 spans/s producer, verify drops counted and warned
- `block`: producer blocks but recovers when consumer catches up; no deadlock with shutdown signal

### 5. OPS-001: Observability gap — no metrics for the exporter itself

**Score: 4 (Medium)** — Probability: Medium (2), Impact: Medium (2)
**Description**: The async exporter is itself an observability mechanism, but the story does not require emitting metrics for queue depth, drop count, flush latency, or worker-thread health. Operators have no programmatic signal that they should tune `queue_size` or fix slow storage.
**Mitigation**:
- At minimum, expose attributes on `AsyncFileExporter` instance (`drops`, `queue_depth`, `last_flush_ts`) so callers can sample
- Log periodic summary at INFO (e.g., every N flushes or once per minute) when async is enabled
- Mention in observability docs how to surface these counters to existing monitoring
**Testing Focus**: Counters increment correctly under drop and overflow scenarios; introspection API works.

## Low Risks

### 6. DATA-002: Gzip stream corruption from improper appending

**Score: 3 (Low)** — Probability: Low (1), Impact: High (3)
**Description**: `gzip` files are stream-format with framing. Appending to a closed gzip file in `'at'` mode is supported but historically buggy in Python <3.7; concurrent reads-during-append can return truncated streams.
**Mitigation**:
- AC-10 mandates batch-flush boundaries (open/write/close per flush) — eliminates per-span append risk
- Each flush produces a self-contained gzip member; concatenated members are valid gzip per RFC 1952
- Test reading back files after multiple flushes with `gzip.open(..., 'rt')` and `gzip` CLI
**Testing Focus**: Multi-batch gzip file is parseable end-to-end; partial-write recovery (kill mid-flush) leaves prior batches intact.

### 7. TECH-002: Gzip + sync mode latency cost not validated

**Score: 3 (Low)** — Probability: Low (1), Impact: High (3)
**Description**: AC-11 allows sync gzip. Per-span `gzip.open(..., 'at')` is roughly 10-50x slower than text append on local disk and produces many tiny gzip members (poor compression ratio, slow to read).
**Mitigation**:
- Document as a known anti-pattern in observability docs (AC-19)
- If feasible, warn at startup when `compress=gzip` is set without `async=true`
**Testing Focus**: Benchmark sync gzip vs sync text; document the multiplier so operators can decide.

### 8. TECH-003: Fork-after-thread interaction (uvicorn/gunicorn workers)

**Score: 3 (Low)** — Probability: Low (1), Impact: High (3)
**Description**: If a parent process has started the `AsyncFileExporter` worker thread and then forks (gunicorn pre-fork model), the child gets a copy of the queue but no live worker, and the file handle state is unsafe. Spans submitted in the child are silently lost.
**Mitigation**:
- Lazy-start the worker thread on first `export()` call, not at construction
- Register a `os.register_at_fork(after_in_child=...)` handler to reinitialize state in fork children
- Document constraint: construct exporter inside the worker process, not before fork
**Testing Focus**: Manual integration test under gunicorn pre-fork; assert no shared-thread surprises.

### 9. PERF-004: Single worker thread is the bottleneck for very high throughput

**Score: 2 (Low)** — Probability: Low (1), Impact: Medium (2)
**Description**: One writer thread + one file = single sequential write stream. At >10K spans/s on a fast disk, the writer's `gzip.compress` + `write` cannot keep up.
**Mitigation**: Out of scope for this story; document the throughput envelope from the benchmark in AC-18.
**Testing Focus**: AC-18 benchmark establishes the realistic ceiling.

### 10. SEC-001: Path traversal via filename construction

**Score: 2 (Low)** — Probability: Low (1), Impact: Medium (2)
**Description**: When `trace_payload_compress: gzip` is set, the code computes the output path by appending `.gz` to a base path. If the base is unsanitized user input (it is not — comes from YAML settings), path traversal could occur.
**Mitigation**: YAML settings are trusted (per CLAUDE.md security warning). Inherits 003.1's path validation. No new attack surface.
**Testing Focus**: Verify the path computation logic doesn't follow symlinks unexpectedly.

### 11. SEC-002: Disk-fill DoS via `overflow_policy: block` and slow consumer

**Score: 2 (Low)** — Probability: Low (1), Impact: Medium (2)
**Description**: An adversary that can stall the disk (NFS unmount, S3 throttling) combined with `block` policy can stall the workflow indefinitely. Different from the pure data-loss risk: this is a liveness risk.
**Mitigation**: `block` is opt-in; default is `drop_newest`. Document explicitly that `block` couples workflow latency to disk health.
**Testing Focus**: With `block` + jammed disk, producer blocks but is interruptible by shutdown signal (no deadlock).

### 12. SEC-003: Misconfiguration → unbounded resource consumption

**Score: 2 (Low)** — Probability: Low (1), Impact: Medium (2)
**Description**: Operator sets `queue_size: 1000000` or similar; under load, the process consumes hundreds of MB of RAM in queued span dicts.
**Mitigation**: Document recommended ranges (default 1000 is safe). Consider validating an upper bound in YAML parsing (e.g., warn if > 100K).
**Testing Focus**: Configuration validation tests for sane bounds.

### 13. DATA-003: Last batch lost on shutdown timeout

**Score: 4 (Medium → reclassified Low after mitigation)**
Already covered under TECH-004 / DATA-001. Listed here only for completeness; not double-counted in the score.

### 14. OPS-003: Test flakiness from thread + timing assertions

**Score: 2 (Low)** — Probability: Medium (2), Impact: Low (1)
**Description**: Concurrency tests on async exporter are notoriously flaky on slow CI runners (timing windows, GC pauses).
**Mitigation**:
- Prefer event-driven assertions (`thread.join(timeout=...)`, `queue.empty()`) over `sleep + check`
- Use a deterministic injectable clock for `flush_interval_s` if possible
- If unavoidable, mark timing-sensitive tests with generous tolerances
**Testing Focus**: Test design must specify deterministic synchronization primitives.

## Minimal Risks

15. **DATA-004**: Span write ordering across concurrent workflows — Score 1. JSONL is line-delimited; order within a single flush is preserved; cross-flush ordering doesn't matter to consumers.
16. **PERF-002**: GIL contention starving worker thread — Score 1. The worker spends most time in I/O which releases the GIL.
17. **PERF-003**: gzip compression overhead negates I/O savings on tiny payloads — Score 1. 25 KB spans compress well; benchmark validates.
18. **OPS-004**: `tea trace cat` UX inconsistency — Score 1. Helper script auto-detects extension. Trivial.
19. **BUS-001**: Adoption risk — feature is opt-in and aimed at production-scale users — Score 1. Story is explicitly low-priority.

## Risk Distribution

### By Category

| Category | Total | Critical | High | Medium | Low | Minimal |
|----------|-------|----------|------|--------|-----|---------|
| TECH | 4 | 0 | 0 | 2 | 2 | 0 |
| SEC  | 3 | 0 | 0 | 0 | 3 | 0 |
| PERF | 4 | 0 | 0 | 1 | 1 | 2 |
| DATA | 4 | 0 | 1 | 0 | 1 | 2 |
| OPS  | 3 | 0 | 0 | 1 | 1 | 1 |
| BUS  | 1 | 0 | 0 | 0 | 0 | 1 |

### By Component

- `tracing.py` (AsyncFileExporter / GzipFileExporter): 11 risks
- `yaml_engine.py` settings wiring: 2 risks
- `tea trace` CLI (cat, cleanup): 2 risks
- Documentation: 2 risks
- Test infrastructure: 1 risk

## Detailed Risk Register

| Risk ID  | Category | Title                                              | Probability | Impact     | Score | Priority |
|----------|----------|----------------------------------------------------|-------------|------------|-------|----------|
| DATA-001 | DATA     | Data loss on hard crash with async exporter       | High (3)    | Medium (2) | 6     | High     |
| TECH-001 | TECH     | Background thread synchronization correctness     | Medium (2)  | Medium (2) | 4     | Medium   |
| TECH-004 | TECH     | Shutdown race / drain timeout exhaustion          | Medium (2)  | Medium (2) | 4     | Medium   |
| PERF-001 | PERF     | Queue overflow under sustained slow disk          | Medium (2)  | Medium (2) | 4     | Medium   |
| OPS-001  | OPS      | No metrics emitted for exporter health            | Medium (2)  | Medium (2) | 4     | Medium   |
| DATA-002 | DATA     | Gzip stream corruption from improper appending    | Low (1)     | High (3)   | 3     | Low      |
| TECH-002 | TECH     | Gzip + sync latency cost not validated            | Low (1)     | High (3)   | 3     | Low      |
| TECH-003 | TECH     | Fork-after-thread interaction                     | Low (1)     | High (3)   | 3     | Low      |
| PERF-004 | PERF     | Single worker thread bottleneck                   | Low (1)     | Medium (2) | 2     | Low      |
| SEC-001  | SEC      | Path traversal via filename                       | Low (1)     | Medium (2) | 2     | Low      |
| SEC-002  | SEC      | Disk-fill DoS via `block` policy + slow disk     | Low (1)     | Medium (2) | 2     | Low      |
| SEC-003  | SEC      | Misconfig → unbounded queue memory                | Low (1)     | Medium (2) | 2     | Low      |
| OPS-003  | OPS      | Test flakiness from thread + timing assertions    | Medium (2)  | Low (1)    | 2     | Low      |
| DATA-004 | DATA     | Cross-flush span ordering                         | Low (1)     | Low (1)    | 1     | Minimal  |
| PERF-002 | PERF     | GIL contention starves worker                     | Low (1)     | Low (1)    | 1     | Minimal  |
| PERF-003 | PERF     | gzip compression overhead on tiny payloads        | Low (1)     | Low (1)    | 1     | Minimal  |
| OPS-004  | OPS      | `tea trace cat` UX consistency                    | Low (1)     | Low (1)    | 1     | Minimal  |
| BUS-001  | BUS      | Adoption risk — feature is opt-in                 | Low (1)     | Low (1)    | 1     | Minimal  |

## Risk-Based Testing Strategy

### Priority 1: High-Risk Tests (must run, must pass before merge)

1. **DATA-001 / TECH-004 — graceful shutdown drain**: Submit N spans, call shutdown, assert all N on disk within timeout. Repeat with timeout artificially low; assert exact dropped count is logged.
2. **DATA-001 — simulated hard crash**: Use `os._exit(9)` or subprocess + SIGKILL between flushes; assert pre-crash batches are valid gzip / valid JSONL.
3. **TECH-001 — concurrency**: 10 producer threads × 100 spans each with `drop_newest`; assert (delivered + dropped) == 1000.
4. **PERF-001 — overflow policies**: Slow-disk fixture (50ms per write), producer at 100 spans/s, queue_size=10. Both `drop_newest` and `block` paths exercised.
5. **OPS-001 — counters**: Drops/queue_depth/flush_count are exposed and increment correctly.

### Priority 2: Medium-Risk Tests

6. **DATA-002 — gzip integrity across flushes**: Write 10 batches; read back via `gzip.open` and via `gunzip` CLI; both should yield N JSONL lines.
7. **TECH-003 — fork safety**: Construct exporter, fork, child writes, parent shuts down — at minimum test does not deadlock; ideally test that lazy-start works.
8. **AC-18 benchmark**: 100-call workflow, 50ms-per-write fixture, sync vs async — assert ≥5x speedup, document actual ratio.

### Priority 3: Standard Tests

9. Sync gzip happy path (AC-11).
10. Default-off byte equivalence to 003.1 (AC-14).
11. Combined async + gzip produces valid `.jsonl.gz` (AC-15).
12. `tea trace cleanup` matches `*.jsonl.gz` (AC-13).
13. `tea trace cat` decompresses correctly (AC-12).
14. Background thread failure → workflow does not crash (AC-16).

## Risk Acceptance Criteria

### Must Fix Before Merge

- TECH-001 — thread synchronization must be correct (concurrency test green)
- TECH-004 — shutdown drain with timeout works and logs dropped count
- DATA-002 — multi-batch gzip files round-trip correctly
- AC-14 byte-equivalence guarantee verified (no regression for default users)

### Can Deploy with Documented Trade-off

- DATA-001 — accept and document; opt-in default-off is sufficient mitigation
- PERF-001 — bounded queue + clear policy is operator's choice
- TECH-003 — document the fork constraint; defer hardening to a follow-up if reports surface

### Accepted Risks (no action needed)

- All Minimal-tier risks (DATA-004, PERF-002, PERF-003, OPS-004, BUS-001)

## Monitoring Requirements

Post-deployment monitoring for operators who enable async:

- **PERF-001 / OPS-001**: Track exporter `drops` counter — rising trend indicates undersized queue or slow disk
- **DATA-001**: Track flush latency p99 — gives the worst-case loss window on crash
- **TECH-004**: Watch shutdown WARN logs for "spans dropped at shutdown" — indicates undersized timeout

These should surface as observable attributes/log fields; alerting/dashboards are operator responsibility.

## Risk Review Triggers

Re-run this risk profile when:

- Adding remote exporters (S3, OTLP) — out-of-scope today, would change DATA-001 calculus
- Adding multi-worker writer threads — would mitigate PERF-004 but complicate TECH-001
- Switching from `threading` to `asyncio` — TECH-003 (fork) becomes non-issue but new event-loop integration risks emerge
- gzip → zstd codec swap — DATA-002 needs revalidation under new codec

## Risk Scoring Summary

```
Base Score = 100
- 0 critical × 20 =   0
- 1 high     × 10 =  10
- 4 medium   ×  5 =  20
- 8 low      ×  2 =  16
Total deductions:    46
Overall Score:       54/100 (Moderate)
```

## Gate Mapping

Per deterministic mapping:
- No score ≥ 9 → not FAIL
- One score = 6 (DATA-001) → **CONCERNS**

**Recommendation:** Gate = CONCERNS, with DATA-001 explicitly waived in the gate file once the documentation in AC-19 lands. Could move to PASS after AC-19 ships and AC-18 benchmark confirms the latency win is real.

## risk_summary (for gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 1
    medium: 4
    low: 8
  highest:
    id: DATA-001
    score: 6
    title: 'Data loss on hard crash with async exporter'
  recommendations:
    must_fix:
      - 'Verify thread synchronization with concurrency tests (TECH-001)'
      - 'Verify graceful shutdown drain logs dropped count (TECH-004)'
      - 'Verify multi-batch gzip files round-trip correctly (DATA-002)'
      - 'Verify default-off byte equivalence to 003.1 (AC-14)'
    monitor:
      - 'Expose drops/queue_depth/flush counters on AsyncFileExporter (OPS-001)'
      - 'Document async data-loss trade-off prominently in observability.md (DATA-001)'
      - 'Document recommended queue_size bounds and operator guidance (PERF-001)'
      - 'Document fork-after-thread constraint or implement lazy worker start (TECH-003)'
```

---

Risk profile: docs/qa/assessments/TEA-OBS-003.3-risk-20260501.md
