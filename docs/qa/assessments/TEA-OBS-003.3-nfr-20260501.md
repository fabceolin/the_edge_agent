# NFR Assessment: TEA-OBS-003.3

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.3 — Async exporter + optional gzip
Story Status: Draft (design-time NFR review)
Mode: YOLO (non-interactive; core four NFRs)

## Summary

- **Security:** PASS — Trusted-source YAML config inherits 003.1 path validation; no new attack surface. One minor recommendation on `queue_size` upper bound.
- **Performance:** CONCERNS — Targets are quantified (≥5x speedup, <1ms `export()`), but no validated baselines for sync gzip cost (TECH-002) or single-writer ceiling (PERF-004).
- **Reliability:** CONCERNS — Acknowledged async data-loss-on-crash trade-off (DATA-001) and shutdown-timeout exhaustion (TECH-004). Background-thread failure handling (AC-16) does not specify retry/recovery semantics.
- **Maintainability:** CONCERNS — Test plan is comprehensive (AC-17/18) but threading tests carry flakiness risk (OPS-003); no introspection counters for runtime tuning (OPS-001); docs requirement (AC-19) is the only protection against operational misuse.

**Overall NFR status: CONCERNS** — driven by reliability (DATA-001) and maintainability (OPS-001 telemetry gap). All issues are acknowledged in the risk profile and are not blockers for an opt-in default-off feature.

**Quality score: 70/100** (3 × CONCERNS = -30)

## Critical Issues

None — no FAIL-tier findings. The reliability risk (DATA-001) is inherent to async buffering and the story addresses it with opt-in defaults plus AC-19 documentation.

## NFR-by-NFR Detail

### Security — PASS

**Coverage:**
- Inherits TEA-OBS-003.1 file-path handling. No new untrusted-input boundary added.
- YAML settings come from trusted source (per CLAUDE.md security warning).
- AC-9 path computation is `<base>.gz` — base is YAML-controlled, not user-runtime input.
- `overflow_policy: block` (AC-6) is opt-in; abuse-as-DoS risk (SEC-002) requires operator misconfiguration plus disk stall (joint condition Low × Medium).

**Gaps:**
- AC-7/AC-8 do not specify upper-bound validation for `queue_size` (e.g., warn if > 100K) — see SEC-003. Operator misconfig of `queue_size: 10_000_000` would consume hundreds of MB of RAM under load.
- No mention of input validation for `flush_interval_s` (negative, zero, or absurdly large values).

**Recommendations:**
- Validate `queue_size` and `flush_interval_s` ranges in `yaml_engine.py` parsing (AC-7); emit a WARN for out-of-range values, refuse impossible ones (e.g., `queue_size <= 0`).
- Add a sentence to AC-19 docs documenting recommended ranges.

### Performance — CONCERNS

**Coverage:**
- AC-3 sets a quantified `export()` target: <1ms wall-clock under async — testable.
- AC-18 sets a quantified end-to-end target: ≥5x speedup vs sync on a 50ms-per-write fixture, with documented actual ratio.
- AC-4 batch-flush design (every `flush_interval_s` or 50 spans) avoids per-span fsync — addresses the original 50–100ms/span complaint.
- AC-10 mandates batch boundaries for gzip — prevents per-span gzip-open hot path on async path.

**Gaps:**
- **TECH-002** — sync-gzip path (AC-11) has no benchmark requirement. Per-span `gzip.open(..., 'at')` is 10–50x slower than text append. Operators enabling `compress=gzip` without `async=true` will pay this cost silently.
- **PERF-004** — single worker thread + single file = single writer. AC-18 benchmark is at 100 spans; no upper-bound throughput envelope (e.g., 10K spans/s) is asserted.
- **PERF-003** (Minimal) — gzip overhead vs uncompressed I/O on tiny payloads not measured; could regress on small-span workloads.
- No memory budget specified for the queue (`queue_size=1000` × ~25 KB/span ≈ 25 MB worst case — not stated explicitly).

**Recommendations:**
- Add a sub-AC under AC-18: include sync-gzip-vs-sync-text comparison, document the multiplier so operators can choose.
- Document the throughput envelope from the benchmark in the observability docs (AC-19) — answers "when do I outgrow this?"
- Either warn at startup when `compress=gzip` is set without `async=true`, or document this as a known anti-pattern.

### Reliability — CONCERNS

**Coverage:**
- AC-5 — shutdown drain with configurable timeout (default 30s) and WARN log on drops.
- AC-6 — bounded queue + configurable overflow policy.
- AC-10 — gzip batch boundaries make each member independently parseable.
- AC-14 — default-off byte equivalence preserves 003.1's reliability profile.
- AC-16 — background-thread failures are non-fatal to the workflow.

**Gaps:**
- **DATA-001** — async + batch flush means hard crash (`kill -9`, OOM, power loss) loses up to one batch (~5s with default `flush_interval_s`). Acknowledged in story; mitigated by opt-in default + documentation. **Residual risk accepted.**
- **TECH-001** — thread synchronization correctness is left to implementation. AC does not require use of `queue.Queue` (thread-safe primitive) or sentinel-based shutdown. A naive flag-based shutdown can race.
- **TECH-004** — shutdown drain may exceed timeout on slow disk; AC-5 says "best-effort" and "warning if any are dropped" but does not require the dropped count, queue depth, or final flush latency to be in the WARN line.
- **AC-16 ambiguity** — "subsequent `export()` calls put spans on the queue but they accumulate without being written" implies an unrecoverable terminal state once the worker dies. No mechanism to retry the worker, no health attribute on the exporter, no escalation beyond the single ERROR log. The queue will grow until `overflow_policy` kicks in.
- **TECH-003** — fork-after-thread (gunicorn pre-fork) is unaddressed. Constructed exporter + child fork = silent span loss in child.
- No spec for what `TraceContext.close()` does if called twice (idempotency).

**Recommendations:**
- AC-5 should require the shutdown WARN to include a structured count of dropped spans and final queue depth.
- Add an explicit testing-priority sub-AC: "thread synchronization uses `queue.Queue` and sentinel-based shutdown" (or equivalent — pin the implementation strategy).
- AC-16 should clarify whether the worker can be restarted, or document that exporter is one-shot post-failure. Either is acceptable; ambiguity is not.
- Either lazy-start the worker thread on first `export()` call (handles fork) or document the constraint in AC-19.
- Add a test for double `close()` / double shutdown idempotency.

### Maintainability — CONCERNS

**Coverage:**
- AC-17 lists test scenarios: async happy path, overflow drop_newest/block, shutdown drain, sync gzip, async gzip, background failure recovery.
- AC-18 specifies a benchmark with quantified target and documentation requirement.
- AC-19 requires docs in `docs/python/observability.md` covering when-to-use, trade-offs, and worked example.
- Story is layered on existing `TraceExporter` Protocol (AC-2) — extends, doesn't fork.

**Gaps:**
- **OPS-001** — no introspection API. Operators cannot programmatically check `drops`, `queue_depth`, `flush_count`, `last_flush_ts`. Without these, tuning `queue_size` or detecting slow disks requires reading log lines.
- **OPS-003** — concurrency tests (drain, overflow, race) are notoriously flaky on slow CI. AC-17 doesn't require deterministic synchronization (event-driven assertions, injectable clock).
- **AC-12** (`tea trace cat`) is a single bullet — no spec for behavior on partial/corrupt files, plain `.jsonl` (uncompressed) vs `.jsonl.gz` extension detection, or stderr/exit-code conventions.
- No mention of code reuse strategy: AsyncFileExporter + GzipFileExporter as separate classes vs `FileExporter(codec=..., async_=...)` factory. Risk of two-by-two combinatorial duplication if the wrong path is chosen.
- AC-19 docs are described but no acceptance test ("docs reviewed for clarity" is implicit).

**Recommendations:**
- Add an AC: "AsyncFileExporter exposes `drops`, `queue_depth`, `flush_count`, `last_flush_ts` as readable attributes" — closes OPS-001.
- Add a test-design constraint: "concurrency tests must use event-driven assertions (`thread.join`, `queue.empty()`); no `time.sleep()` in assertion paths" — addresses OPS-003.
- Specify in AC-12 that `tea trace cat` works on both `.jsonl` and `.jsonl.gz` (auto-detect by extension), and prints to stdout one line per span.
- Pick the design strategy explicitly (separate classes vs. composable codec/async) in Tasks/Subtasks; the choice affects test surface and future extension to zstd.

## Quick Wins

| Action | Estimated Effort | NFR(s) Addressed |
|---|---|---|
| Validate `queue_size` and `flush_interval_s` upper/lower bounds in YAML parser | ~1h | Security (SEC-003), Reliability |
| Expose `drops`/`queue_depth`/`flush_count`/`last_flush_ts` attributes | ~2h | Maintainability (OPS-001) |
| Lazy-start worker thread on first `export()` | ~1h | Reliability (TECH-003) |
| Add concurrency test design rule (event-driven, no `sleep`) to AC-17 | ~30m | Maintainability (OPS-003) |
| Include dropped-count in shutdown WARN log line | ~30m | Reliability (TECH-004) |
| Add sync-gzip benchmark sub-AC under AC-18 | ~30m doc + ~2h test | Performance (TECH-002) |
| Auto-detect extension in `tea trace cat` (AC-12) | ~30m | Maintainability |

Total estimate: ~7-8 hours of design refinement before/during implementation.

## NFR Acceptance Criteria (proposed additions)

These are **suggestions** — story owner decides whether to fold them in. None are blockers; all are quality-raising.

1. **AC-20 (proposed):** `AsyncFileExporter` exposes the following readable attributes for operator introspection: `drops` (int), `queue_depth` (int), `flush_count` (int), `last_flush_ts` (float | None), `worker_alive` (bool).
2. **AC-21 (proposed):** YAML parser validates `queue_size ∈ [1, 100_000]` and `flush_interval_s ∈ [0.1, 3600]`. Out-of-range values produce a startup ERROR; absurd-but-valid values (e.g., `queue_size > 10_000`) produce a startup WARN.
3. **AC-22 (proposed):** Shutdown WARN log line is structured and includes: dropped span count, final queue depth, total flush count, drain duration.
4. **AC-23 (proposed):** Concurrency tests (AC-17) must use event-driven synchronization (`thread.join(timeout)`, `queue.empty()`, `Event.wait(timeout)`); `time.sleep()` is permitted only as a fixture for slow-disk emulation, never in assertion paths.
5. **AC-24 (proposed):** AC-18 benchmark report includes sync-gzip-vs-sync-text multiplier and documented throughput ceiling (spans/s) at which the single-writer becomes the bottleneck.
6. **AC-25 (proposed):** Worker thread lazy-starts on first `export()` call to interact safely with fork; OR `os.register_at_fork(after_in_child=...)` reinitializes state. AC-19 docs note the chosen strategy.

## Test Recommendations

**Priority 1 — Reliability and core correctness (must pass before merge):**

1. **TC-NFR-1 (Reliability/DATA-001):** Subprocess fixture writes N spans, parent sends `SIGKILL`. Assert pre-crash batches on disk are valid JSONL/gzip and span count = N − (in-flight batch).
2. **TC-NFR-2 (Reliability/TECH-001):** 10 producer threads × 100 spans with `drop_newest`. Assert `delivered + dropped == 1000` and no exceptions in workers.
3. **TC-NFR-3 (Reliability/TECH-004):** Slow-disk fixture (50ms/write), submit 100 spans, call shutdown with timeout=1s. Assert WARN log includes exact dropped count and partial output is parseable.
4. **TC-NFR-4 (Reliability/AC-16):** Inject disk-full mid-flush. Assert workflow completes, ERROR is logged once, no exception propagates.
5. **TC-NFR-5 (Maintainability/OPS-001):** After running TC-NFR-2 and TC-NFR-3, assert exposed counters reflect the observed behavior exactly.

**Priority 2 — Performance and observable trade-offs:**

6. **TC-NFR-6 (Performance/AC-18):** 100-call workflow, 50ms/write fixture, sync vs async — assert ≥5x speedup, document actual ratio in test output and AC-19 docs.
7. **TC-NFR-7 (Performance/TECH-002):** Sync gzip vs sync text on local tmpfs — record multiplier; output is informational, no hard threshold.
8. **TC-NFR-8 (Reliability/DATA-002):** Multi-batch gzip file round-trips via `gzip.open(..., 'rt')` and via `gunzip` CLI → both yield N JSONL lines.
9. **TC-NFR-9 (Reliability/double-shutdown):** `close()` called twice on same `TraceContext` is idempotent (no exception, no double-flush).

**Priority 3 — Configuration and integration:**

10. **TC-NFR-10 (Security/SEC-003):** YAML with `queue_size: 0` rejected; `queue_size: 1_000_000` accepted with WARN.
11. **TC-NFR-11 (Maintainability/AC-14):** Default-off byte equivalence to 003.1 — checksum compare.
12. **TC-NFR-12 (Reliability/TECH-003):** Manual integration test — construct exporter, fork (multiprocessing or gunicorn-style), child writes, parent shuts down; assert no deadlock.

## Status Determination

```
Selected NFRs: [security, performance, reliability, maintainability]

Security:        PASS     — No new attack surface; minor config-validation gap (Low)
Performance:     CONCERNS — Quantified targets but unmeasured paths (sync-gzip, throughput ceiling)
Reliability:     CONCERNS — DATA-001 acknowledged trade-off; TECH-001/TECH-004/AC-16 ambiguities
Maintainability: CONCERNS — Test flakiness risk; no runtime introspection; AC-12 underspecified

Deterministic mapping:
- 0 FAIL  → not FAIL
- 3 CONCERNS → CONCERNS

Quality score = 100 − (3 × 10) − (0 × 20) = 70
```

## Gate YAML Block

```yaml
# Gate YAML (paste into qa.qaLocation/gates/TEA-OBS-003.3-async-exporter-gzip.yml under nfr_validation):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'Trusted-source YAML config; no new attack surface. Recommend queue_size/flush_interval_s bounds validation (SEC-003).'
  performance:
    status: CONCERNS
    notes: 'AC-3 (<1ms export) and AC-18 (≥5x speedup) quantified. Sync-gzip cost (TECH-002) and single-writer throughput ceiling (PERF-004) not measured.'
  reliability:
    status: CONCERNS
    notes: 'DATA-001 (async crash data loss) acknowledged + opt-in mitigated. TECH-001 sync correctness, TECH-004 shutdown timeout, AC-16 worker-failure recovery semantics need tightening.'
  maintainability:
    status: CONCERNS
    notes: 'No runtime introspection counters (OPS-001). Concurrency test flakiness risk (OPS-003). AC-12 (tea trace cat) underspecified for extension auto-detect.'
```

## Story Update Line

```
NFR assessment: docs/qa/assessments/TEA-OBS-003.3-nfr-20260501.md
```

## Gate Integration Line

```
Gate NFR block ready → paste into docs/qa/gates/TEA-OBS-003.3-async-exporter-gzip.yml under nfr_validation
```
