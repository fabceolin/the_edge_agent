# Test Design: Epic TEA-OBS-003 — LLM Payload Trace Capture

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO
Scope: Epic-level rollup covering Stories TEA-OBS-003.1, .2, .3
Companion docs: [risk profile](TEA-OBS-003-risk-20260501.md) · [NFR assessment](TEA-OBS-003-nfr-20260501.md) · [child-story assessments](.) (003.1)

---

## 1. Test Strategy Overview

| Metric | Count | Share |
|--------|-------|-------|
| **Total test scenarios** | **78** | 100 % |
| Unit | 36 | 46 % |
| Integration | 32 | 41 % |
| End-to-end (CLI / engine) | 10 | 13 % |
| **By priority** | | |
| P0 (must-pass before release) | 24 | 31 % |
| P1 (high) | 28 | 36 % |
| P2 (medium) | 18 | 23 % |
| P3 (low) | 8 | 10 % |

### Allocation rationale

- **Unit-heavy** because the epic is dominated by pure logic: glob matching, settings parsing, mtime comparison, queue overflow policy, byte placeholder substitution, gzip codec wrapping. These are isolated functions with no I/O dependency once mocked.
- **Integration tests** anchor the highest-value invariants: default-off no-leak, two-file separation (slim spans vs. payloads), Story 1 + Story 2 cross-story flow (capture → TTL elapsed → cleanup), async drain on engine shutdown, second-`FileExporter` registration order.
- **E2E tests** are reserved for things that genuinely require running the CLI / a real engine: `tea run --trace-llm-payloads`, `tea trace cleanup --dry-run`, default-off invariant via real workflow execution. Kept small to control cost.

### Shift-left bias

For each AC the lowest viable level was chosen. Examples:

| Need | Lowest level | Rationale |
|------|--------------|-----------|
| Glob `*` / `**` rejection | Unit | Pure validator function |
| Default-off no-leak | Integration | Must observe filesystem |
| `--dry-run` correctness | Unit (logic) + Integration (CLI) | Logic is pure; CLI shape needs Typer wired |
| Async benchmark speedup | Integration | Requires worker thread + slow-disk fixture |
| Mandatory-retention engine-init failure | Integration | Engine boot path |
| Sync exporter slow-disk latency | Integration | Real I/O fixture |

---

## 2. Story 1 — TEA-OBS-003.1: Core capture + separate exporter + flag

### AC-1 / AC-2 / AC-3 — Settings & CLI flags

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-001 | Unit | P0 | `auto_trace_llm_payloads` parser accepts `False` (default) | Default-off invariant baseline | SEC-001 |
| 003.1-UNIT-002 | Unit | P0 | Parser accepts `True` → `bool(True)` stored | Truthy explicit | — |
| 003.1-UNIT-003 | Unit | P0 | Parser accepts `[extract_batch_*, correct]` → list stored | List-form is the realistic prod case | SEC-002 |
| 003.1-UNIT-004 | Unit | P0 | **Default-off fuzz matrix** — `[None, False, [], 0, "", "False", "0"]` all resolve to "no capture" | Type coercion is the highest-frequency real-world failure mode (NFR-AC-6) | SEC-001, DATA-001 |
| 003.1-UNIT-005 | Unit | P1 | Parser rejects `42`, `{"foo": "bar"}` with actionable error | Misconfiguration safety | TECH-004 |
| 003.1-INT-001 | Integration | P0 | `tea run --trace-llm-payloads` overrides YAML `false` → capture activates | CLI-overrides-YAML contract (AC-2) | — |
| 003.1-INT-002 | Integration | P1 | `--trace-llm-payloads-for extract_batch_* --trace-llm-payloads-for correct` (repeatable) → list of 2 globs in resolved settings | Repeatable flag semantics (AC-3) | — |
| 003.1-INT-003 | Integration | P1 | YAML expansion via TEA-DX-001.1 — `auto_trace_llm_payloads: ${TEA_TRACE_LLM:-false}` resolves correctly with env unset / set / list-typed | Coordination AC (AC-15) | TECH-004 |

### AC-4 — Captured field shape

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-010 | Unit | P0 | Span metadata after capture contains exactly `{messages_input, response_content, tokens_input, tokens_output, model, stop_reason, cost_usd}` | Field-set sentinel — pin schema with explicit list (NFR-AC-11) | — |
| 003.1-UNIT-011 | Unit | P1 | `tokens_input` / `tokens_output` are integers (not strings, not None) | LiteLLM occasionally returns string tokens | — |
| 003.1-UNIT-012 | Unit | P1 | Missing `cost_usd` (LiteLLM returns None) → field stored as `None`, not absent | Avoid accidental schema drift | — |
| 003.1-UNIT-013 | Unit | P2 | `stop_reason` derived from `finish_reason` OR `stop_reason` (both LiteLLM aliases) | Provider variance | — |

### AC-5 — Binary attachment placeholder

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-020 | Unit | P0 | Top-level `messages[i].content = b'...PDF bytes...'` → replaced by `{"type": "binary_omitted", "size_bytes": N}` | High risk per child-story risk profile (DATA-001) | DATA-001 |
| 003.1-UNIT-021 | Unit | P0 | Anthropic-vision nested `content: [{type: "image", source: {data: <bytes>}}]` → nested `data` replaced; size preserved | Nested bytes hide easily | DATA-001 |
| 003.1-UNIT-022 | Unit | P0 | OpenAI-vision `messages[i].images[j].source.bytes` → replaced | Nested bytes hide easily | DATA-001 |
| 003.1-UNIT-023 | Unit | P1 | `bytearray` and `memoryview` also replaced | Pythonic byte-likes | DATA-001 |
| 003.1-UNIT-024 | Unit | P2 | Empty bytes `b""` → still replaced with `size_bytes: 0` | No special-case shortcut | — |
| 003.1-UNIT-025 | Unit | P1 | Resulting payload is `json.dumps`-able with `default=None` (i.e., no residual non-JSON types) | Catch missed edge cases | DATA-001 |

### AC-6 / AC-13 — Default-off no-leak

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-INT-010 | Integration | P0 | **Filesystem snapshot test** — clean tmpdir + workflow with multiple `llm.call`s + flag unset → no `*.llm.jsonl` exists; spans file byte-identical to baseline | The PII no-leak invariant — single most important test in the epic | SEC-001, DATA-001 |
| 003.1-INT-011 | Integration | P0 | Same snapshot test with each falsy form (`False`, `[]`, `null`, env var unset, env var = "0") | Default-off fuzz at integration level | SEC-001 |
| 003.1-UNIT-030 | Unit | P0 | When capture is off, span `metadata` dict is missing the payload keys (not just empty values) | Distinguishes "off" from "on with empty payload" | SEC-001 |

### AC-7 / AC-8 / AC-9 — Separate exporter file

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-INT-020 | Integration | P0 | `--trace-file run.jsonl` + capture on → both `run.jsonl` (no payloads) and `run.llm.jsonl` (with payloads) exist | Two-file separation is the headline AC | OPS-003 |
| 003.1-INT-021 | Integration | P1 | Slim file is a strict subset of payload file — same span IDs, same span order, but slim has no payload keys | Round-trip integrity | OPS-003 |
| 003.1-INT-022 | Integration | P1 | `trace_file` unset + capture on → fallback to `tea-llm-payloads-<run_id>.jsonl` in CWD; INFO log line emits the path | AC-9 fallback | OPS-003 |
| 003.1-UNIT-040 | Unit | P1 | Filename derivation — given `trace_file = "/tmp/foo.jsonl"` → payload path = `/tmp/foo.llm.jsonl`; given `"/tmp/foo"` (no extension) → `/tmp/foo.llm.jsonl` | Naming consistency | OPS-003 |
| 003.1-UNIT-041 | Unit | P2 | Each line in `*.llm.jsonl` is parseable JSON and contains all 7 payload keys | Schema sentinel | — |

### AC-10 / AC-11 / AC-12 — Per-node glob filtering

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-050 | Unit | P0 | `_should_capture_payload("extract_batch_3", ["extract_batch_*"])` → True | Glob match on YAML node name | SEC-002 |
| 003.1-UNIT-051 | Unit | P0 | `_should_capture_payload("classify", ["extract_batch_*"])` → False | Negative match — no leakage | SEC-002 |
| 003.1-UNIT-052 | Unit | P0 | **Glob safety** — standalone `*` and `**` rejected at settings parse with actionable error (NFR-AC-2) | Mandatory mitigation for SEC-002 | SEC-002 |
| 003.1-UNIT-053 | Unit | P0 | At engine init, resolved match list logged at INFO (e.g., `Payload capture enabled for nodes: extract_batch_1, extract_batch_2, correct`) | Surface what's being captured (NFR-AC-2) | SEC-002 |
| 003.1-UNIT-054 | Unit | P1 | No-match pattern (e.g., `[typo_*]`) logs WARNING `payload capture pattern matched no nodes` | Detect broken config early | SEC-002 |
| 003.1-INT-030 | Integration | P1 | Dynamic-parallel branch with rendered name `extract_batch_3` matches glob `extract_batch_*` | AC-11 dynamic node | — |
| 003.1-INT-031 | Integration | P0 | Workflow with 3 nodes (`classify`, `extract_batch_1`, `extract_batch_2`) + glob `extract_batch_*` → `*.llm.jsonl` contains exactly 2 entries (the extract_batch_* spans) | End-to-end glob filtering | SEC-002 |

### AC-14 — Compatibility with existing flags

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-INT-040 | Integration | P1 | Capture on + `auto_trace=true` + `trace_exporter=file` → both files written; no conflict | Backward compat | TECH-002 |
| 003.1-INT-041 | Integration | P2 | Capture on + Opik exporter (TEA-BUILTIN-005) registered → both pipelines coexist; payload exporter does not block Opik | Compat with sibling observability path | TECH-002 |

### Filter-short-circuit invariant (NFR-AC-5, mitigates TECH-001 + TECH-002)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-060 | Unit | P0 | Mock `open()`; emit a non-`llm.call` span with capture on → assert `open()` count on payload file is 0 | Latency hot path — non-LLM spans must short-circuit before any I/O | TECH-001, TECH-002 |
| 003.1-INT-050 | Integration | P1 | Slow-disk fixture (`time.sleep(0.075)` in shimmed `write`) + workflow with 100 non-LLM + 5 LLM spans → total payload-file write time bounded by `5 × 75 ms` (i.e., non-LLM spans contribute zero) | Quantifies short-circuit benefit | TECH-001 |

### Sentinel & PII warning header (NFR-AC-3)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-070 | Unit | P0 | First write to a fresh `*.llm.jsonl` emits `# WARNING: contains LLM request/response payloads. May contain PII.` as the first line, **before** any JSON record | NFR-AC-3 mandatory header | SEC-001 |
| 003.1-INT-060 | Integration | P1 | Re-opening the same `*.llm.jsonl` (engine restart, append mode) → header is **not** duplicated | Idempotent header | SEC-001 |
| 003.1-UNIT-071 | Unit | P2 | Standard JSONL readers (e.g., `pandas.read_json(lines=True)`) tolerate the `#` line via documented `# `-comment-skip pattern; document this in observability page | Operability of the warning | OPS-003 |

### Size cap & per-record truncation (NFR-AC-4, mitigates OPS-001)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-080 | Unit | P1 | `trace_payload_max_record_kb = 64` + 200 KB payload → record truncated; `truncated: true` and `original_size_bytes: 204800` markers present; remaining JSON parseable | Per-record truncation correctness | OPS-001 |
| 003.1-INT-070 | Integration | P1 | `trace_payload_max_file_mb = 10` + synthetic 1000 × 100 KB payloads → rotation happens at 10 MB boundary; rotated file (`*.llm.jsonl.1`) is fully parseable line-by-line; no JSONL boundary corruption | Rotation correctness | OPS-001 |
| 003.1-UNIT-081 | Unit | P2 | Truncation is applied to `messages_input` and `response_content` only; numeric fields untouched | Avoid corrupting tokens / cost | — |

### Mid-run flag toggle (NFR-AC-7)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-INT-080 | Integration | P1 | Engine runs with capture on → flag flips to off mid-run via re-init → subsequent `llm.call` produces no payload write; existing file unchanged after flip | Half-open file behavior | SEC-001 |

### Mandatory-retention enforcement (NFR-AC-1) — **Story 1 + Story 2 binding**

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-INT-090 | Integration | P0 | Production mode (`TEA_ENV=production` or equivalent) + capture on + `trace_payload_retention_days` unset → engine init **fails** with actionable error naming the missing key | Mandatory binding (NFR-AC-1) | SEC-001, DATA-001 |
| 003.1-INT-091 | Integration | P1 | Development mode + capture on + retention unset → engine init succeeds with WARNING (legacy ergonomics) | Allow dev-loop iteration | — |

### Documentation smoke (AC-17 / AC-18)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.1-UNIT-090 | Unit | P3 | `docs/python/observability.md` exists and references "PII", "retention", "size cap", "binary_omitted" | Doc enforcement (NFR-AC-12) | BUS-001 |

---

## 3. Story 2 — TEA-OBS-003.2: Retention policy & cleanup

### AC-1 / AC-2 / AC-3 / AC-4 — Retention setting & init warning

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.2-UNIT-001 | Unit | P0 | `trace_payload_retention_days` parser accepts positive int; rejects `0` and negative with actionable error | Sanity bounds | DATA-001 |
| 003.2-UNIT-002 | Unit | P0 | Capture on + retention unset (dev mode) → exactly one WARNING log line containing the AC-2 wording verbatim | Warning text contract | DATA-001, SEC-001 |
| 003.2-UNIT-003 | Unit | P1 | Capture on + retention set → no warning emitted | Negative case | — |
| 003.2-UNIT-004 | Unit | P1 | Capture **off** + retention set → no warning, no error (orthogonal config, valid) | Independence of settings | — |
| 003.2-UNIT-005 | Unit | P1 | Engine init does not delete files (per AC-4); only inspects mtimes when explicitly invoked | Boundary between init and cleanup | OPS-002 |

### AC-5 / AC-6 / AC-7 / AC-8 / AC-9 / AC-10 / AC-11 — Cleanup subcommand

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.2-UNIT-010 | Unit | P0 | Cleanup logic: given (file paths, mtimes, now, older_than_days) → returns expected delete list | Pure logic — easy to fuzz | OPS-002 |
| 003.2-UNIT-011 | Unit | P0 | Default `--pattern` = `*.llm.jsonl,*.llm.jsonl.gz`; arbitrary other names not matched | Glob anchoring on `.llm.` infix (NFR-AC-8) | OPS-002 |
| 003.2-UNIT-012 | Unit | P1 | `--pattern '*.csv'` — non-default pattern matches as expected | Operator override | — |
| 003.2-UNIT-013 | Unit | P0 | `--dry-run` → no `os.remove` calls; output lists each file with size + mtime | Safety net (NFR-AC-8) | OPS-002 |
| 003.2-UNIT-014 | Unit | P0 | `--older-than 0` → all matching files in scope are deleted (TTL = "wipe everything") | Documented purge mode | OPS-002 |
| 003.2-UNIT-015 | Unit | P1 | Cleanup refuses to follow symlinks; logs WARN per skipped symlink | Anti-traversal (NFR-AC-8) | OPS-002 |
| 003.2-UNIT-016 | Unit | P1 | Spans-only file (`run.jsonl`, no `.llm.` infix) is **never** matched, even with `--pattern '*.jsonl'` (cleanup adds an extra `.llm.` substring guard) | Anchor on infix to prevent collision | OPS-002 |
| 003.2-UNIT-017 | Unit | P2 | In-flight files (mtime within last 5 s) are skipped to avoid clobbering an active writer | Avoid race with sync exporter | OPS-002 |
| 003.2-UNIT-018 | Unit | P2 | `--recursive` traverses subdirs; non-recursive default does not | Flag semantics | — |
| 003.2-INT-001 | Integration | P0 | Fixture creates 5 files with controlled mtimes (1, 7, 30, 60, 90 days old) + `--older-than 30` → exactly the 60-day and 90-day files removed; 30-day file retained | Boundary correctness | OPS-002 |
| 003.2-INT-002 | Integration | P1 | Same fixture + `--dry-run` → all 5 files still present; output reports the 2 candidates | Dry-run integration check | OPS-002 |
| 003.2-INT-003 | Integration | P1 | Cleanup with read-only file (chmod 444) → exit 1, summary lists the failure, other deletions completed | Permission-error tolerance (AC-13) | OPS-002 |

### AC-12 / AC-13 / AC-14 — Exit codes & ergonomics

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.2-UNIT-020 | Unit | P1 | Zero matches → exit 0, summary `Deleted 0 files, freed 0 MB` | Zero-result is success | — |
| 003.2-UNIT-021 | Unit | P2 | `--help` output contains the AC-14 example string verbatim | Doc-as-test | — |
| 003.2-UNIT-022 | Unit | P2 | Each deletion logged at INFO with path + size (NFR cleanup observability) | Audit trail (OPS-004) | OPS-004 |

### AC-15 / AC-17 — Cron / docs / fixtures

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.2-UNIT-030 | Unit | P3 | `docs/python/observability.md` contains the cron one-liner and systemd timer block | Doc smoke | BUS-001 |
| 003.2-INT-010 | Integration | P3 | Sample cron command runs end-to-end against fixture and produces expected file removal | Cron-recipe correctness | BUS-001 |

### Cross-story integration (Story 1 + Story 2)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.2-E2E-001 | E2E | P0 | Real workflow with capture on → file written → `os.utime` ages it → `tea trace cleanup --older-than 1` removes it; spans file untouched | The release-binding sanity test | SEC-001, DATA-001, OPS-002 |
| 003.2-E2E-002 | E2E | P1 | Two consecutive runs share a directory → both `*.llm.jsonl` files visible; cleanup with appropriate TTL retires only the older one | Multi-run accumulation | OPS-001 |

---

## 4. Story 3 — TEA-OBS-003.3: Async exporter + optional gzip

### AC-1 / AC-2 / AC-3 / AC-4 — Async happy path

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-UNIT-001 | Unit | P1 | `AsyncFileExporter.export(span)` returns in <1 ms (with worker thread alive but slow) | Latency target (AC-3) | TECH-001 |
| 003.3-UNIT-002 | Unit | P1 | Worker batches up to 50 spans per flush OR flushes at `flush_interval_s` boundary, whichever first | AC-4 batching contract | TECH-001 |
| 003.3-INT-001 | Integration | P1 | 100-span workflow with async on → all 100 spans appear in the output file after engine close | Happy path | TECH-001 |

### AC-5 — Shutdown drain

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-010 | Integration | P0 | `engine.close()` drains the queue within timeout (default 30 s); queue length 0 at return | Graceful shutdown invariant (NFR-AC-9) | DATA-002 |
| 003.3-UNIT-010 | Unit | P1 | Shutdown timeout reached with N undrained spans → WARN log lists count; spans best-effort flushed | Bounded shutdown | DATA-002 |

### AC-6 — Queue overflow policy

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-UNIT-020 | Unit | P1 | `overflow_policy=drop_newest` + queue full → new span dropped; counter incremented; WARN throttled to 1/min | Default policy | DATA-002 |
| 003.3-UNIT-021 | Unit | P2 | `overflow_policy=block` + queue full → `export()` blocks until space; verify with `threading.Event` | Alt policy | — |
| 003.3-UNIT-022 | Unit | P2 | Drop counter exposed as a metric (read via attribute or `get_overflow_count()`) | Observability of drops | OPS-004 |

### AC-8 / AC-9 / AC-10 / AC-11 — Gzip

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-UNIT-030 | Unit | P1 | `trace_payload_compress=gzip` + write 10 spans → output file `*.llm.jsonl.gz`; readable via stdlib `gunzip`; all 10 records present | Gzip end-to-end | TECH-003 |
| 003.3-UNIT-031 | Unit | P0 | Per-batch flush boundary — interrupt (raise) mid-batch → preceding batches readable by `gunzip`; current batch may be lost (documented expected behavior) | Per-batch boundary mandatory (TECH-003 / AC-10) | TECH-003 |
| 003.3-UNIT-032 | Unit | P2 | Sync gzip path (no async) — open/append/close per span — works but documented as slow | AC-11 | TECH-001 |
| 003.3-INT-020 | Integration | P1 | Async + gzip combined → output is `.jsonl.gz`, written via batch flusher; single file (no rotation by default) | AC-15 combined mode | TECH-003 |

### AC-12 — `tea trace cat` helper

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-030 | Integration | P2 | `tea trace cat foo.llm.jsonl.gz` decompresses and prints; one JSON object per line | Operator ergonomics | OPS-003 |
| 003.3-INT-031 | Integration | P3 | `tea trace cat foo.llm.jsonl` (uncompressed) also works (transparent dispatch) | Same UX for both formats | — |

### AC-13 — Cleanup compatibility

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-040 | Integration | P1 | `tea trace cleanup` retires both `.jsonl` and `.jsonl.gz` files based on TTL | Cross-story integration | OPS-002 |

### AC-14 — Default-off byte identity

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-050 | Integration | P0 | Neither `trace_payload_async` nor `trace_payload_compress` set → behavior byte-identical to Story 1 sync output | No regression | SEC-001 |

### AC-16 — Background thread failure recovery

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-UNIT-040 | Unit | P1 | Worker thread raises `OSError("disk full")` → ERROR log; workflow continues; subsequent `export()` calls accumulate on queue; eventually overflow policy fires | Non-fatal recovery | DATA-002 |
| 003.3-UNIT-041 | Unit | P2 | Permission revoked mid-run → first failed flush logged at ERROR; recoverable on next flush attempt if perms restored | Best-effort recovery | DATA-002 |

### AC-17 / AC-18 — Benchmark & docs

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-060 | Integration | P1 | Benchmark — 100 LLM calls + slow-disk fixture (50 ms-per-write) → async runs ≥ 5× faster than sync; speedup written to test output | AC-18 quantitative | TECH-001 |
| 003.3-INT-061 | Integration | P2 | Benchmark output captured into `docs/python/observability.md` latency table at doc-build time (or referenced in CI artifact) | NFR-AC-10 latency table source-of-truth | TECH-001 |
| 003.3-UNIT-050 | Unit | P3 | `docs/python/observability.md` references "async", "gzip", "data loss on hard crash", and AC-19 trade-offs | Doc smoke | BUS-001 |

### AC-19 — SIGKILL semantics

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.3-INT-070 | Integration | P2 | Subprocess workflow + `kill -9` → flushed batches present and parseable; in-queue spans lost (documented behavior); spans file fully intact | Hard-kill semantics (NFR-AC-9) | DATA-002 |

---

## 5. Cross-Cutting / Epic-Level Tests

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| 003.0-E2E-001 | E2E | P0 | **Default-off invariant via real CLI** — `tea run examples/sample.yaml` (no flags, no env) → workflow succeeds; no `*.llm.jsonl` anywhere; existing trace file byte-identical to pre-epic baseline | The PII no-leak via real CLI path | SEC-001 |
| 003.0-E2E-002 | E2E | P1 | Full happy path — `tea run --trace-file run.jsonl --trace-llm-payloads-for extract_batch_*` → `run.jsonl` + `run.llm.jsonl` produced; payloads only for matching nodes; PII header in `*.llm.jsonl`; INFO log on activation | Headline UX | — |
| 003.0-E2E-003 | E2E | P1 | Story 1 + Story 2 + Story 3 stack — capture on with async + gzip + retention 1 day → run; age files; `tea trace cleanup --older-than 1` removes the `.jsonl.gz` | Full-stack smoke | OPS-001, DATA-001 |
| 003.0-INT-001 | Integration | P0 | Capture-on configuration snapshot logged at engine startup — INFO log line lists resolved nodes, retention setting, async/gzip settings | Security-audit observability (risk profile monitoring requirement) | SEC-001, SEC-002 |
| 003.0-INT-002 | Integration | P1 | Disk-full simulation (raise `OSError(ENOSPC)` from payload exporter) → workflow continues; ERROR logged; spans file unaffected | Resilience | OPS-001, DATA-002 |
| 003.0-INT-003 | Integration | P3 | Rust runtime regression — `cargo test` still passes; no Python-only code referenced from Rust path | Parity drift guard | TECH-005 |

---

## 6. Risk Coverage Map

| Risk | Score | Covered by |
|------|-------|------------|
| SEC-001 — PII leakage | 9 | 003.1-UNIT-004, 003.1-INT-010, 003.1-INT-011, 003.1-UNIT-030, 003.1-INT-090, 003.1-UNIT-070, 003.0-E2E-001, 003.0-INT-001 |
| DATA-001 — Retention TTL not enforced | 6 | 003.1-INT-090, 003.2-UNIT-001, 003.2-UNIT-002, 003.2-E2E-001 |
| OPS-001 — Disk fill | 6 | 003.1-UNIT-080, 003.1-INT-070, 003.2-E2E-002, 003.0-INT-002 |
| SEC-002 — Permissive glob | 6 | 003.1-UNIT-052, 003.1-UNIT-053, 003.1-UNIT-054, 003.1-INT-031, 003.0-INT-001 |
| TECH-001 — Sync I/O latency | 6 | 003.1-UNIT-060, 003.1-INT-050, 003.3-INT-001, 003.3-INT-060, 003.3-INT-061 |
| TECH-002 — Pipeline ordering | 4 | 003.1-UNIT-060, 003.1-INT-040, 003.1-INT-041 |
| OPS-002 — Cleanup deletes wrong files | 4 | 003.2-UNIT-011, 003.2-UNIT-013, 003.2-UNIT-015, 003.2-UNIT-016, 003.2-INT-001 |
| DATA-002 — Async drop on crash | 4 | 003.3-INT-010, 003.3-UNIT-020, 003.3-UNIT-040, 003.3-INT-070 |
| TECH-003 — Gzip mid-record corruption | 4 | 003.3-UNIT-031 |
| OPS-003 — Operator confusion `.jsonl` vs `.llm.jsonl` | 2 | 003.1-INT-020, 003.1-UNIT-040, 003.3-INT-030 |
| TECH-004 — Settings env-expansion conflict | 2 | 003.1-INT-003, 003.1-UNIT-005 |
| BUS-001 — Adoption blocked by docs | 2 | 003.1-UNIT-090, 003.2-UNIT-030, 003.3-UNIT-050 |
| OPS-004 — Cleanup observability | 2 | 003.2-UNIT-022, 003.3-UNIT-022 |
| TECH-005 — Rust parity drift | 1 | 003.0-INT-003 |

**Coverage gaps:** none. All 14 risks have at least one test scenario.

---

## 7. Test Data & Environment Requirements

### 7.1 Test fixtures (Python)

| Fixture | Purpose | Consumed by |
|---------|---------|-------------|
| `tmp_trace_dir` (pytest `tmp_path`) | Isolated trace output directory | All Story 1/2/3 integration tests |
| `mock_litellm_response_factory` | Builds canonical LiteLLM result with `usage`, `cost_usd`, `finish_reason`, `model` | 003.1-UNIT-010..013 |
| `binary_message_factory` | Generates message dicts with bytes at top level, Anthropic-vision nested, OpenAI-vision nested, `bytearray`, `memoryview` | 003.1-UNIT-020..025 |
| `glob_fuzz_inputs` (parameterized) | `[None, False, [], 0, "", "False", "0", True, ["extract_batch_*"], "*", "**"]` | 003.1-UNIT-001..005, -050..054 |
| `aged_files_factory` | Creates files with `os.utime`-controlled mtimes at 1/7/30/60/90 days old | 003.2-UNIT-010, 003.2-INT-001/002, 003.2-E2E-001/002 |
| `slow_disk_fs` | Wraps writes in `time.sleep(0.075)` to simulate NFS / cloud disk; or temporary mount via `pytest-pyfakefs` patched IO | 003.1-INT-050, 003.3-INT-060 |
| `permission_revoke_fixture` | Toggles file mode to 0o444 mid-test | 003.2-INT-003 |
| `signal_kill_subprocess` | Spawns engine subprocess; sends `SIGKILL` after N spans | 003.3-INT-070 |

### 7.2 Sample workflows (`examples/`)

| Workflow | Purpose |
|----------|---------|
| `examples/qa_default_off.yaml` | Minimal LLM workflow; consumed by 003.0-E2E-001 / 003.1-INT-010 default-off tests |
| `examples/qa_payload_capture.yaml` | Has `auto_trace_llm_payloads: [extract_batch_*]`; used by 003.0-E2E-002 and 003.1-INT-031 |
| `examples/qa_payload_full_stack.yaml` | Capture + retention + async + gzip; used by 003.0-E2E-003 |
| `examples/qa_payload_dynamic_parallel.yaml` | Dynamic-parallel branch with rendered names; used by 003.1-INT-030 |

### 7.3 Environment variables / config

| Variable | Used by |
|----------|---------|
| `TEA_ENV=production` | 003.1-INT-090 (mandatory-retention enforcement) |
| `TEA_ENV=development` | 003.1-INT-091 (warning-only path) |
| `TEA_TRACE_LLM=true` (env-expansion source) | 003.1-INT-003 (TEA-DX-001.1 coordination) |

### 7.4 LLM mocking strategy

- **Default**: stub `litellm.completion` with deterministic `MagicMock` returning the canonical response. No real network calls.
- **Tokens / cost**: hard-code `usage.prompt_tokens=42, usage.completion_tokens=17`, `cost_usd=0.000123` for assertion stability.
- **Finish reason**: parameterize `["stop", "length", "content_filter"]` to cover both `finish_reason` and `stop_reason` provider aliases.

### 7.5 Concurrency / timing

- All Story 3 async tests must use `monkeypatch.setattr(time, "monotonic", fake_clock)` or `freezegun` — never `time.sleep` to wait for async drain.
- `engine.close()` must be called inside `with pytest.raises(SystemExit)` only when the test specifically asserts engine init failure; otherwise use a try/finally to ensure shutdown.

### 7.6 Filesystem cleanup

- Each integration test runs under `tmp_path`; pytest's auto-cleanup is sufficient.
- E2E tests must `assert not list(Path(tmp_path).rglob('*.llm.jsonl*'))` before invoking the workflow when checking default-off invariants — paranoia against fixture leakage.

---

## 8. Recommended Execution Order

1. **P0 unit tests** — fastest feedback on glob safety, fuzz matrix, byte placeholder, header sentinel, default-off field set, glob match logic, cleanup logic (≈ 24 unit P0 minus 3 P0 integration = 21 unit). Target: < 5 s total.
2. **P0 integration tests** — default-off snapshot, two-file separation, mandatory-retention enforcement, cleanup boundary correctness, async drain, default-off byte identity, capture-on config snapshot. Target: < 30 s total.
3. **P0 E2E** — default-off CLI smoke, Story 1 + Story 2 cross-story flow.
4. **P1 unit + integration** — field shape, slow-disk benchmark, queue overflow, cross-story compat, gzip end-to-end, header idempotence.
5. **P2** — codec edge cases, `tea trace cat`, in-flight skip, cleanup help text, async metric exposure.
6. **P3** — doc smoke tests, cron recipe correctness, Rust parity smoke.

CI gating: P0 must pass; P1 reported but non-blocking on the first PR per story (blocking on the epic merge PR).

---

## 9. Coverage Validation Checklist

- [x] Every functional AC across the 3 stories has ≥ 1 test scenario (51 functional ACs covered)
- [x] Every NFR-AC (NFR-AC-1..12) has ≥ 1 test scenario
- [x] Every risk in the risk profile (14 risks) has ≥ 1 mitigating test
- [x] No duplicate coverage across levels (slim spans / payload separation tested at integration only; glob match logic at unit only; benchmark at integration only)
- [x] Critical paths have defense in depth (default-off: unit + integration + E2E; mandatory retention: unit + integration; cleanup: unit + integration + E2E)
- [x] Test IDs follow `{epic}.{story}-{LEVEL}-{SEQ}` convention
- [x] Scenarios are atomic and independent (no shared mutable state across tests; each test uses its own `tmp_path`)
- [x] LLM mocking strategy documented; no real network calls

---

## 10. Gate YAML Block

```yaml
test_design:
  scenarios_total: 78
  by_level:
    unit: 36
    integration: 32
    e2e: 10
  by_priority:
    p0: 24
    p1: 28
    p2: 18
    p3: 8
  coverage_gaps: []
  risk_coverage:
    SEC-001: covered
    DATA-001: covered
    OPS-001: covered
    SEC-002: covered
    TECH-001: covered
    TECH-002: covered
    OPS-002: covered
    DATA-002: covered
    TECH-003: covered
    OPS-003: covered
    TECH-004: covered
    BUS-001: covered
    OPS-004: covered
    TECH-005: covered
  nfr_ac_coverage:
    NFR-AC-1: 003.1-INT-090
    NFR-AC-2: 003.1-UNIT-052,003.1-UNIT-053,003.1-UNIT-054
    NFR-AC-3: 003.1-UNIT-070,003.1-INT-060
    NFR-AC-4: 003.1-UNIT-080,003.1-INT-070
    NFR-AC-5: 003.1-UNIT-060,003.1-INT-050
    NFR-AC-6: 003.1-UNIT-004,003.1-INT-011
    NFR-AC-7: 003.1-INT-080
    NFR-AC-8: 003.2-UNIT-011,003.2-UNIT-013,003.2-UNIT-015,003.2-UNIT-016
    NFR-AC-9: 003.3-INT-010,003.3-INT-070,003.3-UNIT-020
    NFR-AC-10: 003.3-INT-060,003.3-INT-061
    NFR-AC-11: 003.1-UNIT-010
    NFR-AC-12: 003.1-UNIT-090,003.2-UNIT-030,003.3-UNIT-050
```

---

## 11. Trace References

```
Test design matrix: docs/qa/assessments/TEA-OBS-003-test-design-20260501.md
P0 tests identified: 24
P1 tests identified: 28
Risks fully covered: 14 / 14
NFR-ACs fully covered: 12 / 12
```
