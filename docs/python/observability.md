# Observability — LLM Payload Trace Capture (TEA-OBS-003)

> ⚠️ **PII WARNING — read this before enabling capture in production.**
>
> When `auto_trace_llm_payloads` is on, every matched `llm.call` writes
> the **full request messages and response content** to disk. Prompts and
> responses can contain CNPJ / financial data / customer identifiers /
> chat history / API keys. Treat captured files (`*.llm.jsonl`) as
> **sensitive** and configure a retention policy *before* enabling capture
> in any environment that processes real data.

This page covers TEA-OBS-003.1 / 003.2 / 003.3:

- **003.1** — opt-in capture of LLM call payloads to a separate JSONL file.
- **003.2** — TTL-based retention enforcement and `tea trace cleanup`.
- **003.3** — async writer and optional gzip compression for production scale.

## Quick start

```yaml
# agent.yaml
settings:
  # Capture every llm.call's request and response (off by default).
  auto_trace_llm_payloads: true
  # Retention: required in production. Without this, the engine logs a
  # WARNING at init naming the missing key and pointing at this page.
  trace_payload_retention_days: 30
  # Optional but recommended at scale (async writer):
  trace_payload_async: true
  # Optional: shrink files ~10x:
  trace_payload_compress: gzip
  # File location: payloads land in <trace_file>.llm.jsonl[.gz].
  trace_file: ./traces/run.jsonl
  trace_exporter: file

state_schema:
  query: str
  result: str
nodes:
  - name: extract_batch_1
    uses: llm.call
    with:
      model: gpt-4o-mini
      messages:
        - role: user
          content: "{{ state.query }}"
edges:
  - from: __start__
    to: extract_batch_1
  - from: extract_batch_1
    to: __end__
```

Run normally:

```bash
tea run agent.yaml --input '{"query": "summarize"}'
# → ./traces/run.jsonl       (slim spans, no payload)
# → ./traces/run.llm.jsonl   (full payloads, one record per llm.call)
```

`*.llm.jsonl.gz` is produced when `trace_payload_compress: gzip` is set.

## Settings reference

| Setting | Type | Default | Story | Description |
|---------|------|---------|-------|-------------|
| `auto_trace_llm_payloads` | `bool` or `list[str]` | `false` | 003.1 | `false` = off; `true` = capture every node; `list` = match YAML node names by glob. Standalone `*` and `**` are rejected. |
| `trace_payload_retention_days` | `int >= 1` | unset | 003.2 | TTL in days for `tea trace cleanup`. **Required** in production — engine logs a WARNING at init if capture is on without it. |
| `trace_payload_async` | `bool` | `false` | 003.3 | Wraps the payload exporter in a queue + background worker. Trade-off: up to one batch may be lost on `kill -9` / power loss. |
| `trace_payload_compress` | `'gzip'` or unset | unset | 003.3 | Writes `.llm.jsonl.gz` instead of `.llm.jsonl`. Reduces disk usage ~10x for typical text payloads. |

CLI overrides for the run command:

```
tea run agent.yaml --trace-llm-payloads
tea run agent.yaml --trace-llm-payloads-for extract_batch_*
tea run agent.yaml --trace-llm-payloads-for extract_batch_* --trace-llm-payloads-for correct
```

CLI patterns are merged with the YAML list (de-duplicated). `--trace-llm-payloads`
on its own forces capture-everywhere unless the YAML pinned a glob list.

## Captured fields (003.1, AC-4)

The new exporter writes one JSON object per `llm.call`. Inside `metadata`,
the key `llm_payload` carries:

| Field | Description |
|-------|-------------|
| `messages_input` | The full messages array sent to the LLM, with bytes/bytearray entries replaced by `{"type": "binary_omitted", "size_bytes": N}`. |
| `response_content` | The raw `content` string returned by the model. |
| `tokens_input` | `usage.prompt_tokens`. |
| `tokens_output` | `usage.completion_tokens`. |
| `model` | Model identifier (from the LiteLLM/OpenAI response). |
| `stop_reason` | `finish_reason` from the response. |
| `cost_usd` | From `litellm.completion_cost` if available. |

The schema is pinned by `the_edge_agent.tracing.LlmPayloadSpan` (TypedDict).

### Binary omission

To avoid serializing 1.5 MB PDFs / images into JSONL, every bytes-typed
entry inside `messages` is recursively replaced with a placeholder. This
covers Anthropic-style `image.source.data: bytes` blocks, raw bytes at the
top level, `bytearray`, and `memoryview`. Strings (including base64 data
URLs already in string form) pass through unchanged.

### File format

The first line of every `*.llm.jsonl[.gz]` file is the JSONL comment:

```
# WARNING: contains LLM request/response payloads. May contain PII (names, financial data, identifiers). Treat as sensitive.
```

Downstream parsers should skip lines starting with `#`. The remaining lines
are valid JSONL — one object per LLM call.

## Per-node opt-in via globs

```yaml
settings:
  auto_trace_llm_payloads:
    - extract_batch_*
    - correct
```

- Match is `fnmatch.fnmatchcase` against the **YAML node name** (not the
  action name).
- For `dynamic_parallel` branches, the glob is matched against the
  *rendered* branch node name (e.g., `extract_batch_3`).
- Standalone `*` and `**` are **rejected** at engine init — they would
  defeat the per-node opt-in safeguard and silently capture every call.

When capture is enabled, the engine logs at INFO the resolved capture
configuration (the resolved node names or `ALL llm.call nodes`). Surface
this in your security audit trail.

## File location

| Scenario | Output path |
|----------|-------------|
| `trace_file: ./traces/run.jsonl` set | `./traces/run.llm.jsonl` (sibling of the slim trace file) |
| `--trace-file ./out/r.jsonl` (CLI) | `./out/r.llm.jsonl` |
| `trace_file` unset (capture-only mode) | `./tea-llm-payloads-<run_id>.jsonl` in the working directory |

The slim file (`run.jsonl`) **never** contains the `llm_payload` field —
the default `FileExporter` strips it before writing. Existing tools
reading `run.jsonl` see zero change.

## Retention (TEA-OBS-003.2)

Captured payload files accumulate at ~150–300 KB per run; at 1000 runs/day
that is ~200 MB/day. More importantly, prompts may carry PII that should
not live forever on disk. TEA does **not** auto-delete: cleanup is an
explicit operator action via `tea trace cleanup`.

### Choosing a TTL

- **30 days** — general use, moderate sensitivity.
- **7 days** — PII-heavy domains (financial, medical, customer chat).
- **1 day or less** — debugging only; turn capture off when you are done.

Document your choice in your project's compliance notes.

### Cleanup command

```
tea trace cleanup [<dir>] --older-than <days> [--dry-run] [--pattern GLOB] [--recursive]
```

Defaults:
- `<dir>` is the current working directory.
- `--pattern` matches `*.llm.jsonl` and `*.llm.jsonl.gz`.
- `--recursive` is **off** by default. Symlinks are never followed; in
  recursive mode, symlinked sub-trees are skipped.
- `--older-than 0` is **rejected** to prevent racing with active writers.

Examples:

```bash
# Preview what would be deleted (always do this first on a new schedule):
tea trace cleanup ./traces --older-than 30 --dry-run

# Real deletion:
tea trace cleanup ./traces --older-than 30
```

Output (zero matches):

```
Deleted 0 files, freed 0 MB
```

Exit code is 0 even with zero matches (so cron jobs can detect "ran
successfully" by exit code). Per-file failures (permission denied,
file in use) print to stderr and the command exits 1, but other files
are still processed.

### Cron example

```cron
# Run daily at 2am. Note: NO redirect to /dev/null on stderr — that
# is the failure-detection mechanism (cron emails stderr to root).
0 2 * * * tea trace cleanup /var/lib/tea/traces --older-than 30
```

### systemd timer example

`/etc/systemd/system/tea-trace-cleanup.service`:

```ini
[Unit]
Description=Clean up TEA payload traces

[Service]
Type=oneshot
ExecStart=/usr/local/bin/tea trace cleanup /var/lib/tea/traces --older-than 30
User=tea
```

`/etc/systemd/system/tea-trace-cleanup.timer`:

```ini
[Unit]
Description=Daily TEA trace cleanup

[Timer]
OnCalendar=*-*-* 02:00:00
Persistent=true

[Install]
WantedBy=timers.target
```

```
sudo systemctl enable --now tea-trace-cleanup.timer
```

## Async writer (TEA-OBS-003.3)

On slow / networked storage (NFS, cloud disk), each per-span fsync can
add 50–100ms. With 7 LLM calls in an `extract_batch` workflow that's
350–700 ms of pure I/O wait. The async exporter buffers spans on a queue
and flushes in batches in a worker thread:

```yaml
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_payload_async: true
```

### When to enable async (NFR-AC-10)

Per-call sync-write latency is dominated by the storage backend. The
qualitative numbers below are from production deployments and TEA-OBS-003
fault-injection tests:

| Backend | Sync per-`llm.call` latency | When to enable async |
|---------|-----------------------------|----------------------|
| Local SSD (NVMe) | ~1 ms | Not needed |
| Local HDD (spinning) | ~5–10 ms | Optional |
| NFS / network share | ~50–100 ms | **Recommended** |
| Cloud-attached disk (EBS gp3, Persistent Disk) | ~50–200 ms | **Recommended** |
| Object storage via FUSE (gcsfuse, s3fs) | ~100–500 ms | **Required** |

**Decision rule:** enable `trace_payload_async: true` when the per-call
latency on your target backend exceeds **20 ms** OR represents **>5 % of
expected `llm.call` duration** (a 2-second LLM call tolerates a
~100 ms write more readily than a 50 ms `tea.cache`-warmed call).

Measured speedup against the 50 ms/write fixture in
`test_tea_obs003_payload.TestAsyncBenchmark::test_async_meets_5x_speedup_at_50ms_per_write`
runs the same 100-span workload sync vs async and asserts the async ratio
is **≥5×**. The actual measured ratio is emitted to test stdout.

**Trade-off — data loss on hard crash.** On `kill -9` or power loss, up
to one in-flight batch (~5 s of spans by default) may be lost. The async
mode is intentionally opt-in and intended for archival of completed
runs, not for forensic / audit-grade durability.

Graceful shutdown (`engine.close()`) drains the queue within 30 s by
default. The `AsyncFileExporter` exposes `drops`, `queue_depth`,
`flush_count`, `last_flush_ts`, and `worker_alive` for tuning and
monitoring.

### Configuration bounds

`AsyncFileExporter` rejects out-of-range values at construction time so
misconfigurations surface at engine init rather than at OOM:

| Knob | Range | Notes |
|---|---|---|
| `queue_size` | 1 – 100 000 | Values > 10 000 produce a startup WARN (memory ~25 KB/span). |
| `flush_interval_s` | 0.01 – 3600 | Bounds prevent worker-thread thrash and unbounded retention windows. |
| `batch_size` | ≥ 1 | Soft upper bound = `queue_size`. |
| `overflow_policy` | `drop_newest` \| `block` | Other values raise `ValueError`. |

## Gzip compression

```yaml
settings:
  auto_trace_llm_payloads: true
  trace_payload_compress: gzip
```

Output becomes `*.llm.jsonl.gz`. The cleanup command's default pattern
already includes `.gz`. Each batch flush opens, writes, and closes the
gzip stream — that means a `kill -9` mid-batch can lose only the
in-flight batch, not the entire archive.

To inspect:

```bash
tea trace cat ./traces/run.llm.jsonl.gz | jq .
```

`tea trace cat` auto-detects gzip via the `.gz` extension and decompresses
to stdout. Comment lines (PII warning) are preserved verbatim.

## Compatibility

- **Default behaviour is byte-identical to pre-003 TEA** — no payloads
  captured, no new files, slim trace file unchanged. The `FileExporter`'s
  new `strip_llm_payload=True` default is a no-op when no payload is
  present, so existing tooling sees zero change.
- Coexists with `auto_trace`, `trace_exporter`, `trace_file`, the Opik
  exporter, `--trace-file`, and `--from-dot` mode.
- Rust runtime parity is explicitly out of scope for this epic
  (Python-only feature).

## Recommended `.gitignore`

```gitignore
*.llm.jsonl
*.llm.jsonl.gz
tea-llm-payloads-*.jsonl
tea-llm-payloads-*.jsonl.gz
```

Captured payloads are never something you want to accidentally commit.

## See also

- [`docs/stories/TEA-OBS-003-llm-payload-trace-capture-epic.md`](../stories/TEA-OBS-003-llm-payload-trace-capture-epic.md)
  — the parent epic with the full risk profile and NFR notes.
- [`python/src/the_edge_agent/tracing.py`](../../python/src/the_edge_agent/tracing.py)
  — `LlmPayloadFileExporter`, `AsyncFileExporter`, `replace_binary_payloads`.
- [`python/src/the_edge_agent/trace_cleanup.py`](../../python/src/the_edge_agent/trace_cleanup.py)
  — pure cleanup logic exposed for unit tests.
