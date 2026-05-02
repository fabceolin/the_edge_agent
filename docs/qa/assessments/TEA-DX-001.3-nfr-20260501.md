# NFR Assessment: TEA-DX-001.3

Date: 2026-05-01
Reviewer: Quinn (Test Architect) — `*nfr-assess` (YOLO / non-interactive mode)
Story: [TEA-DX-001.3 — Intermediate state dumps for debug](../../stories/TEA-DX-001.3-intermediate-state-dumps-debug.md)
Scope assessed: core four (security, performance, reliability, maintainability)

## Summary

- **Security:** **FAIL** — AC-4 mandates "the same redaction logic that excludes secrets from checkpoints," but **no such pass exists**. `checkpoint.py:111` uses `pickle.dump(checkpoint, f)` (raw). The only redaction function in source is `mask_credentials(text: str)` at `python/src/the_edge_agent/cache.py:83` — text-only, intended for log lines, does not traverse state dicts. Checkpoint secrets handling today is by *exclusion* (secrets live in `_secrets_backend`, not in state) — not by redaction. As written, AC-4 cannot be implemented; if implemented literally with the existing code, the dump will faithfully serialize whatever secrets a user happened to put into state. SEC-001 (High in risk profile) is correctly flagged.
- **Performance:** **PASS** — Synchronous JSON write per node is acceptable for a development-only flag. AC-9 disk I/O risk is real but bounded by "off by default." TECH-002 (sync I/O on large states) remains Low.
- **Reliability:** **CONCERNS** — Multiple loose ends:
  - Story Technical Notes refer to a `node_complete` event "or the equivalent" — this event **does not exist at the engine level**. The CLI synthesizes `node_complete` from `event_type == "state"` (`cli.py:1821-1824`). Implementer must hook the engine's `"state"`/`"parallel_state"`/`"branch_complete"` events explicitly.
  - FAILED-write-failure masking original error (TECH-003) is a real concern in the existing `try/except` (`cli.py:1794-2082` wraps the entire run loop).
  - Disk-fill mitigation (DATA-001) accepted as v1, but no warning emission is required.
- **Maintainability:** **CONCERNS** — Story claims "use the existing state-serialization helper" (Task 2). Reusable helpers exist (`json.dump(..., cls=TeaJSONEncoder)` at `cli.py:1838`), but **no per-node snapshot helper exists** today. Implementer must either (a) extract a small `_dump_state(path, state)` helper or (b) inline. The "reuse" language risks hiding a meaningful new code path.

Quality score: **60/100** (100 − 20 FAIL − 10 − 10 = 60).

> Note: The story's risk profile already grades SEC-001 as High and recommends gate **CONCERNS**. This NFR pass is more conservative on Security because the *implementation premise* of AC-4 is incorrect, not just incompletely tested. Once AC-4 is rewritten (see "Acceptance Criteria — Recommendations" below), Security would move to CONCERNS, aligning with the risk profile.

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: FAIL
    notes: 'AC-4 ("same redaction logic used for checkpoints") references a non-existent pass — checkpoint.py:111 uses raw pickle.dump; no state-traversal redaction helper exists. Only mask_credentials (cache.py:83) is text-only. Until AC-4 is rewritten or a redact_state() helper is built, dumps will faithfully serialize any in-state secrets, and FAILED-dump tracebacks may str()-leak repr-of-locals.'
  performance:
    status: PASS
    notes: 'Development-only flag, default off. Sync JSON write per node is acceptable; risk-profile TECH-002 remains Low. No AC required.'
  reliability:
    status: CONCERNS
    notes: 'Engine emits "state"/"parallel_state"/"branch_complete"/"final"/"error" — there is no "node_complete" event (CLI synthesizes it at cli.py:1821-1824). Story Technical Notes handwave with "or the equivalent." Also: FAILED-write failure inside the existing run-loop try/except (cli.py:1794-2082) can mask the original node exception (TECH-003).'
  maintainability:
    status: CONCERNS
    notes: 'Task 2 says "use the existing state-serialization helper" — but no per-node snapshot helper exists; the implementer must either extract one or inline json.dump(..., cls=TeaJSONEncoder). AC-10/AC-11 cover happy path + single-failure path only; redaction, traceback-redaction, parallel-event timing, and read-only-dump-dir negative tests are not at AC level.'
```

## Critical Issues

### 1. AC-4 references a non-existent code path (Security — FAIL)

- **Risk:** SEC-001 (High in risk profile) understates the problem. The story assumes a checkpoint redaction pass exists and can be reused. It does not.
  - `python/src/the_edge_agent/checkpoint.py:111` — `pickle.dump(checkpoint, f)` — raw, no filter.
  - `python/src/the_edge_agent/cache.py:83` — `mask_credentials(text: str)` — string-level regex masking for URL tokens / Bearer tokens; does **not** traverse dicts.
  - `python/src/the_edge_agent/actions/observability_actions.py:83-110` — `sanitize_keys` parameter on the `trace.log` action accepts a *user-supplied* list of keys to redact in a snapshot; not a global pass and not used by checkpointing.
- **Why it matters:** The "secrets are not in state because secrets live in `_secrets_backend`" guarantee holds only for secrets sourced via the secrets backend. If a user assigns a literal API key to a state field (common: `state["openai_key"] = os.getenv("OPENAI_API_KEY")`), checkpoints already store it raw — and `--debug-state` will too. The story's risk-profile mitigation ("verify by code path that redaction reuse covers nested dicts") cannot succeed because there is no pass to verify.
- **Fix path (pick one before merge):**
  - **Option A (preferred):** Build a small `redact_state(state, secret_patterns | secret_keys)` helper applied to both checkpoints and debug dumps. Define what's in scope (key-name allowlist? value-pattern blocklist? both?) and document.
  - **Option B:** Rewrite AC-4 to acknowledge **no redaction**, and gate the flag behind a startup banner that explicitly warns "anything in state is written to disk verbatim — do not run with `--debug-state` against agents that handle keys or PII." This is honest but weaker.
  - **Option C:** Reuse the per-call `sanitize_keys` mechanism: require `--debug-state-redact-keys k1,k2,...`. Less general but matches existing tooling.

### 2. `node_complete` event handwave (Reliability — CONCERNS)

- **Risk:** TECH-001 (Medium in risk profile) is closer to High once you look at the events.
  - StateGraph emits: `"state"` (post-node), `"parallel_state"`, `"branch_complete"`, `"interrupt_before"`, `"interrupt_after"`, `"final"`, `"error"`.
  - The CLI synthesizes the user-visible `node_complete` NDJSON event at `cli.py:1821-1824`.
  - Story says "On every event of type `node_complete` (or the equivalent emitted by `stategraph.py`)" — this is the bug. AC-9 says "snapshots are written when each parent node 'completes', not per branch" — but in parallel runs, each branch emits its own `parallel_state`, and the parent's "complete" is signaled by `parallel_complete` / `branch_complete`, not by a single event.
- **Fix path:** Pin down the event mapping in Task 2 *before* implementation:
  - Sequential nodes → write on `"state"`.
  - Parallel parents → write **once** on `"parallel_complete"` (parent is now resolved); skip writes on per-branch `"parallel_state"`.
  - `dynamic_parallel` → same shape.
  - `"final"` → write only if the user also wants a terminal dump (probably yes, with a sentinel filename like `99-after-FINAL.json`).
  - Document the chosen mapping in Technical Notes.

### 3. FAILED-write masking original error (Reliability — CONCERNS)

- **Risk:** The run-loop's existing `try/except` (`cli.py:1794-2082`) catches `KeyboardInterrupt` and other broad exceptions. Adding a FAILED-state dump inside this block is fine, but if the dump itself raises (read-only dir, disk full, perms) and is not wrapped in its own `try/except`, the user sees the dump-write failure instead of the workflow node exception.
- **Fix path:** Wrap *only* the FAILED-state write in its own `try/except OSError` that logs the dump failure to stderr and re-raises the original node exception. This is risk-profile TECH-003's mitigation, but should be explicit in the AC set.

## Concerns & Missing Considerations

### Maintainability — "existing state-serialization helper" doesn't exist as named

- Task 2 says "Use the existing state-serialization helper." The closest thing is `json.dump(state, f, indent=2, cls=TeaJSONEncoder)` at `cli.py:1838` (one call site, inline, no helper function). Task 2 is implicitly asking the implementer to extract a helper.
- **Fix:** Reword Task 2 to "Extract `_dump_state(path: Path, state: dict, *, redactor: Callable | None) -> None` and call it from both the existing `--output` write and the new `--debug-state` writes." This makes the redaction wiring explicit and centralizes the AC-4 surface.

### Security — traceback redaction scope (SEC-001 sub-issue)

- Even with a redactor, AC-3's FAILED dump includes "the exception traceback." Tracebacks contain `repr()` of locals (when chained from third-party libs that include locals in `__cause__`/`__context__`) and the raised exception's `args`. If a node raises `BadAuth(f"key {api_key} rejected")`, the formatted traceback string contains `api_key` verbatim — and a dict-traversing redactor will not see inside a string.
- **Fix:** Either (a) declare traceback redaction out of scope explicitly in AC-12 and warn in docs, or (b) post-process the traceback string through the same regex-based `mask_credentials()` that already exists. (b) is cheap (~30 min) and closes the canary-leak risk.

### Reliability — production-leak warning (OPS-001)

- AC-12 says the flag is documented "with a warning about redaction guarantees." Documentation is not enforcement. A single `WARNING` log line at startup when `--debug-state` is set, surviving `--quiet`, costs nothing and meaningfully reduces the prod-leak risk.

### Security — path traversal (SEC-002 / DATA-003)

- Filename includes the node name verbatim: `<NN>-after-<node_name>.json`. Node names are author-controlled in YAML, but template-generated parallel item names (`fanout/item-${state["x"]}`) can include arbitrary characters. Sanitize the path component (replace `[^A-Za-z0-9_-]` with `_`) before joining. Risk profile already calls this out at Low.

## Test Recommendations

Risk profile already enumerates P1–P3 tests; NFR-specific must-haves:

| #  | NFR             | Test                                                                                                              | Priority                          |
|----|-----------------|-------------------------------------------------------------------------------------------------------------------|-----------------------------------|
| T1 | Security        | Decision-recorded: AC-4 path chosen (A/B/C above) and a `redact_state` test with a `BadSecret("sk-CANARY-XYZ")` value asserts redaction in the dump | **P0 — blocks merge**             |
| T2 | Security        | Traceback redaction: node raises `RuntimeError(f"key {SECRET}")` → `02-FAILED-...json` does not contain `SECRET`  | **P0** (or document out-of-scope) |
| T3 | Reliability     | Parallel fan-out (3 branches → fan-in): exactly `01-after-fanout.json` + `02-after-fanin.json`, not 5 files       | **P1** (already in profile #5)    |
| T4 | Reliability     | `dynamic_parallel` same shape                                                                                      | P1 (already in profile #6)        |
| T5 | Reliability     | Read-only `--debug-state` dir → original node exception surfaces, not the FAILED-write OSError                     | **P1** (promote from P3)          |
| T6 | Security        | Path traversal: node named `../../etc/passwd` → sanitized filename inside dump dir                                | P1 (already in profile #4)        |
| T7 | Reliability     | OPS-001 enforcement: WARN line on stderr at startup when `--debug-state` is set, including under `--quiet`        | P2                                |
| T8 | Maintainability | Snapshot of `tea run --help` includes `--debug-state` line                                                         | P2                                |
| T9 | Reliability     | No-flag baseline: byte-identical pre-change behavior (`--output` semantics unchanged) (AC-6)                       | P0 (already in profile #11)       |

## Acceptance Criteria — Recommendations

The story already has 12 ACs. Recommend the following before implementation kicks off:

- **Rewrite AC-4** with an explicit redaction contract. Pick one:
  - **AC-4 (Option A):** Debug dumps are passed through a `redact_state(state, secret_keys: list[str])` helper. The same helper is added to checkpoint serialization (`checkpoint.py`). Default `secret_keys` includes `["api_key", "key", "token", "secret", "password", "authorization"]` (case-insensitive). User-overridable via `--debug-state-redact-keys k1,k2,...`. **Traceback strings in FAILED dumps are passed through the existing `mask_credentials()` (`cache.py:83`).**
  - **AC-4 (Option B):** No redaction; debug dumps faithfully serialize state. A startup banner `WARNING: --debug-state enabled — state will be written to disk verbatim, including any secrets in state` is emitted on stderr regardless of `--quiet`.
- **Add AC-13:** Sequential nodes write on `"state"` events; parallel parents write once on `"parallel_complete"`; per-branch `"parallel_state"` and `"branch_complete"` events do **not** trigger separate dumps.
- **Add AC-14:** FAILED-dump write is wrapped in its own `try/except OSError`; if the dump write fails, the original node exception is what propagates.
- **Add AC-15:** Node-name path component is sanitized (replace `[^A-Za-z0-9_-]` with `_`) before joining to the dump dir.
- **Promote AC-12** to require both (a) docs + (b) startup `WARNING` log when `--debug-state` is set, surviving `--quiet`.

## Quick Wins

- Decide AC-4 path (A vs B vs C) and update the story: ~30 min.
- Add `_dump_state()` helper extraction to Task 2: ~20 min refactor + reduces duplication with `--output`.
- Add startup WARN line for `--debug-state`: ~10 min, +T7 test.
- Add path-component sanitizer: ~15 min, +T6 test.
- Wrap FAILED-write in inner `try/except`: ~15 min, +T5 test.
- Run traceback through `mask_credentials()` (Option A only): ~30 min, +T2 test.

## Sign-off

NFR posture is **FAIL on Security** (AC-4 references a non-existent pass) and **CONCERNS on Reliability + Maintainability**. Story is **not safe to implement as written** — the AC-4 redaction premise must be resolved first. After AC-4 is rewritten and AC-13/14/15 are added, this would move to CONCERNS overall (matching the risk profile's CONCERNS recommendation).

Recommended gate: **FAIL → CONCERNS once AC-4 is rewritten and the engine-event mapping (sequential vs parallel) is pinned down in Technical Notes.**

NFR assessment: docs/qa/assessments/TEA-DX-001.3-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.3-intermediate-state-dumps-debug.yml under `nfr_validation`
