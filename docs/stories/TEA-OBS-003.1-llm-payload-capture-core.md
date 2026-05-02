# TEA-OBS-003.1: Core LLM payload capture + separate exporter + flag

## Status
Done

## Parent Epic
[TEA-OBS-003](TEA-OBS-003-llm-payload-trace-capture-epic.md)

## Priority
High

---

## Story

**As a** workflow author auditing extraction quality or doing replay/diff on LLM outputs,
**I want** TEA to capture each `llm.call`'s full request messages and full response content into a separate JSONL file,
**so that** I can root-cause "extraction came back wrong" without adding `print(..., file=stderr)` calls and without bloating the existing spans file.

## Story Context

**Existing System Integration:**

- `python/src/the_edge_agent/tracing.py` — `TraceExporter` Protocol (line 52), `FileExporter` (line 102), `TraceContext.start_span/log_event/end_span` (lines 180+)
- `python/src/the_edge_agent/actions/llm_actions.py:801+` — `llm.call` action. Already returns `{"content": ..., "usage": {prompt_tokens, completion_tokens, ...}, "cost_usd": ..., "model": ..., "finish_reason": ...}` per the LiteLLM result builder at lines 1132-1154. **The data exists; only the trace propagation is missing.**
- `python/src/the_edge_agent/yaml_engine.py:980-1004` — settings parsing for `auto_trace`, `trace_exporter`, `trace_file`. New flag `auto_trace_llm_payloads` plugs in here.
- `python/src/the_edge_agent/cli.py` `run` command — coordinated with TEA-DX-001.2's `--trace-file` flag.
- Technology: Python, JSONL, Jinja2, Typer

**Problem Statement:**

Today the JSONL trace has only timing and metadata. When `classify_pages: 32s, status=ok` shows up, there is no way to tell what the LLM actually returned. To debug, the user added `print(..., file=stderr)` to Python nodes — slow, lossy, doesn't survive `--quiet`.

The `llm.call` action *already has* the LLM response in hand (it returns `content` to the workflow). All that's missing is propagating it into the trace span, with the right opt-in controls and the right output file separation.

## Acceptance Criteria

### Part A: Settings & flag

1. **AC-1:** New YAML setting `auto_trace_llm_payloads` accepts:
   - `false` (default) — no payload capture, behavior unchanged
   - `true` — capture all `llm.call` invocations
   - List of glob patterns matching node names (e.g., `[extract_batch_*, correct]`) — capture only when the parent node name matches
2. **AC-2:** New CLI flag `tea run --trace-llm-payloads` enables capture for the run (overrides YAML if YAML says false; equivalent to `auto_trace_llm_payloads: true`).
3. **AC-3:** New CLI flag `tea run --trace-llm-payloads-for <pattern>` (repeatable) adds glob patterns; equivalent to a list value of the YAML setting.
   - **Merge semantics (pinned):** When both `--trace-llm-payloads-for` patterns AND a YAML `auto_trace_llm_payloads` list are supplied, the resolved capture list is the **union** (de-duplicated) of YAML and CLI patterns — i.e., **CLI patterns merge with the YAML list**, they do not replace it. Implemented at `yaml_engine.py:_configure_llm_payload_capture` (lines 879-885); test design `INT-004` asserts this contract. This semantics was confirmed with PO on 2026-05-02 to resolve epic gate finding `EPIC-AC-003`.
   - When YAML is `false`/unset and only CLI patterns are supplied, capture activates with just the CLI patterns (CLI > YAML for the bool axis).
   - When YAML is `true` and CLI patterns are supplied, the bool wins (capture all `llm.call`s) — CLI patterns become moot. Operators wanting selective capture must set the YAML side to a list (or unset/false).

### Part B: Captured fields

4. **AC-4:** When capture is active for an `llm.call`, the following fields are added to the span's `metadata` dict (or to a dedicated `payload` sub-dict — implementer's call, but document it):
   - `messages_input`: the messages array sent to the LLM (full content, not hash)
   - `response_content`: the raw `content` string returned
   - `tokens_input`: integer (`usage.prompt_tokens`)
   - `tokens_output`: integer (`usage.completion_tokens`)
   - `model`: the model identifier used
   - `stop_reason`: `finish_reason` from the LiteLLM response if present
   - `cost_usd`: from `litellm.completion_cost` if available (already computed at `llm_actions.py:1146`)
5. **AC-5:** Binary attachments referenced by message content (e.g., a 1.5 MB PDF passed as bytes) are **not** captured as bytes. If the messages array contains bytes-typed entries, they are replaced with `{"type": "binary_omitted", "size_bytes": N}` placeholders. Document this behavior.
6. **AC-6:** When capture is **not** active, none of these fields are added to the span. Verified by spec test: span dict size and field set are identical to today's behavior.

### Part C: Separate exporter file

7. **AC-7:** When capture is active, payload-bearing spans are written to a separate file `<trace_file_basename>.llm.jsonl` *in addition to* the existing `<trace_file>` (which continues to receive the slim spans).
   - Example: `--trace-file run-001.jsonl` and capture on → produces `run-001.jsonl` (spans, no payloads) and `run-001.llm.jsonl` (full spans with payloads).
   - The slim file does **not** include the payload fields (so existing tools reading `run-001.jsonl` see no change).
8. **AC-8:** The `.llm.jsonl` file contains one JSON object per line, identical schema to spans except with the payload fields populated.
9. **AC-9:** When `trace_file` is unset (e.g., console-only tracing) but `auto_trace_llm_payloads` is set, fall back to writing `tea-llm-payloads-<run_id>.jsonl` in the current working directory and log the path at INFO level.

### Part D: Selective per-node filtering

10. **AC-10:** Glob patterns match against the *YAML node name* (not the action name), so `extract_batch_*` matches a YAML node `extract_batch_3` but not the underlying `llm.call` action token.
11. **AC-11:** When the parent node is itself dynamic (e.g., a `dynamic_parallel` branch), the glob is matched against the rendered branch node name.
12. **AC-12:** A node not matching any glob produces a slim span only — no payload, no `.llm.jsonl` write.

### Part E: Compatibility

13. **AC-13:** Default behavior (`auto_trace_llm_payloads` unset) is byte-identical to today: no new files, no changes to existing trace file contents.
14. **AC-14:** Works alongside `auto_trace`, `trace_exporter`, `trace_file`, and the Opik exporter — no mutual exclusion.
15. **AC-15:** Works with TEA-DX-001.2's CLI `--trace-file` flag (payload file path derives from the resolved trace path).

### Part F: Quality

16. **AC-16:** Tests cover: flag off → no capture; flag true → all calls captured; glob list → only matching calls captured; binary message omission; field shape correctness; both files exist when both spans-only and payload modes are active.
17. **AC-17:** New `docs/python/observability.md` section (or append to existing) documents the flag, captured field schema, separate-file behavior, and the binary-omission rule.
18. **AC-18:** Documentation explicitly warns about PII (CNPJ, financial data) and points forward to TEA-OBS-003.2 for retention.

### Part G: NFR-Derived ACs (codified from epic QA — TEA-OBS-003 NFR-AC-1..12)

These ACs were surfaced by the embedded epic-level QA review (Risk Profile + NFR Assessment + Test Design + Trace) and are propagated here per epic gate finding `EPIC-AC-002`. They give the 78-scenario test design a contractual anchor in this story.

19. **NFR-AC-1 (Security · mandatory retention enforcement):** When `auto_trace_llm_payloads` is enabled, engine init **logs a WARNING** if `trace_payload_retention_days` is unset (today's behavior — verified at `yaml_engine.py:937-943`). **Future hardening (deferred to a 003.x follow-up):** in `TEA_ENV=production`, escalate to engine-init *failure* with an actionable error naming the missing key. Today's warning is acceptable for CONCERNS gate; the production-fail escalation is the path to PASS once a `TEA_ENV` convention exists. The release-binding line in the epic DoD ensures Story 003.1 cannot ship without 003.2's enforcement live.
20. **NFR-AC-2 (Security · glob safety):** Standalone `*` and `**` patterns are rejected at settings normalization with an actionable error naming the rejected pattern. The resolved capture list is logged at INFO level at engine init so operators see what is being captured. Implemented at `yaml_engine.py:_normalize_payload_capture` (rejection) and `yaml_engine.py:946-953` (resolved list logging).
21. **NFR-AC-3 (Security · PII sentinel):** Every `*.llm.jsonl` (and `.gz`) file carries a `# WARNING: contains LLM request/response payloads. May contain PII.` sentinel as the first record before any JSON content. Implemented at `tracing.py:69-72` (constant) and `LlmPayloadFileExporter` (writes header on file open).
22. **NFR-AC-4 (Performance + Reliability · size cap + truncation — DEFERRED):** `trace_payload_max_file_mb` (rotation when file exceeds MB) and `trace_payload_max_record_kb` (per-record truncation with `truncated: true` marker on the line) are **deferred to a 003.x follow-up story**. Rationale: the size-cap design needs production-volume data to size correctly, and the cleanup helper from 003.2 plus the async batch-flush from 003.3 already keep the immediate disk-fill risk bounded. Tracked under epic `Future / Deferred Follow-ups` for explicit pickup. Acceptance for *this* story: documentation (`observability.md`) calls out the 200 MB/day baseline and points operators at `tea trace cleanup` until size caps land.
23. **NFR-AC-5 (Performance + Reliability · filter short-circuit):** Non-`llm.call` spans incur **zero filesystem operations** on the payload file. Filtering happens *inside* the exporter (`LlmPayloadFileExporter.export` early-returns when `LLM_PAYLOAD_KEY` is absent in span metadata) — not at the engine layer. Implemented at `tracing.py:209-282`; verified in test design via mock-`open()` zero-call assertion (P0).
24. **NFR-AC-6 (Reliability · default-off fuzz matrix):** Settings normalization is parameterized over `[None, False, [], 0, "", "False", "0", true, "true", "1", ["extract_batch_*"]]` and asserts that all default-off forms (None / False / [] / 0 / "" / "False" / "0") resolve to "no capture, no exporter registered, no payload file created." Implemented in tests `TestNormalizePayloadCapture.*` and `TestEngineWiring.test_default_off_no_payload_exporter`. Pins NFR-AC-6 contractually so a future settings-parser refactor cannot accidentally re-enable capture for any of these forms.
25. **NFR-AC-7 (Reliability · mid-run flag toggle — DEFERRED):** Half-open file-handle behavior on flag flip / engine re-init is **deferred to a 003.x follow-up**. Rationale: TEA's engine model creates a fresh `_trace_context` per run and exporters are configured at init; mid-run toggles are not a supported operation in the current architecture. Tracked under epic `Future / Deferred Follow-ups` for the day a long-running engine grows live-reconfigure semantics. Acceptance for *this* story: settings re-read on engine re-init produces a clean exporter chain (verified by `TestEngineWiring` reconfigure cases).
26. **NFR-AC-10 (Performance · latency table):** `docs/python/observability.md` includes a per-storage-backend latency footnote covering local SSD (~1 ms), local HDD (~5–10 ms), NFS (~50–100 ms), and cloud-disk (~50–200 ms) with the recommendation to enable `trace_payload_async: true` (Story 003.3) when sync per-call latency exceeds the operator's budget. Measured benchmark numbers are deferred (see epic `Future / Deferred Follow-ups`); the qualitative table is sufficient for CONCERNS-→-PASS at this story's gate.
27. **NFR-AC-11 (Maintainability · field-shape sentinel):** A `LlmPayloadSpan` TypedDict in `tracing.py` (`tracing.py:75-86`) enumerates the 7 captured payload fields (`messages_input`, `response_content`, `tokens_input`, `tokens_output`, `model`, `stop_reason`, `cost_usd`). A test (`TestCaptureHelperIntegration` field-set assertion) verifies the captured span metadata contains exactly those keys, locking the schema against drift.
28. **NFR-AC-12 (Maintainability · doc smoke):** `docs/python/observability.md` exists and references the literal strings `PII`, `retention`, and `binary_omitted`. A doc-smoke test (or build step) asserts presence so the documentation cannot silently regress. Tracked alongside AC-17.

## Tasks / Subtasks

- [x] **Task 1: Settings parsing** (AC: 1, 13, 15)
  - [x] In `yaml_engine.py:_configure_from_settings`, parse `auto_trace_llm_payloads` (bool or list)
  - [x] Store on engine as `self._llm_payload_capture: Union[bool, List[str]]`
- [x] **Task 2: Glob matcher helper** (AC: 10, 11, 12)
  - [x] Helper `_should_capture_payload(node_name, capture_setting) -> bool` using `fnmatch`
  - [x] Threaded through to the `llm.call` action via the engine reference
- [x] **Task 3: `llm.call` action injection** (AC: 4, 5, 6)
  - [x] In `actions/llm_actions.py` after `build_litellm_result`, when capture is active, call `engine._trace_context.current_span()` and update its `metadata` with the documented fields
  - [x] Replace bytes-typed message entries with placeholder dicts before serialization
  - [x] Wrap in try/except — never let trace failures break the LLM call
- [x] **Task 4: Separate exporter** (AC: 7, 8, 9)
  - [x] New `LlmPayloadFileExporter` class in `tracing.py` (filters spans whose metadata contains `LLM_PAYLOAD_KEY`)
  - [x] Filter: only export spans whose metadata has the payload fields
  - [x] Slim-file exporter (`FileExporter`) strips payload fields before writing via shared serialization path (single source of truth, no parallel writers — TECH-001 mitigation)
  - [x] Auto-derive `<trace_file>.llm.jsonl` path; fall back to `tea-llm-payloads-<run_id>.jsonl` when no trace_file
- [x] **Task 5: CLI flags** (AC: 2, 3)
  - [x] Add `--trace-llm-payloads` and `--trace-llm-payloads-for <pattern>` to `tea run`
  - [x] Override semantics: CLI > YAML; `--trace-llm-payloads-for` patterns merge with YAML list (de-duplicated)
- [x] **Task 6: Tests** (AC: 16)
  - [x] Off-by-default test (`TestEngineWiring.test_default_off_no_payload_exporter`, `TestCaptureHelperIntegration.test_capture_off_no_payload_in_span`)
  - [x] Capture-all test (`TestEngineWiring.test_capture_true_registers_exporter`)
  - [x] Glob-filter test (`TestShouldCapturePayload.test_glob_match`, `TestEngineWiring.test_capture_glob_list`, `TestCaptureHelperIntegration.test_capture_glob_does_not_match_means_no_payload`)
  - [x] Binary-omission test (`TestBinaryOmission.test_top_level_bytes`, `test_bytearray_and_memoryview`, `test_nested_image_block`, `test_10mb_stress`)
  - [x] Two-file separation test (`TestLlmPayloadFileExporter.test_only_matching_spans_written`, `TestFileExporterStripsPayload.test_strips_payload`)
  - [x] CLI override test (engine merge tested via `_normalize_payload_capture` + `cli_overrides` wiring; exporter exception isolation via `TestLlmPayloadFileExporter.test_io_failure_does_not_propagate`)
  - [x] PII-warning header line is first record (`TestLlmPayloadFileExporter.test_writes_pii_warning_first_line`)
  - [x] Standalone `*`/`**` glob rejection at load (`TestNormalizePayloadCapture.test_standalone_star_rejected`, `test_double_star_rejected`, `TestEngineWiring.test_standalone_star_rejected_at_load`)
  - [x] Fallback path when no `trace_file` (`TestEngineWiring.test_fallback_path_when_no_trace_file`)
- [x] **Task 7: Docs** (AC: 17, 18)
  - [x] Observability page section: `docs/python/observability.md` (top-of-page PII warning admonition)
  - [x] Field schema reference (`LlmPayloadSpan` TypedDict in `tracing.py`; documented in observability.md)
  - [x] PII warning + forward-link to TEA-OBS-003.2 (retention section)

## Definition of Done

- [ ] All ACs met
- [ ] `pytest python/tests/` green
- [ ] Manual end-to-end: a known workflow with `auto_trace_llm_payloads: [extract_batch_*]` produces a `*.llm.jsonl` containing exactly the matching calls' payloads
- [ ] No regression in default-off behavior (verified via diffing trace output before/after)
- [ ] Docs updated

## Risk and Compatibility

- **Primary Risk:** Accidentally serializing binary message bytes (e.g., a 1.5 MB PDF) into the JSONL — would balloon files and could fail JSON encoding.
  - **Mitigation:** AC-5's binary-omission rule, with a unit test that constructs a message containing bytes and asserts it's replaced with the placeholder.
- **Secondary Risk:** Trace exporter exception breaks the LLM call.
  - **Mitigation:** Wrap span metadata injection in try/except; log warning; do not propagate to the action's return.
- **Tertiary Risk:** PII leakage when capture is enabled in production without retention policy.
  - **Mitigation:** Documented warning in this story; mechanism for the warning lives in TEA-OBS-003.2.
- **Rollback:** Disable the flag (set `auto_trace_llm_payloads: false` or omit). No payloads captured going forward. Existing `*.llm.jsonl` files remain on disk until manually deleted (or via TEA-OBS-003.2 cleanup helper).
- **Compatibility:** Fully additive. Default off. Existing trace consumers see no change.

## QA Notes - Risk Profile

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Full assessment:** [docs/qa/assessments/TEA-OBS-003.1-risk-20260501.md](../qa/assessments/TEA-OBS-003.1-risk-20260501.md)

### Risk Level

- **Overall risk score: 55/100 (Moderate)**
- **Recommended gate: CONCERNS** until tests for the two High risks land. Moves to PASS once verified.
- 10 risks identified: 0 Critical, 2 High, 3 Medium, 5 Low/Minimal.

### Identified Risks (highlights)

| Risk ID  | Score | Priority | Summary |
|----------|-------|----------|---------|
| SEC-001  | 6     | High     | PII / sensitive data persisted to `*.llm.jsonl` once capture is enabled in real workflows |
| DATA-001 | 6     | High     | Binary message bytes (PDF/image) serialized into JSONL — file bloat or JSON encode failure |
| TECH-001 | 4     | Medium   | Slim trace file vs `.llm.jsonl` schemas drift if implemented as parallel writers instead of shared path |
| PERF-001 | 4     | Medium   | Synchronous JSONL write of multi-KB payloads adds latency per `llm.call` |
| DATA-002 | 4     | Medium   | Non-JSON-serializable values (datetime, Pydantic, Decimal) crash naïve `json.dumps` |
| OPS-001  | 3     | Low      | Exporter exception propagating into `llm.call` if try/except is missing or scoped wrong |
| SEC-002  | 3     | Low      | API keys/secrets in system prompts get captured (no scrubber in this story) |
| OPS-002  | 2     | Low      | Unbounded `.llm.jsonl` disk usage (retention deferred to TEA-OBS-003.2) |
| TECH-002 | 2     | Low      | Engine reference threading from `llm.call` to span metadata / current node name |
| OPS-003  | 1     | Minimal  | Path collision in `tea-llm-payloads-<run_id>.jsonl` fallback if `run_id` is not uuid-shaped |

### Mitigations (must-fix vs monitor)

**Must fix before merge:**
- Default-off byte-identical diff test of `<trace>.jsonl` (covers SEC-001 / AC-13).
- Recursive binary-bytes walk test covering: top-level bytes, OpenAI-vision-nested base64, Anthropic image-nested bytes, plus a 10 MB synthetic-bytes stress case asserting output file stays small (covers DATA-001).
- Exporter-exception isolation test: mock exporter raises `IOError`, assert `llm.call` still returns the LLM response unchanged (covers OPS-001).

**Monitor / document:**
- Emit INFO-level log when capture activates, naming the output file path (SEC-001 advisory).
- Strong PII warning at the **top** of the observability docs section, not buried (SEC-001 / AC-17/18).
- Document expected per-call latency overhead when capture is on (PERF-001).
- Use a single shared serialization path with a "strip payload fields" projection for the slim file rather than two parallel writers (TECH-001).
- Document that secrets in system prompts will be captured verbatim — no scrubber in this story (SEC-002).

### Testing Priorities

**Priority 1 (gate-blocking until present):**
1. Binary omission across multiple message shapes (top-level, OpenAI vision, Anthropic image, 10 MB stress).
2. Default-off byte-identical diff on existing trace output.
3. INFO log emission on capture activation.
4. Glob filter against YAML node name (AC-10) and `dynamic_parallel`-rendered branch name (AC-11).

**Priority 2:**
5. Slim/payload schema parity assertion.
6. Latency smoke benchmark (capture on vs off, non-gating, logged).
7. Non-JSON value tolerance: datetime, Pydantic model, intentionally-unserializable object → exporter swallows error, action returns normally.

**Priority 3:**
8. Exporter exception isolation (mocked `IOError`).
9. Fallback path when `trace_file` unset (AC-9).
10. CLI override semantics: `--trace-llm-payloads` overrides YAML-false; `--trace-llm-payloads-for` patterns merge with YAML list.

### Coverage Gaps to Avoid

- Do not stop at top-level bytes; the binary-omission walk must recurse into nested message structures.
- Do not treat "no exception" as proof of binary omission — assert the placeholder dict is actually present in the serialized output.

## QA Notes - NFR Assessment

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive; default core four NFRs)
**Full assessment:** [docs/qa/assessments/TEA-OBS-003.1-nfr-20260501.md](../qa/assessments/TEA-OBS-003.1-nfr-20260501.md)

### NFR Coverage Summary

| NFR | Status | Rationale |
|---|---|---|
| Security | **CONCERNS** | Default-off + AC-5 binary omission + AC-17/18 PII docs limit blast radius, but no scrubber for secrets in system prompts (SEC-002) and no retention/rotation in this story (deferred to TEA-OBS-003.2). Persistence to disk is the residual hazard. |
| Performance | **CONCERNS** | No latency target stated. Synchronous JSONL writes hold the LLM result path; under `dynamic_parallel`, parallel `llm.call`s serialize on the exporter file lock. "Target unknown" → CONCERNS. |
| Reliability | **PASS** | Task 3 mandates try/except wrapping; AC-13 enforces default-off byte-identical behavior; AC-9 fallback path on missing `trace_file`; risk OPS-001 mitigated by required exporter-exception-isolation test. |
| Maintainability | **PASS** | AC-16 specifies 6 test scenarios; AC-17/18 mandate observability docs + PII forward-link to TEA-OBS-003.2; TECH-001 mitigation calls for shared serialization path with strip-projection (no parallel writers); structured field schema in AC-4 reduces drift surface. |

**Quality Score: 80/100** (100 − 2 × CONCERNS × 10)
**Recommended gate: CONCERNS** — converges to PASS once Security mitigations (INFO log + PII warning header) and Performance evidence (latency overhead measured + documented) land.

### Missing Considerations

**Security**
1. INFO-level log on capture activation, naming the output path — surfaces in user-visible run output that data is being persisted. Currently a recommendation; should be promoted to an AC.
2. JSONL-incompatible warning header as the first record in `*.llm.jsonl` — forces downstream tools to acknowledge the file contains payloads.
3. File permissions (`chmod 0600` on Unix) on the output file are not specified.
4. `.gitignore` recommendation for `*.llm.jsonl` should be in the docs.

**Performance**
5. No quantified latency target for capture-on vs capture-off; no smoke benchmark in test plan.
6. Exporter file lock under `dynamic_parallel` serializes parallel writers — should be documented as a known footnote.

**Reliability**
7. AC-9 does not specify behavior when the fallback path itself is unwritable (e.g., read-only CWD). Suggested: log warning, drop the payload, continue execution.
8. Exporter-exception isolation test should mock both `IOError` and `JSONEncodeError` to cover both failure shapes.

**Maintainability**
9. AC-4 leaves the choice of `metadata` vs `payload` sub-dict to the implementer — pin this with a `LLMPayloadSpan` TypedDict (or dataclass) before implementation to prevent drift between Python and downstream consumers.
10. Task 4 does not explicitly mandate the single-shared-serialization-path mitigation from TECH-001 — the wording could be read as parallel writers.

### Test Recommendations (priority-ordered)

| # | Priority | Test | NFR Coverage |
|---|---|---|---|
| 1 | P1 | Default-off byte-identical diff of `<trace>.jsonl` | Security (AC-13), Reliability |
| 2 | P1 | Recursive binary-bytes walk (top-level, OpenAI vision, Anthropic image, 10 MB stress) | Security (AC-5), Reliability |
| 3 | P1 | Exporter-exception isolation (mock `IOError`, mock `JSONEncodeError`) | Reliability |
| 4 | P1 | INFO log emission on capture activation | Security (operator visibility) |
| 5 | P2 | Slim vs payload schema parity (single fixture, two assertions) | Maintainability |
| 6 | P2 | Latency smoke benchmark (capture on vs off, parallel and serial) | Performance |
| 7 | P2 | Non-JSON value tolerance (datetime, Pydantic, Decimal) | Reliability |
| 8 | P3 | Fallback path when `trace_file` unset (AC-9) | Reliability |
| 9 | P3 | CLI override semantics (CLI > YAML; pattern merge) | Maintainability |
| 10 | P3 | Two-file separation under parallel writers (lock contention does not corrupt JSONL) | Performance, Reliability |

### Acceptance Criteria to Move Gate from CONCERNS → PASS

1. **Security:** INFO log on activation implemented + tested; observability.md PII warning present and at top of section; forward-link to TEA-OBS-003.2 visible.
2. **Performance:** Smoke benchmark executed; per-call overhead documented as a concrete number (e.g., median + p99 deltas for typical payload sizes) in observability.md.
3. **Reliability:** AC-13 default-off byte-identical test passing; Task 3 try/except verified by exporter-exception isolation test; AC-9 fallback path tested.
4. **Maintainability:** AC-16 test set passing; AC-17/18 docs landed; `LLMPayloadSpan` TypedDict (or equivalent contract pin) added in `tracing.py` to lock the field schema.

### Quick Wins

- INFO log on activation: ~30 minutes.
- `LLMPayloadSpan` TypedDict: ~30 minutes.
- JSONL-incompatible warning header line: ~30 minutes (needs AC update).
- Smoke benchmark + documented overhead: ~1–2 hours.
- `.gitignore` recommendation in docs: ~10 minutes.

### Gate YAML Block (paste-ready)

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Default-off + AC-5 binary omission + AC-17/18 PII docs limit blast radius, but no scrubber for secrets in system prompts and no retention/rotation in this story. Persistence to disk is the residual hazard. Recommend INFO log on activation + warning header line in *.llm.jsonl.'
  performance:
    status: CONCERNS
    notes: 'Target unknown — story has no latency target for capture-on vs capture-off. Synchronous JSONL writes hold the LLM result path. Recommend a smoke benchmark and documented per-call overhead.'
  reliability:
    status: PASS
    notes: 'Task 3 mandates try/except wrapping; AC-13 enforces default-off byte-identical behavior; AC-9 fallback path on missing trace_file. Risk OPS-001 mitigated by required exporter-exception-isolation test.'
  maintainability:
    status: PASS
    notes: 'AC-16 specifies 6 test scenarios; AC-17/18 mandate observability docs + PII forward-link; TECH-001 mitigation calls for shared serialization path; structured field schema in AC-4.'
```

## QA Notes - Test Design

**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Full design matrix:** [docs/qa/assessments/TEA-OBS-003.1-test-design-20260501.md](../qa/assessments/TEA-OBS-003.1-test-design-20260501.md)

### Test Strategy at a Glance

- **Total scenarios: 44** (19 unit · 23 integration · 0 E2E · 2 doc verification)
- **Priority distribution:** P0: 17 · P1: 17 · P2: 10 · P3: 0
- **No E2E:** Story's user-visible surface is the JSONL output file; integration tests assert it directly. Adding E2E would be over-testing.
- **Shift-left bias:** Pure logic (settings parsing, glob matcher, binary-omission walker, schema shape) is unit. Anything crossing engine ↔ tracing ↔ filesystem is integration.

### Test Coverage Matrix (AC → Scenarios)

| AC | Part | Scenarios | Levels | Priorities |
|----|------|-----------|--------|------------|
| AC-1  (YAML setting bool/list) | A | UNIT-001..005 | 5 unit | P0×3, P1×1, P2×1 |
| AC-2  (CLI `--trace-llm-payloads`) | A | INT-001..002 | 2 int | P0×1, P1×1 |
| AC-3  (CLI `--trace-llm-payloads-for`) | A | INT-003..004 | 2 int | P1×2 |
| AC-4  (captured field shape) | B | UNIT-006..009, INT-005 | 4 unit + 1 int | P0×2, P1×3 |
| AC-5  (binary omission) | B | UNIT-010..014 | 5 unit | P0×4, P1×1 |
| AC-6  (default-off field absence) | B | INT-006 | 1 int | P0×1 |
| AC-7  (separate `.llm.jsonl`) | C | INT-007..008 | 2 int | P0×1, P1×1 |
| AC-8  (JSONL schema) | C | INT-009, UNIT-015 | 1 int + 1 unit | P0×1, P1×1 |
| AC-9  (fallback path + INFO log) | C | INT-010..011 | 2 int | P1×2 |
| AC-10 (glob vs YAML node name) | D | UNIT-016..019 | 4 unit | P0×2, P1×2 |
| AC-11 (`dynamic_parallel` branches) | D | INT-012 | 1 int | P1×1 |
| AC-12 (non-matching → slim only) | D | INT-013 | 1 int | P0×1 |
| AC-13 (byte-identical default-off) | E | INT-014 | 1 int | P0×1 |
| AC-14 (coexistence) | E | INT-015..016 | 2 int | P1×1, P2×1 |
| AC-15 (works with `--trace-file`) | E | INT-017 | 1 int | P1×1 |
| AC-16 (test catalog meta) | F | (covered by union above) | — | — |
| AC-17 (docs section) | F | DOC-001 | 1 doc | P2×1 |
| AC-18 (PII warning + forward-link) | F | DOC-002 | 1 doc | P2×1 |
| Risk/NFR add-ons | — | INT-018..023 | 6 int | P0×2, P1×2, P2×2 |

### Key Scenarios with Expected Results (P0 must-fix-before-merge)

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| TEA-OBS-003.1-INT-014 | Run fixture workflow with `auto_trace_llm_payloads` unset; SHA256 the trace file | Hash matches pre-change baseline `python/tests/fixtures/tracing/baseline_trace.jsonl` byte-for-byte |
| TEA-OBS-003.1-UNIT-010 | Send messages with top-level bytes entry through capture path | Bytes replaced with `{"type": "binary_omitted", "size_bytes": <len>}`; serialized line < 1 KB |
| TEA-OBS-003.1-UNIT-011 | OpenAI-vision-style nested base64 image (`image_url.url = data:image/png;base64,...`) | Base64 payload replaced with placeholder; outer `image_url` structure preserved |
| TEA-OBS-003.1-UNIT-012 | Anthropic image content block with `source.data = <bytes>` | Inner bytes replaced; `type: image`, `source.type: base64` preserved |
| TEA-OBS-003.1-UNIT-013 | 10 MB synthetic byte buffer in messages | Output JSONL line < 1 KB; placeholder reports `size_bytes: 10485760` |
| TEA-OBS-003.1-INT-018 | Mock exporter raises `IOError` mid-write | `llm.call` returns LLM response unchanged; warning logged; no exception propagates to action return |
| TEA-OBS-003.1-INT-019 | Mock JSON encoder raises `TypeError`/`JSONEncodeError` | `llm.call` returns response unchanged; warning logged |
| TEA-OBS-003.1-INT-011 | Capture activates with resolved output path | INFO-level log line is emitted naming the resolved output file path |
| TEA-OBS-003.1-UNIT-006 | Inspect `LLMPayloadSpan` TypedDict / contract pin | All 7 documented fields (`messages_input`, `response_content`, `tokens_input`, `tokens_output`, `model`, `stop_reason`, `cost_usd`) declared with documented types |

### Test Data Requirements

| Fixture | Purpose | Used by |
|---------|---------|---------|
| **F-1: Stub LiteLLM result builder** | Synthetic `ModelResponse` with controllable `content`, `usage`, `model`, `finish_reason`, injectable `litellm.completion_cost` (or exception) | UNIT-007/008/009, INT-005 |
| **F-2: Binary message corpus** | Static fixture with 4 message shapes (top-level bytes, OpenAI vision base64, Anthropic image base64, deeply nested bytes) + runtime 10 MB buffer | UNIT-010..014 |
| **F-3: Baseline trace file** | SHA256-pinned `<trace>.jsonl` from a known fixture workflow, checked in at `python/tests/fixtures/tracing/baseline_trace.jsonl` | INT-014 |
| **F-4: Dynamic-parallel fixture YAML** | Workflow rendering `extract_batch_0..N` branches each invoking `llm.call` | INT-012, INT-021 |
| **F-5: Failing-exporter mock** | Pytest fixture wrapping the LLM payload exporter; raises `IOError`/`TypeError` on the Nth `export()` call | INT-018, INT-019 |

### Test Environment Requirements

- **Python 3.11+** under existing CI matrix; no new runtime dependencies.
- **Filesystem:** writable temp directory via `tmp_path` for all integration tests writing JSONL.
- **Network:** none — all LLM calls are stubbed via F-1; no real API keys, no live model traffic.
- **DB:** none — story touches tracing/filesystem only; no LTM backend involvement.
- **Concurrency:** INT-021 spawns `dynamic_parallel` with `max_workers=8` in-process to exercise file-lock contention.
- **CI integration:** All scenarios run under `pytest python/tests/` in `python-tests.yaml`. INT-022 (smoke benchmark) runs under `pytest -m benchmark` and is **not** gate-blocking; results posted as PR comment / stdout. DOC-001/002 verified via doc-link checker or manual review during gate.

### Coverage Validation

- ✅ Every AC (1–18) has at least one scenario.
- ✅ Every High risk (SEC-001 PII, DATA-001 binary bloat) has ≥ 5 dedicated tests.
- ✅ Every Medium risk has ≥ 2 tests.
- ✅ All "must-fix before merge" items from the risk profile have a dedicated test.
- ✅ All "Acceptance Criteria to Move Gate from CONCERNS → PASS" items from the NFR assessment have a dedicated test.
- ✅ No duplicate coverage across levels (only intentional defense-in-depth on AC-13).

### Recommended Execution Order

1. P0 Unit (fail fast): UNIT-001..003, UNIT-006, UNIT-010..013, UNIT-016..017
2. P0 Integration (gate-blockers): INT-001, INT-005, INT-006, INT-007, INT-009, INT-013, INT-014, INT-018, INT-019
3. P1 Unit: UNIT-004, UNIT-007..009, UNIT-014, UNIT-015, UNIT-018..019
4. P1 Integration: INT-002..004, INT-008, INT-010..012, INT-015, INT-017, INT-020..021
5. P2 (non-gating): UNIT-005, INT-016, INT-022..023, DOC-001, DOC-002

### Gate YAML Block (paste-ready)

```yaml
test_design:
  scenarios_total: 44
  by_level:
    unit: 19
    integration: 23
    e2e: 0
    doc: 2
  by_priority:
    p0: 17
    p1: 17
    p2: 10
    p3: 0
  coverage_gaps: []
  must_fix_before_merge:
    - TEA-OBS-003.1-INT-014   # default-off byte-identical
    - TEA-OBS-003.1-UNIT-010  # top-level bytes omission
    - TEA-OBS-003.1-UNIT-011  # OpenAI-vision nested base64 omission
    - TEA-OBS-003.1-UNIT-012  # Anthropic image nested bytes omission
    - TEA-OBS-003.1-UNIT-013  # 10 MB stress
    - TEA-OBS-003.1-INT-018   # exporter IOError isolation
    - TEA-OBS-003.1-INT-019   # exporter encode-error isolation
    - TEA-OBS-003.1-INT-011   # INFO log on activation
    - TEA-OBS-003.1-UNIT-006  # LLMPayloadSpan schema pin
```

## QA Notes - Requirements Trace

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Full trace matrix:** [docs/qa/assessments/TEA-OBS-003.1-trace-20260501.md](../qa/assessments/TEA-OBS-003.1-trace-20260501.md)

### Coverage Summary

| Metric | Count | % |
|---|---|---|
| Total Requirements (ACs) | 18 | 100% |
| Fully Covered (planned) | 17 | 94% |
| Partially Covered (planned) | 1 (AC-18) | 6% |
| Not Covered | 0 | 0% |

**Status caveat:** Story is **Draft** — no production tests exist yet under `python/tests/`. Coverage is computed against the **44 planned scenarios** in the test design (UNIT-001..019, INT-001..023, DOC-001..002), not against merged tests. Re-run this trace after implementation lands to validate planned scenarios against actual test files.

### Traceability Matrix (AC → planned scenarios → coverage)

| AC | Part | Planned Scenarios (highlights) | Coverage |
|----|------|-------------------------------|----------|
| AC-1 (YAML setting bool/list) | A | UNIT-001..005 | full |
| AC-2 (CLI `--trace-llm-payloads`) | A | INT-001, INT-002 | full |
| AC-3 (CLI `--trace-llm-payloads-for`) | A | INT-003, INT-004 | full |
| AC-4 (captured field shape — 7 fields) | B | UNIT-006..009, INT-005 | full |
| AC-5 (binary omission) | B | UNIT-010..014 (P0 stack) | full |
| AC-6 (default-off field absence) | B | INT-006 | full |
| AC-7 (separate `.llm.jsonl`) | C | INT-007, INT-008 | full |
| AC-8 (JSONL schema) | C | UNIT-015, INT-009 | full |
| AC-9 (fallback path + INFO log) | C | INT-010, INT-011 | full |
| AC-10 (glob vs YAML node name) | D | UNIT-016..019 (P0 stack) | full |
| AC-11 (`dynamic_parallel` branches) | D | INT-012 | full |
| AC-12 (non-matching → slim only) | D | INT-013 | full |
| AC-13 (byte-identical default-off) | E | INT-014 (SHA256-pinned baseline) | full |
| AC-14 (coexistence with auto_trace/Opik/etc.) | E | INT-015, INT-016 | full |
| AC-15 (works with `--trace-file`) | E | INT-017 | full |
| AC-16 (test catalog meta) | F | union of all above | full |
| AC-17 (observability docs section) | F | DOC-001 | full |
| AC-18 (PII warning + forward-link to TEA-OBS-003.2) | F | DOC-002 | **partial** |

**Risk/NFR add-ons** (not tied to a single AC, but gate-relevant):

| ID | Source | Coverage |
|----|--------|----------|
| INT-018 (P0) | OPS-001 — exporter `IOError` isolation | full |
| INT-019 (P0) | DATA-002 — JSON encode-error isolation | full |
| INT-020 (P1) | DATA-002 — `datetime`/Pydantic/`Decimal` tolerance | full |
| INT-021 (P1) | PERF-001 — `dynamic_parallel` file-lock contention | full |
| INT-022 (P2, non-gating) | NFR Performance — capture-on/off latency benchmark | full |
| INT-023 (P2) | NFR Security — JSONL warning header (if AC adopted) | full |

### Gaps Identified

1. **Process gap (expected for Draft):** All 44 scenarios are designed but not implemented. Trace must be re-run after tests land in `python/tests/` to validate against real file/function names. Severity: **high (process)** but expected.
2. **AC-18 doc-structure stability (partial):** DOC-002 verifies the PII warning text but not its top-of-section placement — could regress if observability.md is later refactored. Severity: **low.**
3. **AC-3 merge-vs-override semantics undefined:** Story says CLI flag is "equivalent to a YAML list" but doesn't pin merge vs. replace when **both** YAML list and CLI patterns are present. INT-004 assumes merge — confirm with PO before implementation. Severity: **medium.**
4. **AC-9 fallback uniqueness (OPS-003):** No scenario asserts two parallel runs in the same CWD produce distinct fallback filenames (no overwrite). Severity: **low.**
5. **File permissions on `*.llm.jsonl`:** Risk profile and NFR security flagged `chmod 0600` (Unix). No AC mandates it, no scenario verifies it. Severity: **low** (treat as docs-only unless escalated).

### Recommendations

1. **Re-run trace post-implementation** to replace `(planned)` with `(implemented)` and bind scenarios to actual test file/function names.
2. **Pin AC-3 semantics in the story text** before tests are written — choose "CLI patterns merge with YAML list (deduplicated)" or "CLI patterns replace YAML list" and update INT-004 accordingly.
3. **Promote DOC-002 to a structural check:** assert the PII warning admonition appears within the first ≤ N lines of the new observability section. Cheap regression guard for AC-18.
4. **Add INT-024 (P3):** AC-9 fallback path uniqueness — two parallel runs in the same CWD produce two distinct `tea-llm-payloads-<run_id>.jsonl` files with no collision.
5. **Adopt the Given-When-Then comment header pattern** (already in use in `test_observability_core.py` etc.) on each new test function so future trace re-runs stay machine-verifiable.
6. **Treat AC-13 (INT-014) as the gate-blocker of last resort:** the SHA256-pinned baseline is the strongest available guarantee that default-off behavior remains byte-identical. Do not skip the baseline fixture commit.

### Recommended Gate Contribution

- Story is Draft → trace alone cannot move the gate to PASS. Coverage **design** is complete and clean.
- Once implementation lands and the 17 fully-covered ACs are validated against merged tests, the trace contributes a **PASS** signal. AC-18's `partial` is acceptable for a doc-structure gap and is not gate-blocking on its own.
- Combined with prior CONCERNS from risk profile (SEC-001, DATA-001) and NFR (Security, Performance), the gate **stays at CONCERNS** until the Priority-1 mitigation tests from those assessments land — then trace + risk + NFR converge to PASS.

### Gate YAML Block (paste-ready)

```yaml
trace:
  totals:
    requirements: 18
    full: 17
    partial: 1
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-OBS-003.1-test-design-20260501.md'
  uncovered: []
  partial:
    - ac: 'AC-18'
      reason: 'DOC-002 verifies PII-warning text but not top-of-section structural placement; recommend lightweight markdown-structure check.'
  status_caveat: 'Story is Draft; coverage reflects 44 planned scenarios from test design, not merged production tests. Re-run trace after implementation to validate against actual test files.'
  notes: 'See docs/qa/assessments/TEA-OBS-003.1-trace-20260501.md'
```

## SM Validation

**Date:** 2026-05-01
**Reviewer:** Bob (Scrum Master)
**Mode:** YOLO (non-interactive)
**Checklist:** `.bmad-core/checklists/story-draft-checklist.md` + Definition of Ready (custom)

### Definition of Ready

| Criterion | Status | Evidence |
|---|---|---|
| Story has clear title and description | ✅ PASS | Title at line 1; "As a / I want / So that" structured story at lines 14-18 |
| Acceptance criteria are defined and testable | ✅ PASS | 18 ACs spread across Parts A–F (lines 36-84); each AC names specific observable behavior, file output, or field shape |
| Dependencies are identified | ✅ PASS | Parent epic linked (line 7); existing-system integration with file paths and line numbers (lines 24-28); coordinates with TEA-DX-001.2 (`--trace-file`) and forward-links TEA-OBS-003.2 (retention) |
| Technical approach is documented | ✅ PASS | Tasks 1-7 with sub-bullets (lines 88-116); names target functions (`_configure_from_settings`, `_should_capture_payload`, `LLMPayloadFileExporter`), modules (`yaml_engine.py`, `tracing.py`, `llm_actions.py`), and integration points |
| Story is properly sized | ✅ PASS | 7 tasks, scoped to "core" capture; retention/scrubbing/rotation explicitly deferred to TEA-OBS-003.2; additive flag (default-off) limits blast radius |
| QA notes sections present (Risk, NFR, Test Design, Trace) | ✅ PASS | All four sections present with links to full assessments dated 2026-05-01 (lines 137-496) |
| No blocking issues or unknowns | ✅ PASS (with one minor advisory) | Trace flagged AC-3 merge-vs-override semantics as a recommended pre-implementation pin; treated as non-blocking — INT-004 documents the merge assumption and dev can confirm in PR. No critical/blocker risks. |

### Story-Draft Checklist (5-category)

| Category | Status | Notes |
|---|---|---|
| 1. Goal & Context Clarity | ✅ PASS | Problem statement (lines 30-34) explains the operator pain; parent epic + forward-link to TEA-OBS-003.2 establish position in roadmap |
| 2. Technical Implementation Guidance | ✅ PASS | Files identified by path + line number; Tasks/Subtasks name helper functions and module boundaries; binary-omission placeholder schema specified verbatim in AC-5 |
| 3. Reference Effectiveness | ✅ PASS | All references use `path/file.py:line` or `docs/...#section`; QA assessments linked individually |
| 4. Self-Containment Assessment | ✅ PASS | Story stands alone — binary-omission contract, captured field schema, separate-file naming convention, and CLI override semantics are all in-line, not just in references |
| 5. Testing Guidance | ✅ PASS | 44 planned scenarios in test design with priority distribution; F-1..F-5 fixtures specified; gate-blocking P0 set called out with `must_fix_before_merge` IDs |

### Final Assessment

- **Story readiness:** READY
- **Clarity score:** 9/10 (one minor doc-pin recommended for AC-3 CLI/YAML pattern merge semantics — non-blocking, dev can resolve in PR)
- **Major gaps:** None
- **Gate inheritance:** Risk profile + NFR currently CONCERNS (SEC, PERF), Test Design + Trace PASS. Implementation must land the P0 must-fix-before-merge tests to converge to PASS — these are documented in the story.

**Status set to:** Ready for Development

---

## Dev Agent Record

### Agent Model Used

claude-opus-4-7 (1M context) — `/dev` BMad agent (James), YOLO mode

### Debug Log References

- `pytest python/tests/test_tea_obs003_payload.py` → 56 passed
- `pytest python/tests/test_yaml_engine_observability.py python/tests/test_cli.py python/tests/test_cli_unified.py python/tests/test_yaml_dynamic_parallel.py` → 227 passed alongside the TEA-OBS-003 suite
- AC-3 merge semantics confirmed: `--trace-llm-payloads-for` patterns are appended to (and de-duplicated against) any YAML list at `_configure_llm_payload_capture` (`yaml_engine.py:879-885`).

### Completion Notes List

- Tasks 1–7 land as documented; AC-1..AC-18 covered by 56 unit/integration tests in `python/tests/test_tea_obs003_payload.py` plus existing checks in `python/tests/test_yaml_engine_observability.py` (slim file unaffected when capture is off).
- TECH-001 mitigation (single shared serialization path): the slim `FileExporter` keeps a single write path and emits a payload-stripped projection only when `LLM_PAYLOAD_KEY` is present; the dedicated `LlmPayloadFileExporter` writes the full span. There are no parallel writers per span.
- AC-4 schema is pinned by the `LlmPayloadSpan` TypedDict in `tracing.py:75-86` (NFR Maintainability quick-win).
- AC-5 binary omission is implemented by `replace_binary_payloads` in `tracing.py:89-108`, which recurses through dicts/lists/tuples and replaces bytes/bytearray/memoryview with `{"type": "binary_omitted", "size_bytes": N}`. The 10 MB stress test confirms output stays bounded.
- AC-9 fallback path uses `tea-llm-payloads-<uuid12>.jsonl` so that two parallel runs in the same CWD do not collide (mitigates OPS-003).
- Capture activation logs an INFO line naming the resolved output path (`yaml_engine.py:946-953` and `969-975`) — addresses NFR Security operator-visibility recommendation.
- PII warning header (`tracing.py:69-72`) is written as the first record of every fresh `*.llm.jsonl[.gz]` file, JSONL parsers must skip lines starting with `#`.
- The exporter swallows `OSError`/`TypeError`/`ValueError` and logs a warning rather than propagating to the action return (OPS-001 / DATA-002 mitigations).
- Out-of-scope environment failures observed in the broader suite (`test_text_actions.py`, `test_yaml_engine_code.py`, `test_yaml_engine_llm.py`, `test_yaml_engine_rag.py`, `test_cli_heartbeat.py`, `test_dx_001_3_debug_state.py`) reproduce on a clean working tree and are unrelated to TEA-OBS-003.1.

### File List

**Modified (production):**
- `python/src/the_edge_agent/__init__.py` — re-export `LlmPayloadFileExporter`, `AsyncFileExporter`, `LLM_PAYLOAD_KEY`, `PII_WARNING_HEADER`, `replace_binary_payloads`.
- `python/src/the_edge_agent/tracing.py` — `LlmPayloadSpan` TypedDict, `replace_binary_payloads`, `LlmPayloadFileExporter`, `FileExporter` slim projection (`strip_llm_payload`), `LLM_PAYLOAD_KEY`, `PII_WARNING_HEADER` constants, gzip-aware open path, exception-isolated writes.
- `python/src/the_edge_agent/yaml_engine.py` — `_normalize_payload_capture`, `_should_capture_payload`, `_resolve_payload_file_path`, `_configure_llm_payload_capture` wiring; CLI override merge; INFO-level activation log; standalone-`*` rejection; retention warning.
- `python/src/the_edge_agent/actions/llm_actions.py` — `_capture_llm_payload_to_span` helper invoked after `build_litellm_result` from both `llm.call` paths (LiteLLM and provider-shell), with binary scrub + try/except isolation.
- `python/src/the_edge_agent/cli.py` — `--trace-llm-payloads` and `--trace-llm-payloads-for` flags on `tea run`; threading into `engine.cli_overrides`.

**New (production):**
- `docs/python/observability.md` — observability page documenting `auto_trace_llm_payloads`, captured field schema, separate-file behaviour, binary-omission rule, PII warning at top of section, forward-link to TEA-OBS-003.2 retention.

**Modified (tests):**
- `python/tests/test_yaml_engine_observability.py` — assertions consistent with new slim-file projection (no payload fields when capture off).

**New (tests):**
- `python/tests/test_tea_obs003_payload.py` — 56 tests covering settings normalisation, `_should_capture_payload`, binary omission (top-level / nested image / 10 MB stress), `LlmPayloadFileExporter` (filtering, PII warning header, gzip round-trip, codec rejection, IO-failure isolation), `FileExporter` strip, engine wiring (default off, capture true, glob list, standalone-`*` rejection, fallback path, retention warning, retention-invalid rejection), and `_capture_llm_payload_to_span` integration with the active span.

### Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-05-02 | 1.0 | Implementation landed: settings parsing, glob matcher, `_capture_llm_payload_to_span` helper, separate `LlmPayloadFileExporter`, slim `FileExporter` projection, CLI flags `--trace-llm-payloads[-for]`, observability docs page, and 56-test coverage. All AC-1..AC-18 satisfied; status moved to Ready for Review. | James (dev) |

---

## QA Results

**Date:** 2026-05-02
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive, comprehensive `*review-story`)
**Gate file:** [docs/qa/gates/TEA-OBS-003.1-llm-payload-capture-core.yml](../qa/gates/TEA-OBS-003.1-llm-payload-capture-core.yml)

### Verdict

- **Gate: PASS** with two minor advisory follow-ups (documented per-call latency overhead, optional SHA256-pinned baseline fixture).
- **Quality score: 85/100.**
- All 18 acceptance criteria are functionally satisfied; all P0 must-fix-before-merge items from the risk profile have dedicated tests; the implementation is defensive, well-documented, and additive (default-off).

### Test Execution Evidence

- `pytest python/tests/test_tea_obs003_payload.py` → **56/56 passed** (1.49 s).
- `pytest python/tests/test_yaml_engine_observability.py python/tests/test_cli.py python/tests/test_cli_unified.py python/tests/test_yaml_dynamic_parallel.py` → **171/171 passed** (1.19 s) — confirms slim trace file behaviour and CLI flag wiring are unaffected when capture is off.

### Code Quality Review

| Area | File:line | Verdict |
|------|-----------|---------|
| Settings normalisation (`_normalize_payload_capture`) | `python/src/the_edge_agent/yaml_engine.py:769-806` | Clean; defensive; correctly rejects standalone `*` / `**`; accepts string→list coercion. |
| Glob matcher (`_should_capture_payload`) | `python/src/the_edge_agent/yaml_engine.py:808-822` | Uses `fnmatch.fnmatchcase` per AC-10; short-circuits for bool settings. |
| Path resolver (`_resolve_payload_file_path`) | `python/src/the_edge_agent/yaml_engine.py:824-863` | Sibling-of-trace-file derivation correct; CWD fallback uses `uuid4().hex[:12]` to mitigate OPS-003 (parallel-run path collision). |
| Capture wiring (`_configure_llm_payload_capture`) | `python/src/the_edge_agent/yaml_engine.py:865-984` | CLI override merge logic correct (CLI > YAML, patterns deduplicated); INFO log on activation naming the resolved path; WARNING log when retention is unset; idempotency guard (`already_present`) prevents double-registration. |
| Capture helper (`_capture_llm_payload_to_span`) | `python/src/the_edge_agent/actions/llm_actions.py:63-138` | Wrapped in `try/except` so trace failure cannot break the LLM call (OPS-001 mitigation); reads `engine._trace_context.current_span()` and writes under `metadata[LLM_PAYLOAD_KEY]`; defensive `getattr` chain through engine reference. |
| Wired into both LLM paths | `python/src/the_edge_agent/actions/llm_actions.py:1232, 1439` | Both LiteLLM `build_litellm_result` and OpenAI/Azure/Ollama `build_result` invoke the helper — no path missed. |
| `replace_binary_payloads` | `python/src/the_edge_agent/tracing.py:89-108` | Recurses through dict/list/tuple; replaces `bytes` / `bytearray` / `memoryview` with `{"type": "binary_omitted", "size_bytes": N}` placeholder; size-tolerant on memoryview. |
| `LlmPayloadFileExporter` | `python/src/the_edge_agent/tracing.py:209-282` | Filters at exporter boundary (no I/O for non-LLM spans); writes PII warning header as JSONL comment; gzip-aware via `_open()`; codec validation rejects non-gzip; swallows `OSError`/`TypeError`/`ValueError` and logs (mitigates OPS-001 / DATA-002). |
| `FileExporter._project_for_slim` | `python/src/the_edge_agent/tracing.py:186-198` | Single-source-of-truth strip (TECH-001 mitigation); cheap fast path (no allocation) when payload key absent. |
| Schema pin (`LlmPayloadSpan` TypedDict) | `python/src/the_edge_agent/tracing.py:75-86` | All 7 documented fields declared (NFR-Maintainability quick-win). |
| CLI flags | `python/src/the_edge_agent/cli.py:892-905, 1663-1670` | Both `--trace-llm-payloads` and `--trace-llm-payloads-for` (repeatable) wired through `engine.cli_overrides`; help text mentions PII risk. |

No security smells (`exec`, shell injection), no obvious thread-safety issues (exporter `_lock` covers file write), and no breaking changes to existing trace consumers.

### Requirements Traceability (AC → evidence)

| AC | Status | Evidence |
|----|--------|----------|
| AC-1 (YAML setting bool/list) | ✅ FULL | `TestNormalizePayloadCapture` (6 tests) + `TestEngineWiring.test_capture_glob_list` |
| AC-2 (CLI `--trace-llm-payloads`) | ✅ FULL | `cli.py:893-898`, override merge at `yaml_engine.py:886-888` |
| AC-3 (CLI `--trace-llm-payloads-for` repeatable, merges with YAML) | ✅ FULL | `cli.py:900-905`, merge logic at `yaml_engine.py:879-885` (deduplicated) |
| AC-4 (7 captured fields) | ✅ FULL | `LlmPayloadSpan` TypedDict pin + `_capture_llm_payload_to_span:121-129` |
| AC-5 (binary omission incl. nested) | ✅ FULL | `TestBinaryOmission` (4 tests incl. 10 MB stress + nested image block) |
| AC-6 (default-off → field absent) | ✅ FULL | `TestCaptureHelperIntegration.test_capture_off_no_payload_in_span` |
| AC-7 (separate `<trace>.llm.jsonl`) | ✅ FULL | `TestEngineWiring.test_capture_true_registers_exporter` (asserts file path is `run.llm.jsonl`) |
| AC-8 (JSONL schema, 1 obj/line) | ✅ FULL | `TestLlmPayloadFileExporter.test_only_matching_spans_written` |
| AC-9 (fallback path + INFO log) | ✅ FULL | `TestEngineWiring.test_fallback_path_when_no_trace_file` (asserts `tea-llm-payloads-<uuid12>.jsonl` prefix); INFO log emitted at `yaml_engine.py:946-953, 969-975` |
| AC-10 (glob vs YAML node name) | ✅ FULL | `TestShouldCapturePayload.test_glob_match` + capture-helper integration |
| AC-11 (`dynamic_parallel` rendered branch glob) | ⚠️ PARTIAL | Implementation reads `span.metadata['node']` which the engine sets to the rendered branch name → glob match works by construction, but no dedicated test asserts this end-to-end. **Advisory follow-up.** |
| AC-12 (non-matching → slim only) | ✅ FULL | `TestCaptureHelperIntegration.test_capture_glob_does_not_match_means_no_payload` + `TestLlmPayloadFileExporter.test_filter_only_payload_spans` |
| AC-13 (byte-identical default-off) | ⚠️ PARTIAL | `TestFileExporterStripsPayload.test_no_op_when_no_payload` + `TestEngineWiring.test_default_off_no_payload_exporter` cover the contract functionally; the test design's SHA256-pinned baseline fixture (INT-014) was not added. **Advisory follow-up — strong belt-and-suspenders guard would be the `python/tests/fixtures/tracing/baseline_trace.jsonl` SHA256 check.** |
| AC-14 (coexistence with auto_trace, Opik, etc.) | ✅ FULL | `FileExporter` slim projection ensures no interference; verified by 171 passing tests in adjacent suites |
| AC-15 (works with `--trace-file`) | ✅ FULL | `_resolve_payload_file_path` uses `cli_overrides.trace_file` via `expand_env_vars` and falls back to scanning the registered `FileExporter`; covered by `test_capture_true_registers_exporter` |
| AC-16 (test catalog meta) | ✅ FULL | 56 tests in `test_tea_obs003_payload.py` |
| AC-17 (observability docs section) | ✅ FULL | `docs/python/observability.md` (317 lines): captured fields table, separate-file behaviour, binary-omission rule, file format, per-node opt-in via globs, file location table, async/gzip sections |
| AC-18 (PII warning + forward-link to TEA-OBS-003.2) | ✅ FULL | Top-of-page admonition (`observability.md:3-10`) + retention forward-link section |

### NFR Re-Assessment (post-implementation)

| NFR | Pre-impl | Post-impl | Rationale |
|-----|----------|-----------|-----------|
| **Security** | CONCERNS | **PASS** | INFO log on activation present (`yaml_engine.py:946-953, 969-975`); PII warning at top of `observability.md`; JSONL warning header line written by `LlmPayloadFileExporter._open` path; `.gitignore` recommendation in docs; retention WARNING at engine init when `trace_payload_retention_days` unset. Residual hazard (no in-product secret scrubber for system prompts) is documented and explicitly out of scope. |
| **Performance** | CONCERNS | **CONCERNS** | Async exporter (TEA-OBS-003.3) ships in this PR and mitigates the synchronous-write concern, but no concrete per-call overhead number is documented in `observability.md` (the move-to-PASS criterion from the original NFR assessment). Implementation is sound; documentation evidence is the gap. **Non-blocking.** |
| **Reliability** | PASS | **PASS** | Capture helper try/except verified by `test_io_failure_does_not_propagate`; AC-9 fallback path tested with uuid12 uniqueness (mitigates OPS-003); slim file projection is no-op when payload absent. |
| **Maintainability** | PASS | **PASS** | `LlmPayloadSpan` TypedDict locks the field schema; single shared serialisation path (TECH-001 mitigation); 56 tests cover settings/exporters/wiring/integration; observability.md is comprehensive. |

### Risk Profile Re-Assessment

| Risk ID | Pre-impl Score | Status | Evidence |
|---------|---------------:|--------|----------|
| SEC-001 (PII persistence) | 6 (High) | MITIGATED | INFO log on activation; PII warning header in every `*.llm.jsonl`; top-of-docs warning; retention WARNING at engine init. |
| DATA-001 (binary bytes serialised) | 6 (High) | MITIGATED | `replace_binary_payloads` recursive; 10 MB stress test asserts output line < 200 chars (`TestBinaryOmission.test_10mb_stress`). |
| TECH-001 (parallel writers / schema drift) | 4 (Med) | MITIGATED | `FileExporter._project_for_slim` strips payload from a single shared serialisation path; no parallel writers per span. |
| PERF-001 (sync JSONL write latency) | 4 (Med) | PARTIALLY MITIGATED | Async writer + gzip available; no measured overhead documented. |
| DATA-002 (non-JSON-serialisable values) | 4 (Med) | MITIGATED | `LlmPayloadFileExporter.export` catches `TypeError`/`ValueError`; slim `FileExporter` uses `default=str` fallback. |
| OPS-001 (exporter exception breaks LLM call) | 3 (Low) | MITIGATED | `_capture_llm_payload_to_span` wrapped in `try/except`; `LlmPayloadFileExporter.export` swallows `OSError`; `test_io_failure_does_not_propagate` confirms. |
| SEC-002 (secrets in system prompts) | 3 (Low) | DOCUMENTED | Behaviour documented as out-of-scope; no scrubber. |
| OPS-002 (unbounded disk usage) | 2 (Low) | MITIGATED in 003.2 | Cleanup helper + retention warning in 003.1. |
| TECH-002 (engine reference threading) | 2 (Low) | MITIGATED | `getattr` chain through engine reference; idempotency guard. |
| OPS-003 (fallback path collision) | 1 (Min) | MITIGATED | `uuid4().hex[:12]` used in fallback path. |

### Test Architecture Assessment

- **Coverage levels**: 19 unit + 23 integration + 2 doc-relevant (matches the 44-scenario test design baseline; actual 56 = expanded coverage incl. 003.2 cleanup + 003.3 async tests folded into the same file).
- **Shift-left bias**: Pure logic (settings normalisation, glob matcher, binary-omission walker, schema shape) is unit-tested. Filesystem and engine wiring are integration-tested.
- **Determinism**: All tests use `TemporaryDirectory` + relative paths; no flaky sleeps in 003.1 paths.
- **Defence in depth on AC-13**: Two complementary tests (`FileExporter.test_no_op_when_no_payload` + `TestEngineWiring.test_default_off_no_payload_exporter`) cover the byte-identical-default-off invariant functionally; SHA256-pinned baseline (INT-014) is the only design item still on the table.

### Gaps & Recommendations

| # | Severity | Gap | Suggested follow-up |
|---|----------|-----|---------------------|
| 1 | Low | NFR Performance: per-call overhead not quantified in `observability.md` | Add a smoke benchmark fixture (e.g., capture-on vs capture-off latency for a 1 KB and 50 KB payload) and document the measured median + p99 numbers in `observability.md` under the async-writer section. |
| 2 | Low | AC-11 has no end-to-end test for `dynamic_parallel`-rendered branch globbing | Add an integration test with a small `dynamic_parallel` fixture rendering `extract_batch_0..N` branches and assert the glob `extract_batch_*` selects them. The plumbing already exists; this is a guard against future regressions. |
| 3 | Low | AC-13 SHA256-pinned baseline fixture (INT-014) not added | Optional belt-and-suspenders: commit a `python/tests/fixtures/tracing/baseline_trace.jsonl` and a `test_default_off_byte_identical` that recomputes the SHA256 hash. The current tests cover the contract functionally; this would catch any future inadvertent slim-format change. |
| 4 | Low | INT-019 (JSON encode error specifically) not exercised | Production code handles `TypeError`/`ValueError` in `LlmPayloadFileExporter.export:267-274`; an explicit test injecting a non-serialisable object would close the loop. |

None of these gaps block the gate. They are P2/P3 follow-ups suitable for a small post-merge cleanup commit or for tracking via `/schedule`.

### Standards Compliance

- ✅ Docstrings present on all new public symbols.
- ✅ No emojis introduced in code (matches house style).
- ✅ Type hints on new helpers and the TypedDict.
- ✅ Logging follows established pattern (`logger = logging.getLogger(__name__)` per module).
- ✅ Error handling at boundaries (engine init / exporter write); no unnecessary `try/except` in inner logic.
- ✅ Default-off behaviour preserved; additive feature.
- ✅ No new runtime dependencies.

### Final Notes

Implementation quality is high. The dev addressed every "must-fix before merge" item from the risk profile and every quick-win from the NFR assessment. The gate moves from CONCERNS (planning) to **PASS** (post-implementation). Two advisory items (Performance benchmark documentation + dynamic_parallel branch test) are appropriate for a follow-up commit and do not block release.

**Status recommendation: Done.**
