# NFR Assessment: TEA-OBS-003.1

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.1 — Core LLM payload capture + separate exporter + flag
Mode: YOLO (non-interactive; default core four NFRs)

## Scope Assessed

- Security
- Performance
- Reliability
- Maintainability

(Usability, Compatibility, Portability, Functional Suitability not selected.)

## Summary

| NFR | Status | One-liner |
|---|---|---|
| Security | **CONCERNS** | Default-off + opt-in + binary omission rule, but no secret/PII scrubber and no retention in-scope; persistence to disk is the hazard. |
| Performance | **CONCERNS** | Synchronous JSONL write per `llm.call` with no stated latency target; payload size can be multi-KB to multi-hundred-KB per call. Target unknown. |
| Reliability | **PASS** | Story explicitly requires try/except isolation around span injection (Task 3) and default-off byte-identical compatibility (AC-13). Test design covers exporter exception isolation. |
| Maintainability | **PASS** | 6 test scenarios required (AC-16), single-shared-serialization-path mitigation (TECH-001), explicit docs requirement (AC-17/18), structured field schema. |

**Quality Score: 80/100** (100 − 2 × CONCERNS × 10)

**Recommended gate: CONCERNS** — converges to PASS once Security mitigations (INFO log + strong PII warning header) and Performance evidence (latency overhead measured + documented) land. Reliability and Maintainability are already gated by required tests/docs in the story.

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'Default-off + AC-5 binary omission + AC-17/18 PII docs limit blast radius, but no scrubber for secrets in system prompts (SEC-002) and no retention/rotation in this story (deferred to TEA-OBS-003.2). Persistence to disk is the residual hazard. Recommend INFO log on activation + warning header line in *.llm.jsonl.'
  performance:
    status: CONCERNS
    notes: 'Target unknown — story has no latency target for capture-on vs capture-off. Synchronous JSONL writes hold the LLM result path. Recommend a smoke benchmark (non-gating) and a documented "expected per-call overhead" note.'
  reliability:
    status: PASS
    notes: 'Task 3 mandates try/except wrapping; AC-13 enforces default-off byte-identical behavior; AC-9 fallback path on missing trace_file. Risk OPS-001 mitigated by required exporter-exception-isolation test.'
  maintainability:
    status: PASS
    notes: 'AC-16 specifies 6 test scenarios; AC-17/18 mandate observability docs + PII forward-link to TEA-OBS-003.2; TECH-001 mitigation calls for shared serialization path with strip-projection (no parallel writers); structured field schema in AC-4 reduces drift surface.'
```

## Detailed Findings

### Security — CONCERNS

**Evidence reviewed**
- AC-5: bytes-typed message entries replaced with `{"type": "binary_omitted", "size_bytes": N}` placeholder before serialization.
- AC-13: default-off behavior is byte-identical to current trace output (no new files, no field changes).
- AC-17 & AC-18: docs section + PII warning + forward-link to TEA-OBS-003.2 (retention).
- Risk profile (`TEA-OBS-003.1-risk-20260501.md`): SEC-001 (PII leakage) High, SEC-002 (secrets in system prompts) Low.

**Why CONCERNS, not PASS**
- Capture writes full request messages and full response content to disk in plaintext JSONL. Once enabled, the data exists on the filesystem indefinitely (no retention or rotation in this story).
- No scrubber for API keys, bearer tokens, or other secrets that may sit in system prompts (SEC-002 — explicitly out of scope).
- Documentation warnings depend on operator discipline. "Don't enable in production with PII" is a process control, not a technical control.
- Default-off + opt-in + binary omission are good controls but reduce probability, not impact. Impact remains High because LGPD/GDPR exposure is real if `*.llm.jsonl` is checked in or shipped to a log aggregator.

**Why not FAIL**
- Default-off prevents the failure mode by default; users have to take an explicit action to expose data.
- Binary-omission rule (AC-5) prevents the worst inadvertent leak (multi-MB PDFs with embedded customer data).
- A clear forward-link to TEA-OBS-003.2 is required (AC-18) so the retention follow-up is visible.

**Missing considerations / recommendations**
1. **INFO-level log on activation** naming the output path — surfaces in user-visible run output that data is being persisted. (Recommended in risk profile; promote to ACs.)
2. **Header record in `*.llm.jsonl`** — first line a JSONL-incompatible warning comment (e.g., `# WARNING: contains LLM request/response payloads. May contain PII.`) so any downstream tool either trips loudly or sees the warning. Currently AC-8 says "one JSON object per line" — this needs a deliberate carve-out if adopted.
3. **Permissions on the output file** — consider `chmod 0600` on Unix to limit casual disclosure. Not required by ACs today.
4. **Capture activation must not propagate into git diffs** — recommend documenting the suggested `.gitignore` pattern (`*.llm.jsonl`) in the docs section.

### Performance — CONCERNS

**Evidence reviewed**
- AC-7: separate file `<trace>.llm.jsonl` writer.
- Implementation Task 4: extends `FileExporter` or adds `LLMPayloadFileExporter` — synchronous append-and-flush per span (consistent with current `FileExporter` at `tracing.py:117-121`).
- Risk profile PERF-001 (Medium): synchronous JSONL write of multi-KB payloads adds latency per `llm.call`.

**Why CONCERNS, not PASS**
- No latency target is stated in any AC. "Target unknown" → CONCERNS per task rules.
- Payload size per call can range from ~1 KB (short chat turns) to several hundred KB (long-context prompt with retrieved chunks). At the high end, JSON-encoding + fsync per call is non-trivial.
- The exporter holds a thread lock (`tracing.py:113, 119`) — under `dynamic_parallel`, parallel `llm.call`s serialize on the file write. Worth documenting.

**Why not FAIL**
- Capture is opt-in. The default-off path is byte-identical.
- The work is "format dict + json.dumps + append" — not algorithmically expensive. Real latency budget for a single `llm.call` is dominated by model latency (hundreds of ms to tens of seconds), so even a 5–20ms exporter overhead is noise relative to the call itself.

**Missing considerations / recommendations**
1. **Smoke benchmark**: capture-on vs capture-off, single thread, 100 calls, log p50/p95/p99 deltas. Non-gating, but recorded as evidence.
2. **Document the "shared write lock under parallel" footnote** so users running `dynamic_parallel` extraction at width >8 understand serialization cost.
3. **Buffered writes / batched flush** are not in this story. Listed here as a future opt-in for OBS-003 series, not blocking this story.

### Reliability — PASS

**Evidence reviewed**
- Task 3 explicitly: "Wrap in try/except — never let trace failures break the LLM call".
- AC-13 default-off byte-identical compatibility.
- AC-9 fallback `tea-llm-payloads-<run_id>.jsonl` when `trace_file` is unset.
- AC-14 no mutual exclusion with `auto_trace`, `trace_exporter`, `trace_file`, Opik exporter.
- Risk profile OPS-001: exporter exception propagating into `llm.call` — Low, with mandated isolation test.

**Why PASS**
- Error isolation is a first-class implementation requirement, not a docstring suggestion.
- Default-off path is the existing tested behavior; AC-13 makes any byte drift a test failure.
- Fallback path for missing `trace_file` (AC-9) with INFO log is a reliability win.

**Missing considerations / recommendations**
1. The exporter-exception isolation test should mock both `IOError` (disk full) and `JSONEncodeError` (non-serializable nested object) to cover both failure shapes.
2. AC-9 should specify behavior when the fallback path itself fails (e.g., CWD not writable). Suggest: log warning, drop the payload, continue execution. Not currently in ACs.
3. Concurrent run safety: the fallback path uses `<run_id>`, which should guarantee uniqueness — verify `run_id` is uuid-shaped (risk profile flagged OPS-003 Minimal).

### Maintainability — PASS

**Evidence reviewed**
- AC-4: structured field schema (`messages_input`, `response_content`, `tokens_input`, `tokens_output`, `model`, `stop_reason`, `cost_usd`).
- AC-16: 6 explicit test scenarios.
- AC-17: dedicated `docs/python/observability.md` section.
- AC-18: PII warning + forward-link to TEA-OBS-003.2.
- Risk profile TECH-001 (Medium): mitigated by single shared serialization path with strip-projection rather than parallel writers.

**Why PASS**
- Field schema is explicit, named, and matches LiteLLM's existing return shape (already produced at `llm_actions.py:1132-1154`) — low translation surface.
- Test design exists (test-design assessment file present), covering all 18 ACs.
- Docs requirement is enforced by AC, not aspiration.
- Code locations are pinpointed in story context (line numbers in `tracing.py`, `llm_actions.py`, `yaml_engine.py`) — implementer doesn't have to discover the integration points.

**Missing considerations / recommendations**
1. **Field schema documented in code, not just docs** — recommend a `LLMPayloadSpan` TypedDict (or dataclass) in `tracing.py` so the contract is machine-checkable. AC-4 currently says "implementer's call" on whether fields go on `metadata` vs a `payload` sub-dict — pin this before implementation to prevent drift.
2. **Shared serialization path** (TECH-001 mitigation) should be explicit in Task 4: "single span dict → strip payload fields for slim file → keep payload fields for `.llm.jsonl`". Currently Task 4 reads as if it could be implemented as parallel writers.
3. **Slim/payload schema parity test** (Priority-2 in risk profile) should land alongside the two-file separation test (AC-16) — same test fixture, two assertions.

## Critical Issues

1. **Security: PII persistence to disk** (SEC-001 from risk profile)
   - Risk: Real workflows enable the flag for "one quick debug" and forget; CNPJ/financial/PHI lands on disk indefinitely.
   - Fix: Add INFO log on activation (path-naming) + a JSONL-incompatible warning header line in the output file. Promote both from risk-profile recommendation to AC.

2. **Performance: latency target unknown** (PERF-001 from risk profile)
   - Risk: Operators enable capture in `dynamic_parallel` with width 16; serialization on the exporter lock adds non-trivial wall-clock.
   - Fix: Smoke benchmark (capture-on vs capture-off, single + parallel) included in test plan, with results documented in observability.md as "expected per-call overhead".

## Quick Wins

- Add INFO log on capture activation (`"LLM payload capture ENABLED → <path>"`) — ~30 minutes.
- Add a `LLMPayloadSpan` TypedDict to pin the field schema — ~30 minutes.
- Add JSONL-incompatible warning header as the first line of `*.llm.jsonl` — ~30 minutes (requires AC update).
- Run smoke benchmark, capture p50/p95 deltas in `docs/python/observability.md` — ~1–2 hours.
- Document `.gitignore` recommendation for `*.llm.jsonl` — ~10 minutes.

## Test Recommendations (priority-ordered)

| # | Priority | Test | NFR Coverage |
|---|---|---|---|
| 1 | P1 | Default-off byte-identical diff of `<trace>.jsonl` | Security (AC-13), Reliability |
| 2 | P1 | Recursive binary-bytes walk (top-level, OpenAI vision, Anthropic image, 10 MB stress) | Security (AC-5), Reliability (no encode crash) |
| 3 | P1 | Exporter-exception isolation (mock `IOError`, mock `JSONEncodeError`) | Reliability |
| 4 | P1 | INFO log emission on capture activation | Security (operator visibility) |
| 5 | P2 | Slim vs payload schema parity (single fixture, two assertions) | Maintainability |
| 6 | P2 | Latency smoke benchmark (capture on vs off, parallel and serial) | Performance |
| 7 | P2 | Non-JSON value tolerance (datetime, Pydantic, Decimal) | Reliability |
| 8 | P3 | Fallback path when `trace_file` unset (AC-9) | Reliability |
| 9 | P3 | CLI override semantics (CLI > YAML; pattern merge) | Maintainability |
| 10 | P3 | Two-file separation under parallel writers (lock contention does not corrupt JSONL) | Performance, Reliability |

## Acceptance Criteria for Gate Pass

To move from **CONCERNS → PASS** at gate time:

1. **Security:** INFO log on activation implemented + tested; observability.md PII warning present and at top of section; forward-link to TEA-OBS-003.2 visible.
2. **Performance:** Smoke benchmark run; per-call overhead documented as a number (e.g., "~5 ms median, ~15 ms p99 for typical 4 KB payloads") in observability.md.
3. **Reliability:** Already on track — passing tests on Task 3's try/except + AC-13 default-off + AC-9 fallback satisfy the gate.
4. **Maintainability:** Already on track — AC-16 test set + AC-17/18 docs satisfy the gate. Promote `LLMPayloadSpan` TypedDict (or equivalent contract pin) for extra confidence.

## Notes

- Risk profile and test-design assessments already exist in this directory and are referenced from the story. This NFR assessment complements them — does not duplicate.
- Story is in **Draft** status; no implementation yet. NFR conclusions are necessarily forward-looking, gated on implementation matching ACs.

---

NFR assessment: docs/qa/assessments/TEA-OBS-003.1-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-OBS-003.1-llm-payload-capture-core.yml under nfr_validation
