# Risk Profile: Story TEA-OBS-003.1

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: TEA-OBS-003.1 — Core LLM payload capture + separate exporter + flag

## Executive Summary

- Total Risks Identified: 10
- Critical Risks: 0
- High Risks: 2
- Medium Risks: 3
- Low/Minimal Risks: 5
- Overall Risk Score: **55/100** (moderate; gate recommendation = CONCERNS pending mitigation tests)

The feature is **additive and default-off**, which sharply caps blast radius. The two High risks (PII leakage, binary-payload serialization) are both explicitly called out in the story's Risk and Compatibility section and have first-class ACs (AC-5, AC-18). Gate posture is contingent on those mitigations being verified by tests, not just promised in docs.

## Critical Risks Requiring Immediate Attention

None at score 9.

## High Risks (Score 6)

### 1. SEC-001: PII / sensitive data leakage to disk when capture enabled

**Score: 6 (High)** — Probability: Medium (2) × Impact: High (3)

- **Probability rationale:** Default-off + explicit opt-in + doc warning (AC-18) drops baseline probability. But once a user flips the flag for "one quick debug" of a real workflow, payloads carrying CNPJ, customer financial data, or PHI will hit disk with no retention. Real-world experience says debug flags get left on.
- **Impact rationale:** Regulatory exposure (LGPD/GDPR), audit-log pollution, potential disclosure if `*.llm.jsonl` is later checked in or shipped to a log aggregator.
- **Mitigation:**
  - Documentation must use STRONG warning language at the top of the section (not buried), per AC-17/18.
  - Forward-link to TEA-OBS-003.2 must be explicit so users know retention is a follow-up, not in this story.
  - Recommend (advisory): emit an INFO-level log line when payload capture activates ("LLM payload capture ENABLED — output: <path>") so it's visible in run output that data is being persisted.
  - Recommend (advisory): include a `# WARNING: contains LLM request/response payloads. May contain PII.` header line as the **first** record in `*.llm.jsonl`. JSONL readers will trip on it; that's the point — forces the user to acknowledge.
- **Testing focus:** Verify INFO log emission on activation. Verify default-off path emits no `.llm.jsonl` file at all (AC-13 byte-identical test).
- **Residual risk:** Medium — operational discipline is required; this story cannot fully solve it (retention is OBS-003.2).
- **Owner:** dev + docs

### 2. DATA-001: Binary message bytes serialized into JSONL

**Score: 6 (High)** — Probability: Medium (2) × Impact: High (3)

- **Probability rationale:** The story explicitly cites a 1.5 MB PDF case. Any workflow doing OCR/document extraction passes bytes through messages. Medium probability because not all users hit this path, but those who do will hit it on the first run.
- **Impact rationale:** (a) JSON encoding failure → exporter raises → if try/except is missing or wrong, breaks the LLM call; (b) if encoding succeeds via base64 fallback, file balloons by ~33% per binary; (c) file write IO + memory pressure during large run.
- **Mitigation (per AC-5):**
  - Replace bytes entries with `{"type": "binary_omitted", "size_bytes": N}` BEFORE serialization.
  - Walk nested structures — bytes can hide inside `content: [{type: image, source: {data: b'...'}}]` (Anthropic format), `messages[i].images[j]` (OpenAI vision), and Pydantic-style attachments.
- **Testing focus (priority 1):**
  - Construct a message with top-level bytes → assert placeholder.
  - Construct OpenAI-vision-style nested base64 → assert placeholder applied to nested bytes.
  - Construct Anthropic image_url-style nested bytes → assert placeholder.
  - Stress: 10 MB synthetic bytes value → assert single-digit-KB output file (proves replacement happened, not just decoration).
- **Residual risk:** Low after mitigation, IF the recursive walk covers nested cases. Story's task list says "Replace bytes-typed message entries" (singular, top-level reading) — recommend the impl explicitly does a recursive walk.
- **Owner:** dev

## Medium Risks (Score 4)

### 3. TECH-001: Exporter coordination — slim file vs payload file divergence

**Score: 4 (Medium)** — Probability: Medium (2) × Impact: Medium (2)

- **Description:** Two files must stay schema-aligned. If a future change adds a span field, both `<trace>.jsonl` and `<trace>.llm.jsonl` need it, but only one path will be touched if the implementer subclasses `FileExporter` instead of using a filter pipeline.
- **Mitigation:** Prefer a single shared serialization path with a "strip payload fields" projection for the slim file, rather than two parallel writers (story task 4 already hints at this — "or extend `FileExporter` with a filter callable").
- **Testing focus:** Test asserting that for any span emitted with capture on, fields in `slim ∪ {payload fields} == fields in payload-file span`.
- **Owner:** dev

### 4. PERF-001: Per-call latency from synchronous JSONL write of full payload

**Score: 4 (Medium)** — Probability: Medium (2) × Impact: Medium (2)

- **Description:** `llm.call` already incurs network latency; adding sync serialization + disk write of a multi-KB-to-MB payload before returning to the workflow will add tens to hundreds of ms per call. In a `dynamic_parallel` extract workflow with 50 calls, that's measurable.
- **Mitigation:**
  - Confirm exporter writes are appended (not full-file rewrites) — `FileExporter` should already do this, verify.
  - Recommend: when capture is on, document the expected latency overhead in the observability page (AC-17 hook).
  - Out-of-scope but worth noting: a future async/buffered exporter is a natural follow-up.
- **Testing focus:** Benchmark test (or at minimum, smoke test) — capture-on vs capture-off elapsed time for 20-call workflow. Don't gate on numbers, but log them.
- **Owner:** dev

### 5. DATA-002: Non-JSON-serializable values in messages (Pydantic, datetime, custom objects)

**Score: 4 (Medium)** — Probability: Medium (2) × Impact: Medium (2)

- **Description:** Messages may contain Pydantic models (tool-call args), datetime objects (timestamps in system prompts), Decimal (financial amounts), enums, or custom dataclasses. Naïve `json.dumps(messages)` raises `TypeError` mid-write.
- **Mitigation:**
  - Use a json encoder with `default=str` fallback OR a recursive normalizer.
  - Wrap in try/except (AC's "never let trace failures break the LLM call" — this is the trigger).
  - Log a single-line warning when normalization is needed; don't spam.
- **Testing focus:**
  - Test with `messages` containing `datetime.now()` in a system prompt → verify capture succeeds with stringified datetime.
  - Test with Pydantic model in `content` → verify capture succeeds (model_dump path).
  - Test with intentionally unserializable object (a thread lock) → verify exception is swallowed, span still written WITHOUT payload, LLM call returns normal result.
- **Owner:** dev

## Low Risks (Score 2-3)

### 6. OPS-001: Trace exporter exception propagating to LLM action

**Score: 3 (Low)** — Probability: Low (1) × Impact: High (3)

- **Description:** Story Risk Section calls this out as Secondary Risk. If any payload serialization or file-write step raises, and try/except is missing or scoped wrong, the entire `llm.call` returns None or raises, breaking the workflow.
- **Mitigation:** AC-implied try/except wrap around span metadata injection AND around exporter file write.
- **Testing focus:**
  - Inject a mock exporter whose `write_span` raises `IOError("disk full")` → assert `llm.call` still returns the LLM response unchanged.
  - Inject unserializable payload (see DATA-002 #5) → assert workflow continues.
- **Residual risk:** Low after explicit try/except + tests; without those tests, this risk is High because the failure mode is silent until prod.
- **Owner:** dev

### 7. SEC-002: API keys / system-prompt secrets in captured messages

**Score: 3 (Low)** — Probability: Low (1) × Impact: High (3)

- **Description:** Authors sometimes (incorrectly) put API keys, internal URLs, or auth tokens directly into system prompts. Capture would persist them.
- **Mitigation:** Out-of-scope for this story (no redaction logic planned). Documentation should note "captured payloads include the FULL system prompt and message content — review for secrets before sharing payload files."
- **Testing focus:** None at code level. Doc review.
- **Owner:** docs
- **Accept rationale:** Adding a secret-scrubber is OBS-003.2+ scope; calling it out keeps users informed without scope creep.

### 8. OPS-002: Unbounded disk usage from `*.llm.jsonl`

**Score: 2 (Low)** — Probability: Low (1) × Impact: Medium (2)

- **Description:** Long-running workflows with capture on can produce GB-scale payload files. No rotation, no size cap in this story.
- **Mitigation:** Deferred to TEA-OBS-003.2 (retention/cleanup). Document the "no rotation" caveat.
- **Testing focus:** None. Document.
- **Owner:** docs

### 9. TECH-002: Engine reference threading from `llm.call` to span metadata

**Score: 2 (Low)** — Probability: Low (1) × Impact: Medium (2)

- **Description:** `llm.call` action needs access to (a) capture-active flag, (b) current node name (for glob match), (c) trace context. Existing actions plumbing may not expose all three cleanly. Risk of fragile patches like global state or kwargs-overloading.
- **Mitigation:** Use the existing engine `_trace_context` reference (already mentioned in task list). Confirm node name is available on the active span when entering `llm.call` — if not, that's a small upstream fix.
- **Testing focus:** Verify glob-filter test (AC-10) passes against a YAML where the parent node name has special characters and against a `dynamic_parallel`-rendered branch (AC-11).
- **Owner:** dev

### 10. OPS-003: Path collision when `trace_file` unset (multiple parallel runs)

**Score: 1 (Minimal)** — Probability: Low (1) × Impact: Low (1)

- **Description:** AC-9 fallback path is `tea-llm-payloads-<run_id>.jsonl`. If `run_id` collides (two parallel `tea run` invocations on same machine within the same second using time-based id), files clobber.
- **Mitigation:** Trust that `run_id` is uuid-shaped (verify in impl). If it's a timestamp, add randomness suffix.
- **Testing focus:** Cursory — verify run_id uniqueness in a quick parallel-spawn test.
- **Owner:** dev

## Risk Distribution

### By Category

| Category | Risks | Critical | High | Medium | Low |
|----------|-------|----------|------|--------|-----|
| SEC      | 2     | 0        | 1    | 0      | 1   |
| DATA     | 2     | 0        | 1    | 1      | 0   |
| TECH     | 2     | 0        | 0    | 1      | 1   |
| PERF     | 1     | 0        | 0    | 1      | 0   |
| OPS      | 3     | 0        | 0    | 0      | 3   |
| BUS      | 0     | 0        | 0    | 0      | 0   |

### By Component

- `actions/llm_actions.py` (payload extraction, bytes-walk): 4 risks (DATA-001, DATA-002, PERF-001, OPS-001)
- `tracing.py` (exporter split): 2 risks (TECH-001, OPS-001)
- `yaml_engine.py` + `cli.py` (settings/flag plumbing): 1 risk (TECH-002)
- Documentation: 3 risks (SEC-001, SEC-002, OPS-002)

## Detailed Risk Register

| Risk ID  | Category | Title                                              | Probability | Impact | Score | Priority |
|----------|----------|----------------------------------------------------|-------------|--------|-------|----------|
| SEC-001  | SEC      | PII leakage when capture enabled                   | M (2)       | H (3)  | 6     | High     |
| DATA-001 | DATA     | Binary bytes serialized into JSONL                 | M (2)       | H (3)  | 6     | High     |
| TECH-001 | TECH     | Slim/payload file schema divergence                | M (2)       | M (2)  | 4     | Medium   |
| PERF-001 | PERF     | Sync JSONL write latency per `llm.call`            | M (2)       | M (2)  | 4     | Medium   |
| DATA-002 | DATA     | Non-JSON-serializable values in messages           | M (2)       | M (2)  | 4     | Medium   |
| OPS-001  | OPS      | Exporter exception propagating to LLM call         | L (1)       | H (3)  | 3     | Low      |
| SEC-002  | SEC      | API keys/secrets in captured messages              | L (1)       | H (3)  | 3     | Low      |
| OPS-002  | OPS      | Unbounded `.llm.jsonl` disk usage                  | L (1)       | M (2)  | 2     | Low      |
| TECH-002 | TECH     | Engine/node-name reference threading               | L (1)       | M (2)  | 2     | Low      |
| OPS-003  | OPS      | Path collision on `run_id`-only fallback           | L (1)       | L (1)  | 1     | Minimal  |

## Risk-Based Testing Strategy

### Priority 1: High-Risk Tests (must exist)

1. **Binary omission — multi-shape (DATA-001):**
   - Top-level bytes in message content
   - OpenAI-vision-style nested base64 image
   - Anthropic-style nested image bytes
   - 10 MB synthetic bytes → assert output file <100 KB
2. **Default-off byte-identical (SEC-001 + AC-13):**
   - Run identical workflow with and without flag
   - Diff `<trace>.jsonl` → must be byte-identical
   - Assert no `.llm.jsonl` file exists when flag off
3. **Activation log emission (SEC-001 advisory):**
   - When flag flips on, INFO log line names the output file path
4. **Glob filter — node-name not action-name (DATA-001 + AC-10/11):**
   - Workflow with two `llm.call` nodes; glob matches one
   - Assert exactly one set of payloads in `.llm.jsonl`
   - Same for `dynamic_parallel`-rendered branch name (AC-11)

### Priority 2: Medium-Risk Tests

5. **Slim/payload schema parity (TECH-001):**
   - For same span, assert `slim_fields ∪ payload_fields == full_field_set`
6. **Latency smoke (PERF-001):**
   - 20-call workflow, capture on vs off, log elapsed; non-gating but recorded
7. **Non-JSON value tolerance (DATA-002):**
   - `datetime` in message → captured as string
   - Pydantic model → captured via model_dump
   - Unserializable object (thread lock) → exporter swallows error, LLM call still returns

### Priority 3: Low-Risk Tests

8. **Exporter exception isolation (OPS-001):**
   - Mock exporter raising `IOError` → `llm.call` returns normally
9. **Fallback path (AC-9):**
   - `trace_file` unset + capture on → `tea-llm-payloads-<run_id>.jsonl` in CWD; INFO log
10. **CLI override (AC-2/3 + Task 5):**
    - YAML says false, CLI `--trace-llm-payloads` → capture active
    - YAML list, CLI `--trace-llm-payloads-for X` → patterns merged

### Coverage Gaps to Avoid

- Do NOT skip the recursive bytes walk test (DATA-001 #2-3). Top-level-only is the most common implementation mistake here.
- Do NOT rely on `json.dumps` raising as the test for binary omission — the placeholder must be present in the output, not just "no exception."

## Risk Acceptance Criteria

### Must Fix Before Merge

- All High risks have a passing test (SEC-001 #2 default-off diff, DATA-001 binary tests).
- OPS-001 try/except is verified by test, not just present in code.

### Can Merge with Mitigation Documented

- SEC-002 (secrets) — documentation only, no scrubber in this story.
- OPS-002 (disk usage) — documentation noting follow-up in OBS-003.2.
- PERF-001 — latency note in observability docs; non-gating numbers.

### Accepted Risks

- OPS-003 path collision — accept if `run_id` is uuid-shaped; otherwise add suffix.
- SEC-002 — explicitly out of scope; documented warning suffices.

## Monitoring Requirements

Post-deployment (whenever a real run uses this):

- **SEC-001:** Audit which environments enable `auto_trace_llm_payloads`. Tag those runs.
- **PERF-001:** If users report slow runs after enabling, correlate with capture flag.
- **DATA-001:** Watch for `*.llm.jsonl` file sizes >100 MB — likely a binary leak.
- **OPS-001:** Grep logs for "trace exporter failed" warnings; rising rate = silent breakage.

## Risk Review Triggers

Re-run risk profile when:

- TEA-OBS-003.2 (retention) lands — re-evaluate SEC-001 residual risk.
- A new LLM message format ships (e.g., new multimodal type) — re-evaluate DATA-001 walk coverage.
- Async/buffered exporter is added — PERF-001 changes.
- Any change to `TraceContext.start_span/log_event/end_span` signatures.

## Gate Mapping

Per the deterministic gate algorithm:

- No risk score ≥ 9 → not FAIL
- 2 risks at score 6 → **CONCERNS**
- Both High risks have explicit ACs and mitigation paths in the story.

**Recommended gate:** **CONCERNS** until tests for SEC-001 (default-off byte-identical) and DATA-001 (recursive binary omission) are visible in the test suite. Once those land, gate moves to PASS.

## Risk Summary Block (paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 2
    medium: 3
    low: 5
  highest:
    id: SEC-001
    score: 6
    title: 'PII leakage when payload capture enabled'
  recommendations:
    must_fix:
      - 'Verify recursive binary-bytes walk via tests covering top-level, OpenAI-vision-nested, and Anthropic-image-nested shapes (DATA-001)'
      - 'Add default-off byte-identical diff test against existing trace file (SEC-001 / AC-13)'
      - 'Add exporter-exception-isolation test asserting llm.call still returns the LLM response (OPS-001)'
    monitor:
      - 'INFO-level log line on capture activation naming the output file path (SEC-001)'
      - 'Document PII warning prominently at top of observability section, with explicit forward-link to TEA-OBS-003.2 retention (SEC-001 / AC-18)'
      - 'Document expected per-call latency overhead when capture is on (PERF-001)'
      - 'Ensure shared serialization path keeps slim and payload files schema-aligned (TECH-001)'
```

## Story Hook Line

Risk profile: docs/qa/assessments/TEA-OBS-003.1-risk-20260501.md
