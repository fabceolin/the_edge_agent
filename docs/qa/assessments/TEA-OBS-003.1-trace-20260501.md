# Requirements Traceability Matrix

## Story: TEA-OBS-003.1 — Core LLM payload capture + separate exporter + flag

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Story status at trace:** Draft (no implementation, no production tests merged yet)
**Planning reference:** [TEA-OBS-003.1-test-design-20260501.md](TEA-OBS-003.1-test-design-20260501.md)

---

## Coverage Summary

| Metric | Count | % |
|---|---|---|
| Total Requirements (ACs) | 18 | 100% |
| Fully Covered (planned) | 17 | 94% |
| Partially Covered (planned) | 1 | 6% |
| Not Covered | 0 | 0% |

**Important caveat:** Story is **Draft** — no production test files exist yet under `python/tests/`. The "Coverage" column below reflects coverage by **planned scenarios from the test design** (UNIT-001..019, INT-001..023, DOC-001..002), not by tests merged in `main`. Trace must be re-run after implementation lands so that planned scenarios can be confirmed against actual test files (e.g., `python/tests/test_llm_payload_capture.py` once it exists).

---

## Coverage Levels Used

- **`full (planned)`** — At least one planned scenario covers the AC; Given-When-Then is unambiguous; either unit + integration or a single comprehensive test layer is justified.
- **`partial (planned)`** — Planned scenarios cover the AC but leave a documented sub-aspect unverified.
- **`none`** — No planned or existing test addresses the AC.

When the story moves out of Draft, replace `(planned)` with `(implemented)` after verifying the file/function names against the merged test suite.

---

## Requirement Mappings

### Part A — Settings & flag

#### AC-1: YAML setting `auto_trace_llm_payloads` accepts `false` / `true` / list of glob patterns
**Coverage: FULL (planned) — unit**

- **Unit:** `TEA-OBS-003.1-UNIT-001` (P0)
  - **Given:** YAML with `auto_trace_llm_payloads: false` (or unset)
  - **When:** `_configure_from_settings` parses the settings dict
  - **Then:** Engine attribute `_llm_payload_capture` is `False`; no exporter wired
- **Unit:** `TEA-OBS-003.1-UNIT-002` (P0)
  - **Given:** YAML with `auto_trace_llm_payloads: true`
  - **When:** Settings parsed
  - **Then:** Attribute is the boolean `True`; capture is engine-wide
- **Unit:** `TEA-OBS-003.1-UNIT-003` (P0)
  - **Given:** YAML with `auto_trace_llm_payloads: [extract_batch_*, correct]`
  - **When:** Settings parsed
  - **Then:** Attribute is the list `['extract_batch_*', 'correct']`, preserving order
- **Unit:** `TEA-OBS-003.1-UNIT-004` (P1)
  - **Given:** YAML with malformed value (e.g., integer `42`, dict)
  - **When:** Settings parsed
  - **Then:** Configuration error raised with field-name pointer
- **Unit:** `TEA-OBS-003.1-UNIT-005` (P2)
  - **Given:** YAML with empty list `auto_trace_llm_payloads: []`
  - **When:** Settings parsed
  - **Then:** Treated as "capture nothing" — no spans get payloads (degenerate but legal)

#### AC-2: CLI flag `tea run --trace-llm-payloads` enables capture and overrides YAML-false
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-001` (P0)
  - **Given:** YAML with `auto_trace_llm_payloads: false`, CLI invoked with `--trace-llm-payloads`
  - **When:** `tea run` resolves capture setting
  - **Then:** Effective setting becomes `True`; `.llm.jsonl` produced with payloads
- **Integration:** `TEA-OBS-003.1-INT-002` (P1)
  - **Given:** No YAML setting; CLI invoked with `--trace-llm-payloads`
  - **When:** `tea run` executes a workflow with `llm.call`
  - **Then:** All `llm.call` invocations captured; YAML default did not block

#### AC-3: CLI flag `--trace-llm-payloads-for <pattern>` (repeatable) merges glob patterns
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-003` (P1)
  - **Given:** CLI `--trace-llm-payloads-for extract_batch_* --trace-llm-payloads-for correct`
  - **When:** `tea run` resolves capture
  - **Then:** Effective setting equals `['extract_batch_*', 'correct']`; both patterns active
- **Integration:** `TEA-OBS-003.1-INT-004` (P1)
  - **Given:** YAML setting `[review_*]` plus CLI `--trace-llm-payloads-for extract_batch_*`
  - **When:** Capture resolved
  - **Then:** Effective list is the union/merge `[review_*, extract_batch_*]` (CLI > YAML for boolean override; merge for list patterns — pin documented behavior)

### Part B — Captured fields

#### AC-4: Required fields added to span (`messages_input`, `response_content`, `tokens_input`, `tokens_output`, `model`, `stop_reason`, `cost_usd`)
**Coverage: FULL (planned) — unit + integration**

- **Unit:** `TEA-OBS-003.1-UNIT-006` (P0)
  - **Given:** `LLMPayloadSpan` TypedDict / contract pin in `tracing.py`
  - **When:** Schema introspected
  - **Then:** All 7 fields declared with documented types (str / int / float / dict / list)
- **Unit:** `TEA-OBS-003.1-UNIT-007` (P1)
  - **Given:** Stubbed LiteLLM response with full `usage`, `model`, `finish_reason`
  - **When:** Capture path projects fields onto span
  - **Then:** All 7 fields present with values matching the stubbed response
- **Unit:** `TEA-OBS-003.1-UNIT-008` (P1)
  - **Given:** Stubbed response missing `finish_reason`
  - **When:** Capture path projects fields
  - **Then:** `stop_reason` is `None` (or omitted, per documented schema); no exception
- **Unit:** `TEA-OBS-003.1-UNIT-009` (P1)
  - **Given:** `litellm.completion_cost` raises (model unsupported)
  - **When:** Capture path runs
  - **Then:** `cost_usd` is `None`; capture continues; warning logged
- **Integration:** `TEA-OBS-003.1-INT-005` (P0)
  - **Given:** End-to-end YAML workflow with one `llm.call`, capture-on
  - **When:** Workflow runs and exporter flushes
  - **Then:** Resulting JSONL contains a span with all 7 fields populated and the values match the stubbed LLM call

#### AC-5: Binary message bytes replaced with `{"type": "binary_omitted", "size_bytes": N}`
**Coverage: FULL (planned) — unit (mandatory P0 stack)**

- **Unit:** `TEA-OBS-003.1-UNIT-010` (P0)
  - **Given:** Messages with **top-level bytes** entry (`{"role": "user", "content": b"..."}`)
  - **When:** Binary-omission walker runs
  - **Then:** Bytes replaced with placeholder; `size_bytes` matches `len(bytes)`; serialized line < 1 KB
- **Unit:** `TEA-OBS-003.1-UNIT-011` (P0)
  - **Given:** OpenAI-vision-style message with nested `image_url.url = "data:image/png;base64,..."`
  - **When:** Walker runs
  - **Then:** Base64 payload replaced with placeholder; outer `image_url` structure preserved (so downstream parsers still see the type)
- **Unit:** `TEA-OBS-003.1-UNIT-012` (P0)
  - **Given:** Anthropic image content block (`{"type": "image", "source": {"type": "base64", "data": <bytes>}}`)
  - **When:** Walker runs
  - **Then:** Inner `data` bytes replaced; `type: image` and `source.type: base64` preserved
- **Unit:** `TEA-OBS-003.1-UNIT-013` (P0)
  - **Given:** 10 MB synthetic byte buffer in any message slot
  - **When:** Walker runs and span serialized
  - **Then:** Output JSONL line < 1 KB; placeholder reports `size_bytes: 10485760`
- **Unit:** `TEA-OBS-003.1-UNIT-014` (P1)
  - **Given:** Deeply nested bytes (≥ 4 levels into a list of dicts)
  - **When:** Walker runs
  - **Then:** Bytes replaced even at depth; surrounding structure intact

#### AC-6: When capture inactive, payload fields absent from spans
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-006` (P0)
  - **Given:** `auto_trace_llm_payloads` unset, workflow with `llm.call` runs
  - **When:** Span dict inspected after `end_span`
  - **Then:** None of the 7 payload fields present; span field set identical to today's baseline

### Part C — Separate exporter file

#### AC-7: Capture-on produces `<basename>.llm.jsonl` in addition to slim `<trace_file>`
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-007` (P0)
  - **Given:** `--trace-file run-001.jsonl` and capture on
  - **When:** Workflow runs
  - **Then:** Both `run-001.jsonl` (no payloads) and `run-001.llm.jsonl` (with payloads) exist on disk
- **Integration:** `TEA-OBS-003.1-INT-008` (P1)
  - **Given:** Capture on, slim file inspected
  - **When:** Each line parsed
  - **Then:** No payload fields in any record of the slim file (existing tools see no change)

#### AC-8: `.llm.jsonl` is one JSON object per line, identical span schema + payload fields
**Coverage: FULL (planned) — unit + integration**

- **Unit:** `TEA-OBS-003.1-UNIT-015` (P1)
  - **Given:** A captured span dict matching `LLMPayloadSpan`
  - **When:** Serialized via the payload exporter
  - **Then:** Output is exactly one line ending in `\n`; `json.loads` round-trips to the original dict
- **Integration:** `TEA-OBS-003.1-INT-009` (P0)
  - **Given:** End-to-end capture run producing `*.llm.jsonl`
  - **When:** File parsed line-by-line
  - **Then:** Each line is valid JSON; schema includes base span fields + all 7 payload fields

#### AC-9: When `trace_file` unset, fall back to `tea-llm-payloads-<run_id>.jsonl` and INFO-log the path
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-010` (P1)
  - **Given:** No `trace_file` set; `auto_trace_llm_payloads: true`
  - **When:** Workflow runs from a writable CWD
  - **Then:** File `tea-llm-payloads-<run_id>.jsonl` created in CWD with the captured payloads
- **Integration:** `TEA-OBS-003.1-INT-011` (P1)
  - **Given:** Capture activates with resolved output path
  - **When:** Engine bootstraps the exporter
  - **Then:** INFO-level log line emitted naming the resolved output file path (caplog asserts message + level)

### Part D — Selective per-node filtering

#### AC-10: Glob patterns match against the YAML node name (not the action token)
**Coverage: FULL (planned) — unit (P0 stack)**

- **Unit:** `TEA-OBS-003.1-UNIT-016` (P0)
  - **Given:** Glob `extract_batch_*`, current node name `extract_batch_3`
  - **When:** `_should_capture_payload` evaluated
  - **Then:** Returns `True`
- **Unit:** `TEA-OBS-003.1-UNIT-017` (P0)
  - **Given:** Glob `extract_batch_*`, current node name `correct`
  - **When:** `_should_capture_payload` evaluated
  - **Then:** Returns `False`
- **Unit:** `TEA-OBS-003.1-UNIT-018` (P1)
  - **Given:** Glob `llm.call` (action token), current node name `extract_batch_3` containing an `llm.call` action
  - **When:** Matcher evaluated
  - **Then:** Returns `False` (matches against YAML node name only, not action token — explicit anti-test for AC-10's intent)
- **Unit:** `TEA-OBS-003.1-UNIT-019` (P1)
  - **Given:** Multiple globs `[extract_*, review_*]`, node `extract_batch_3`
  - **When:** Matcher evaluated
  - **Then:** Returns `True` (any glob match wins)

#### AC-11: For `dynamic_parallel`, glob matches the rendered branch node name
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-012` (P1)
  - **Given:** `dynamic_parallel` rendering branches `extract_batch_0..3`, glob `extract_batch_*`
  - **When:** Workflow runs and each branch invokes `llm.call`
  - **Then:** All four branches captured; payload spans contain `node: extract_batch_<i>`

#### AC-12: Non-matching node produces slim span only — no payload, no `.llm.jsonl` write
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-013` (P0)
  - **Given:** Glob `[extract_batch_*]` and a workflow node `summarize` invoking `llm.call`
  - **When:** Workflow runs
  - **Then:** `summarize`'s span appears in slim file only; no entry for `summarize` in `*.llm.jsonl`

### Part E — Compatibility

#### AC-13: Default behavior is byte-identical to today
**Coverage: FULL (planned) — integration (gate-blocker)**

- **Integration:** `TEA-OBS-003.1-INT-014` (P0)
  - **Given:** Fixture workflow + pinned baseline `python/tests/fixtures/tracing/baseline_trace.jsonl`
  - **When:** Workflow runs with `auto_trace_llm_payloads` unset
  - **Then:** SHA256 of generated trace file equals SHA256 of baseline byte-for-byte; no `.llm.jsonl` produced

#### AC-14: Coexists with `auto_trace`, `trace_exporter`, `trace_file`, Opik
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-015` (P1)
  - **Given:** All four legacy flags configured plus `auto_trace_llm_payloads: true`
  - **When:** Workflow runs
  - **Then:** All exporters fire; no mutual exclusion error; Opik trace contains the standard fields, `.llm.jsonl` contains the payload fields
- **Integration:** `TEA-OBS-003.1-INT-016` (P2)
  - **Given:** Opik exporter active alone (no file exporter), capture on
  - **When:** Workflow runs
  - **Then:** AC-9 fallback file produced; Opik traces unaffected

#### AC-15: Works with TEA-DX-001.2's CLI `--trace-file`
**Coverage: FULL (planned) — integration**

- **Integration:** `TEA-OBS-003.1-INT-017` (P1)
  - **Given:** CLI `--trace-file ./out/run.jsonl --trace-llm-payloads`
  - **When:** `tea run` resolves paths
  - **Then:** `./out/run.jsonl` and `./out/run.llm.jsonl` both produced; payload path derives from the resolved trace path

### Part F — Quality

#### AC-16: Tests cover flag-off, flag-true, glob list, binary omission, field shape, both-files-when-active
**Coverage: FULL (planned) — meta (covered by union of all above)**

This is a meta-AC; satisfied transitively by the scenarios above. Concrete mapping:
- flag-off → INT-006, INT-014
- flag-true → INT-005, INT-007, INT-009
- glob list → UNIT-016..019, INT-012, INT-013
- binary omission → UNIT-010..014
- field shape → UNIT-006, UNIT-007, INT-005, INT-009
- both files when active → INT-007, INT-008

#### AC-17: `docs/python/observability.md` documents flag, schema, separate-file behavior, binary-omission rule
**Coverage: FULL (planned) — doc verification**

- **Doc check:** `TEA-OBS-003.1-DOC-001` (P2)
  - **Given:** `docs/python/observability.md` after implementation
  - **When:** Section inspected (manual review or doc-link checker)
  - **Then:** Sub-headings present for: flag values, captured field schema (all 7 fields), separate-file naming/derivation, binary-omission placeholder spec

#### AC-18: Docs warn about PII (CNPJ, financial data) and link to TEA-OBS-003.2 for retention
**Coverage: PARTIAL (planned) — doc verification**

- **Doc check:** `TEA-OBS-003.1-DOC-002` (P2)
  - **Given:** Observability docs after implementation
  - **When:** PII-warning sub-section inspected
  - **Then:** Warning is at the **top** of the section (per NFR mitigation); explicit CNPJ/financial-data mention; visible forward-link to TEA-OBS-003.2 retention story

**Why partial:** DOC-002 verifies the warning text, but no automated test guarantees the warning stays at the *top* of the section if the doc is later refactored. Recommend a lightweight markdown-structure check (e.g., assert the warning admonition appears within the first N headings/lines of the section) — see Recommendation #2 below.

---

## Risk- and NFR-Driven Add-on Scenarios (INT-018..023)

These are not tied to a single AC but cover gate-blocking risks/NFRs from the prior assessments:

| ID | Source | Coverage |
|---|---|---|
| INT-018 (P0) | Risk OPS-001 — exporter `IOError` isolation | Mock exporter raises `IOError` mid-write → `llm.call` returns LLM response unchanged; warning logged; no exception propagates |
| INT-019 (P0) | Risk DATA-002 — non-JSON-serializable values | Mock JSON encoder raises `TypeError`/`JSONEncodeError` → `llm.call` returns response unchanged; warning logged |
| INT-020 (P1) | Risk DATA-002 expanded — `datetime` / Pydantic / `Decimal` | Capture serializes without crashing or with documented coercion |
| INT-021 (P1) | Risk PERF-001 / NFR Performance — file-lock contention | `dynamic_parallel` `max_workers=8` with capture on → JSONL file is well-formed (no interleaved/truncated lines) |
| INT-022 (P2, non-gating) | NFR Performance — smoke benchmark | Capture-on vs capture-off latency overhead measured and posted; not gate-blocking but documented |
| INT-023 (P2) | NFR Security — JSONL warning header | Optional first-record warning header in `*.llm.jsonl` (if AC adopted) verified |

---

## Critical Gaps

**Within ACs 1–18:** none of "no coverage" severity. AC-18 is the only `partial`, and the gap is a structural-stability check rather than a functional gap.

**Outside the AC set (recommended additions):**

1. **Story-status gap (process):** Story is **Draft**. The 44 planned scenarios above are designed but not implemented. Until tests land in `python/tests/`, this trace establishes intent only. **Severity: high (process)**, but expected for a Draft story.

2. **Doc-structure stability (AC-18):** No automated check that the PII warning stays at the top of the observability section. **Severity: low.** Suggested action below.

3. **AC-3 merge-vs-override semantics:** The story text says CLI flag is *equivalent to* a YAML list, but doesn't pin whether a YAML list **plus** CLI patterns **merge** or whether CLI **replaces** YAML's list. INT-004 assumes merge — confirm with PO before implementation, otherwise the test will need to be re-baselined. **Severity: medium.**

4. **`run_id` shape for fallback path (AC-9):** Risk profile flagged OPS-003 (path collision if `run_id` is not uuid-shaped). No scenario asserts the fallback filename is uniquely scoped per run. **Severity: low.** Suggested test below.

5. **Permissions on output file:** NFR Security #3 — no scenario asserts `chmod 0600` on the `*.llm.jsonl` (Unix). No AC mandates it; treat as docs-only unless NFR escalates. **Severity: low.**

---

## Test Design Recommendations

1. **Promote Draft → In Progress only after planned tests are scaffolded.** The trace can be re-validated against actual file paths once `python/tests/test_llm_payload_capture.py` (or split files) exist.
2. **Add a doc-structure check** to DOC-002: assert the PII warning admonition (`!!! warning` / `> ⚠️`) appears within the first ≤ N lines of the observability `## Capturing LLM Payloads` section. Cheap regression guard.
3. **Pin AC-3 merge semantics in the story text** before tests are written. Either:
   - "CLI patterns *merge* with YAML list, deduplicated" (current INT-004 assumption), OR
   - "CLI patterns *replace* YAML list when present" (alternative).
4. **Add INT-024 (P3):** AC-9 fallback path uniqueness — two parallel runs in the same CWD with no `trace_file` produce two distinct `tea-llm-payloads-<run_id>.jsonl` files (no overwrite, no collision).
5. **Use Given-When-Then comments at the top of each test function** (this is the project's de-facto pattern in `test_observability_core.py` etc.) so the trace stays machine-verifiable when re-run post-implementation.

---

## Risk Assessment (Coverage View)

| Coverage State | Count | Risk |
|---|---|---|
| Full (planned) | 17 ACs | Low (assuming planned scenarios are implemented as designed) |
| Partial (planned) | 1 AC (AC-18) | Low — gap is doc-structure stability, not functional |
| None | 0 ACs | — |

**Composite risk while story is Draft:** Coverage is design-complete. Risk that planned scenarios fail to materialize as production tests is **medium** — the Definition of Done's "All ACs met" + AC-16's explicit test enumeration provide the implementation guard.

---

## Gate YAML Block (paste-ready)

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
      reason: 'Doc verification (DOC-002) covers PII-warning content but not its top-of-section structural placement; recommend a lightweight markdown-structure check.'
  status_caveat: 'Story is Draft; coverage reflects 44 planned scenarios from test design, not merged production tests. Re-run trace after implementation to validate against actual test files.'
  notes: 'See docs/qa/assessments/TEA-OBS-003.1-trace-20260501.md'
```

---

## Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-OBS-003.1-trace-20260501.md
```
