# Test Design: Story TEA-OBS-003.1 — Core LLM payload capture + separate exporter + flag

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO (non-interactive)
Story: [docs/stories/TEA-OBS-003.1-llm-payload-capture-core.md](../../stories/TEA-OBS-003.1-llm-payload-capture-core.md)
Risk profile: [TEA-OBS-003.1-risk-20260501.md](TEA-OBS-003.1-risk-20260501.md)
NFR assessment: [TEA-OBS-003.1-nfr-20260501.md](TEA-OBS-003.1-nfr-20260501.md)

---

## Test Strategy Overview

- **Total test scenarios:** 44
- **Unit tests:** 19 (43%)
- **Integration tests:** 23 (52%)
- **E2E tests:** 0 (0%) — story is fully exercisable via the YAML engine + exporter integration layer; no UI/cross-system journey
- **Doc verification tasks:** 2 (5%)
- **Priority distribution:** P0: 17 · P1: 17 · P2: 10 · P3: 0

**Shift-left bias:** Pure logic (settings parsing, glob matching, binary-omission walker, schema shape) → unit. Anything that crosses the YAML engine ↔ tracing ↔ filesystem boundary → integration. No E2E because the user-facing surface is the JSONL output file, which integration tests assert directly.

**Risk-driven emphasis:** The two High risks from the risk profile (SEC-001 PII via accidental capture; DATA-001 binary bloat / encode failure) drive 7 of the 17 P0 tests. The Medium risks (TECH-001 schema drift; PERF-001 latency; DATA-002 non-JSON values; OPS-001 exporter exception) each have at least one dedicated scenario.

---

## Test Scenarios by Acceptance Criterion

### AC-1 — `auto_trace_llm_payloads` accepts `false` / `true` / list of glob patterns

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-UNIT-001 | Unit | P0 | `_configure_from_settings` parses `auto_trace_llm_payloads: false` → engine attribute is `False` | Pure parsing logic, fast feedback, default path | — |
| TEA-OBS-003.1-UNIT-002 | Unit | P0 | Parses `auto_trace_llm_payloads: true` → engine attribute is `True` | Pure parsing | — |
| TEA-OBS-003.1-UNIT-003 | Unit | P0 | Parses `auto_trace_llm_payloads: [extract_batch_*, correct]` → engine attribute is `["extract_batch_*", "correct"]` | Pure parsing of list-shape variant | TECH-002 |
| TEA-OBS-003.1-UNIT-004 | Unit | P1 | Setting omitted entirely → engine attribute is `False`/`None` (no capture) | Default-off invariant; pairs with AC-13 | SEC-001 |
| TEA-OBS-003.1-UNIT-005 | Unit | P2 | Invalid type (int, dict, None-string) raises clear validation error with key name | Surfaces misconfiguration early rather than silent disable | — |

### AC-2 — CLI flag `--trace-llm-payloads` enables capture, overrides YAML

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-001 | Integration | P0 | `tea run` with YAML `auto_trace_llm_payloads: false` + CLI `--trace-llm-payloads` → capture active for the run | CLI-over-YAML override is the primary user surface; crosses CLI ↔ engine boundary | — |
| TEA-OBS-003.1-INT-002 | Integration | P1 | CLI flag absent + YAML `true` → capture active (control case) | Confirms CLI flag is purely additive, not required | — |

### AC-3 — CLI flag `--trace-llm-payloads-for <pattern>` (repeatable)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-003 | Integration | P1 | `--trace-llm-payloads-for foo_* --trace-llm-payloads-for bar_*` → merged glob list `[foo_*, bar_*]` reaches engine | Repeatable-flag semantics through Typer | — |
| TEA-OBS-003.1-INT-004 | Integration | P1 | YAML list `[a_*]` + CLI `--trace-llm-payloads-for b_*` → effective list is `[a_*, b_*]` (merge, not replace) | Documents merge semantics per NFR test #9 | — |

### AC-4 — Captured field shape (`messages_input`, `response_content`, `tokens_input`, `tokens_output`, `model`, `stop_reason`, `cost_usd`)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-UNIT-006 | Unit | P0 | `LLMPayloadSpan` TypedDict (or contract pin) declares all 7 fields with documented types | Locks the schema before implementation; prevents drift | TECH-001 |
| TEA-OBS-003.1-INT-005 | Integration | P0 | End-to-end `llm.call` with capture on → emitted span contains all 7 documented fields populated from a stubbed LiteLLM result | Asserts the wire-up between LiteLLM result builder and span metadata | TECH-002 |
| TEA-OBS-003.1-UNIT-007 | Unit | P1 | `cost_usd` is present and numeric when `litellm.completion_cost` returns a value | Field presence under happy path | — |
| TEA-OBS-003.1-UNIT-008 | Unit | P1 | `cost_usd` is `None` (or omitted) when `litellm.completion_cost` raises or returns None — no crash | Graceful degradation when cost computation unavailable | — |
| TEA-OBS-003.1-UNIT-009 | Unit | P1 | `stop_reason` mirrors `finish_reason` from LiteLLM response; absent when not provided | Field mapping correctness | — |

### AC-5 — Binary attachments replaced with `{"type": "binary_omitted", "size_bytes": N}` placeholders

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-UNIT-010 | Unit | P0 | Top-level bytes entry in messages → replaced with placeholder; `size_bytes` matches `len(b)` | Simplest binary shape; baseline | DATA-001 |
| TEA-OBS-003.1-UNIT-011 | Unit | P0 | OpenAI-vision-style nested base64 image (`{"type": "image_url", "image_url": {"url": "data:image/png;base64,..."}}`) → bytes/base64 payload replaced; outer structure preserved | Real-world LLM message shape | DATA-001 |
| TEA-OBS-003.1-UNIT-012 | Unit | P0 | Anthropic image content block (`{"type": "image", "source": {"type": "base64", "data": <bytes>}}`) → bytes replaced | Real-world LLM message shape | DATA-001 |
| TEA-OBS-003.1-UNIT-013 | Unit | P0 | 10 MB synthetic bytes → serialized JSONL line is < 1 KB; placeholder `size_bytes` is 10485760 | Stress test that walker actually omits, not just attempts | DATA-001 |
| TEA-OBS-003.1-UNIT-014 | Unit | P1 | Deeply nested mixed structure (list-of-dicts-of-lists with bytes leaves) → all bytes leaves replaced; non-bytes data untouched | Confirms recursive walk, not just top-level scan | DATA-001 |

### AC-6 — Default-off: no payload fields added to span

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-006 | Integration | P0 | Capture off → emitted span dict has identical key set to baseline (no `messages_input`, `response_content`, etc.) | Field-set parity invariant; pairs with AC-13 byte-identical | SEC-001 |

### AC-7 — Separate `<trace_file_basename>.llm.jsonl` file in addition to slim file

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-007 | Integration | P0 | `--trace-file run-001.jsonl` + capture on → both `run-001.jsonl` (slim) and `run-001.llm.jsonl` (payloads) exist after run | Separate-file invariant is the user-visible promise | TECH-001 |
| TEA-OBS-003.1-INT-008 | Integration | P1 | Slim file content has zero payload fields (`messages_input` etc. not present in any line) | Legacy-tool compatibility — slim consumers see no change | TECH-001 |

### AC-8 — `.llm.jsonl` is JSONL, schema = spans + payload fields

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-009 | Integration | P0 | Every line of `.llm.jsonl` parses as JSON via `json.loads`; line count matches captured-call count | JSONL well-formedness | DATA-002 |
| TEA-OBS-003.1-UNIT-015 | Unit | P1 | Schema parity: a single span fixture serialized to slim and to payload-bearing variants → payload variant is strict superset of slim variant | Single-source-of-truth serialization (TECH-001 mitigation: shared path with strip-projection) | TECH-001 |

### AC-9 — Fallback `tea-llm-payloads-<run_id>.jsonl` when `trace_file` unset; INFO log

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-010 | Integration | P1 | `trace_file` unset + capture on → file `tea-llm-payloads-<run_id>.jsonl` written in CWD | Fallback path | OPS-003 |
| TEA-OBS-003.1-INT-011 | Integration | P1 | INFO-level log emitted at capture activation, naming the resolved output file path | Operator visibility (NFR Security recommendation, gate-blocker) | SEC-001 |

### AC-10 — Glob matches YAML node name (not action token)

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-UNIT-016 | Unit | P0 | `_should_capture_payload("extract_batch_3", ["extract_batch_*"])` → `True` | Core glob match | TECH-002 |
| TEA-OBS-003.1-UNIT-017 | Unit | P0 | `_should_capture_payload("llm.call", ["extract_batch_*"])` → `False` (action token, not node name) | Negative case proving we match node name, not action | TECH-002 |
| TEA-OBS-003.1-UNIT-018 | Unit | P1 | Multiple patterns — any match → `True`; matches first hit | Multi-pattern semantics | — |
| TEA-OBS-003.1-UNIT-019 | Unit | P1 | No patterns match → `False`; bool `True` matches all node names | Wildcard semantics for `True` setting | — |

### AC-11 — `dynamic_parallel` rendered branch name is matched

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-012 | Integration | P1 | YAML with `dynamic_parallel` rendering branches `extract_batch_0..2` + glob `[extract_batch_*]` → all three branches captured | Confirms node-name resolution happens after dynamic rendering, not before | TECH-002 |

### AC-12 — Non-matching node produces slim span only

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-013 | Integration | P0 | YAML node `bar` + glob `[foo_*]` → slim span only; no `.llm.jsonl` line for `bar` | Selective filtering invariant | SEC-001 |

### AC-13 — Default-off byte-identical to today

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-014 | Integration | P0 | Run a known fixture workflow with `auto_trace_llm_payloads` unset → SHA256 of `<trace>.jsonl` matches a baseline captured pre-change | Strongest single regression guard for the entire feature | SEC-001, TECH-001 |

### AC-14 — Coexistence with `auto_trace`, `trace_exporter`, Opik

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-015 | Integration | P1 | `auto_trace=true` + `trace_exporter=opik` + `auto_trace_llm_payloads=true` → both Opik exporter and LLM payload exporter receive spans without conflict | Confirms exporter chain composability | — |
| TEA-OBS-003.1-INT-016 | Integration | P2 | `FileExporter` + `OpikExporter` + payload exporter together → no exporter errors, all three sinks have expected entries | Exporter chain correctness under load | — |

### AC-15 — Works with TEA-DX-001.2's CLI `--trace-file`

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-017 | Integration | P1 | `--trace-file custom.jsonl --trace-llm-payloads` → `custom.llm.jsonl` is produced (path derived from resolved trace path) | Cross-story integration with sibling DX story | — |

### AC-16 — Test catalog (meta-AC; satisfied by the union above)

The 6 explicitly required scenarios in AC-16 are covered as follows:
- "flag off → no capture" → INT-006, INT-014
- "flag true → all calls captured" → INT-005, INT-007
- "glob list → only matching captured" → INT-013, UNIT-016, UNIT-017
- "binary message omission" → UNIT-010..014
- "field shape correctness" → UNIT-006, INT-005
- "both files exist" → INT-007, INT-008

### AC-17 — `docs/python/observability.md` documents flag, schema, separate file, binary rule

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-OBS-003.1-DOC-001 | Doc verification | P2 | Section in `docs/python/observability.md` exists and references: flag name, captured field schema, separate-file behavior, binary-omission rule | Documentation completeness |

### AC-18 — PII warning at top + forward-link to TEA-OBS-003.2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-OBS-003.1-DOC-002 | Doc verification | P2 | PII warning is the first sub-heading or callout in the section (not buried); forward-link to TEA-OBS-003.2 is present and resolves | NFR Security recommendation; risk SEC-001 mitigation |

---

## Risk-Driven & NFR-Driven Tests (beyond explicit ACs)

These are not pinned to a specific AC but are required by the risk profile / NFR assessment to converge the gate from CONCERNS to PASS.

| ID | Level | Priority | Test | Justification | Mitigates |
|----|-------|----------|------|---------------|-----------|
| TEA-OBS-003.1-INT-018 | Integration | P0 | Mock exporter raises `IOError` mid-write → `llm.call` returns the LLM response unchanged; warning is logged; span emission is skipped | Exporter-exception isolation (gate-blocker) | OPS-001 |
| TEA-OBS-003.1-INT-019 | Integration | P0 | Mock JSON encoder raises `TypeError`/`JSONEncodeError` → `llm.call` returns response unchanged; warning logged | Encode-error isolation (NFR Reliability test #3) | OPS-001 |
| TEA-OBS-003.1-INT-020 | Integration | P1 | Messages contain `datetime`, Pydantic model, `Decimal`, intentionally-unserializable object → exporter swallows error, action returns normally | Non-JSON value tolerance | DATA-002 |
| TEA-OBS-003.1-INT-021 | Integration | P1 | `dynamic_parallel` with 8 concurrent `llm.call`s under capture → `.llm.jsonl` has 8 well-formed lines, no truncation, no interleaved JSON | Concurrent-writer correctness under file lock | PERF-001, OPS-001 |
| TEA-OBS-003.1-INT-022 | Integration | P2 | Smoke benchmark: capture-off vs capture-on median + p99 latency for typical (4 KB) and large (256 KB) payloads — logged, non-gating | Quantifies PERF-001 overhead per NFR recommendation | PERF-001 |
| TEA-OBS-003.1-INT-023 | Integration | P2 | Fallback path with read-only CWD (simulated permission error) → log warning, drop the payload, execution continues | Fallback-failure handling per NFR Reliability missing-consideration #7 | OPS-002 |

---

## Risk Coverage Map

| Risk ID | Score | Tests covering |
|---------|-------|----------------|
| SEC-001 (PII / sensitive data persisted) | 6 H | UNIT-004, INT-006, INT-011, INT-013, INT-014, DOC-002 |
| DATA-001 (binary bytes serialized) | 6 H | UNIT-010, UNIT-011, UNIT-012, UNIT-013, UNIT-014 |
| TECH-001 (slim/payload schema drift) | 4 M | UNIT-006, UNIT-015, INT-007, INT-008, INT-014 |
| PERF-001 (sync write latency) | 4 M | INT-021, INT-022 |
| DATA-002 (non-JSON values) | 4 M | INT-009, INT-020 |
| OPS-001 (exporter exception escapes) | 3 L | INT-018, INT-019, INT-021 |
| SEC-002 (secrets in system prompts) | 3 L | DOC-002 (documentation only — no scrubber in scope this story) |
| OPS-002 (unbounded disk usage) | 2 L | INT-023 (fallback failure); retention deferred to TEA-OBS-003.2 |
| TECH-002 (engine reference threading) | 2 L | UNIT-003, UNIT-016, UNIT-017, INT-005, INT-012 |
| OPS-003 (run_id path collision) | 1 Min | INT-010 |

All Critical / High risks have ≥ 3 tests. All Medium risks have ≥ 2 tests. No High risk has only documentation coverage.

---

## Test Data & Environment Requirements

### Fixtures
- **F-1 — Stub LiteLLM result builder:** Fixture returning a synthetic `ModelResponse` with controllable `content`, `usage.prompt_tokens`, `usage.completion_tokens`, `model`, `finish_reason`, and an injectable `litellm.completion_cost` value (or exception). Required by UNIT-007/008/009, INT-005.
- **F-2 — Binary message corpus:** Static fixture file containing the 4 message shapes used by UNIT-010..014 (top-level bytes, OpenAI vision base64, Anthropic image base64, deeply nested), plus a runtime-generated 10 MB byte buffer for UNIT-013.
- **F-3 — Baseline trace file:** Pre-change SHA256-pinned `<trace>.jsonl` from a known fixture workflow, checked into `python/tests/fixtures/tracing/baseline_trace.jsonl`. Used by INT-014 (default-off byte-identical).
- **F-4 — Dynamic-parallel fixture YAML:** Workflow that uses `dynamic_parallel` to render `extract_batch_0..N` branches, each invoking `llm.call`. Used by INT-012, INT-021.
- **F-5 — Failing-exporter mock:** `pytest` fixture that wraps `LLMPayloadFileExporter` and raises `IOError`/`TypeError` on the Nth `export()` call. Used by INT-018, INT-019.

### Environment
- **Python:** 3.11+ (matches existing CI matrix; nothing new required)
- **Filesystem:** writable temp dir via `tmp_path` pytest fixture for all integration tests writing JSONL
- **No network:** All LLM calls are stubbed via the LiteLLM result-builder fixture (F-1) — no real API keys, no live model traffic
- **No DB:** This story touches tracing/filesystem only; no LTM backend involvement
- **Concurrency:** INT-021 uses `dynamic_parallel` with `max_workers=8` to exercise file-lock contention; runs in-process

### CI Integration
- All scenarios run under `pytest python/tests/` in the existing `python-tests.yaml` GitHub Actions workflow
- INT-022 (smoke benchmark) is `pytest -m benchmark` and must NOT block the gate; results posted to PR as a comment via existing benchmark-reporter (or stdout if not yet wired)
- DOC-001 / DOC-002 are verified via a documentation-link checker (or manual review during the gate review) — they are advisory, not gate-blocking on their own

---

## Recommended Execution Order

1. **P0 Unit** (fast feedback, fail-loudest-first): UNIT-001..003, UNIT-006, UNIT-010..013, UNIT-016..017
2. **P0 Integration** (gate-blockers): INT-001, INT-005, INT-006, INT-007, INT-009, INT-013, INT-014, INT-018, INT-019
3. **P1 Unit:** UNIT-004, UNIT-007..009, UNIT-014, UNIT-015, UNIT-018..019
4. **P1 Integration:** INT-002..004, INT-008, INT-010..012, INT-015, INT-017, INT-020..021
5. **P2 Integration & Doc:** UNIT-005, INT-016, INT-022..023, DOC-001, DOC-002

---

## Coverage Validation

- ✅ Every AC (1–18) has at least one test scenario.
- ✅ No duplicate coverage across levels — only AC-13 has overlapping concerns (UNIT vs INT vs the byte-identical baseline) and that overlap is intentional defense-in-depth for the strongest invariant in the story.
- ✅ Every High and Medium risk has at least one P0 or P1 test.
- ✅ Every "must-fix before merge" item from the risk profile is mapped: binary omission walk (UNIT-010..014), default-off byte-identical (INT-014), exporter-exception isolation (INT-018, INT-019).
- ✅ Every "Acceptance Criteria to Move Gate from CONCERNS → PASS" item from the NFR assessment is covered:
  - Security INFO log → INT-011
  - Security PII warning at top → DOC-002
  - Performance smoke benchmark → INT-022
  - Reliability default-off byte-identical → INT-014
  - Reliability fallback path → INT-010, INT-023
  - Reliability exporter exception isolation → INT-018, INT-019
  - Maintainability `LLMPayloadSpan` schema pin → UNIT-006

---

## Output 2: Gate YAML Block (paste-ready)

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
  risk_coverage:
    SEC-001: [UNIT-004, INT-006, INT-011, INT-013, INT-014, DOC-002]
    DATA-001: [UNIT-010, UNIT-011, UNIT-012, UNIT-013, UNIT-014]
    TECH-001: [UNIT-006, UNIT-015, INT-007, INT-008, INT-014]
    PERF-001: [INT-021, INT-022]
    DATA-002: [INT-009, INT-020]
    OPS-001: [INT-018, INT-019, INT-021]
    SEC-002: [DOC-002]
    OPS-002: [INT-023]
    TECH-002: [UNIT-003, UNIT-016, UNIT-017, INT-005, INT-012]
    OPS-003: [INT-010]
  must_fix_before_merge:
    - TEA-OBS-003.1-INT-014  # default-off byte-identical
    - TEA-OBS-003.1-UNIT-010 # top-level bytes omission
    - TEA-OBS-003.1-UNIT-011 # OpenAI-vision nested base64 omission
    - TEA-OBS-003.1-UNIT-012 # Anthropic image nested bytes omission
    - TEA-OBS-003.1-UNIT-013 # 10 MB stress
    - TEA-OBS-003.1-INT-018  # exporter IOError isolation
    - TEA-OBS-003.1-INT-019  # exporter encode-error isolation
    - TEA-OBS-003.1-INT-011  # INFO log on activation (NFR security gate)
    - TEA-OBS-003.1-UNIT-006 # LLMPayloadSpan schema pin (NFR maintainability gate)
```

---

## Output 3: Trace References

```text
Test design matrix: docs/qa/assessments/TEA-OBS-003.1-test-design-20260501.md
P0 tests identified: 17
Must-fix-before-merge tests identified: 9
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (no over-testing — pure logic is unit, exporter wiring is integration, no E2E because no user journey)
- [x] No duplicate coverage across levels (only intentional defense-in-depth on AC-13)
- [x] Priorities align with business risk (High risks → P0; Medium → P0/P1; Low → P1/P2)
- [x] Test IDs follow naming convention `TEA-OBS-003.1-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] All "must-fix before merge" items from the risk profile have a dedicated test
- [x] All NFR gate-converging items have a dedicated test
