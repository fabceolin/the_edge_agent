# Requirements Traceability Matrix

## Story: TEA-DX-001.3 — Intermediate state dumps for debug (`tea run --debug-state <dir>`)

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO
**Story status at trace time:** Draft (no implementation, no test code yet)

---

## Trace Inputs

- Story file: [`docs/stories/TEA-DX-001.3-intermediate-state-dumps-debug.md`](../../stories/TEA-DX-001.3-intermediate-state-dumps-debug.md)
- Risk profile: [`TEA-DX-001.3-risk-20260501.md`](TEA-DX-001.3-risk-20260501.md)
- NFR assessment: [`TEA-DX-001.3-nfr-20260501.md`](TEA-DX-001.3-nfr-20260501.md)
- Test design: [`TEA-DX-001.3-test-design-20260501.md`](TEA-DX-001.3-test-design-20260501.md)
- Source of test mappings: the test-design 18-scenario matrix (UNIT-001..007 / INT-001..014 / E2E-001 / DOC-001). No test code exists yet — this trace maps **AC → planned test scenario**, not AC → existing pytest.

---

## Coverage Summary

| Bucket | Count | % | Notes |
|--------|-------|---|-------|
| Total testable requirements | 15 | — | 12 stated ACs + 3 proposed (AC-13/14/15) |
| Fully covered (planned) | 13 | 87% | At least one P0/P1 scenario each |
| Partially covered (planned) | 2 | 13% | AC-7 single-cell matrix; AC-12 docs-only DOC-001 review |
| Not covered (planned) | 0 | 0% | — |
| Implementation status | 0 of 18 | 0% | All scenarios are designed but not yet authored — story is Draft |

> **Headline:** Test design *coverage* is complete on paper. Trace's red flag is that **none of these tests exist yet** and that two pre-implementation blockers (AC-4 redaction contract; engine-event mapping for AC-9/AC-13) make several P0 scenarios un-authorable today.

---

## Requirement Mappings (Given-When-Then)

### AC-1 — `tea run --debug-state <dir>` flag exists (default off)

**Coverage: FULL (planned)**

- **Unit** — `TEA-DX-001.3-UNIT-001` (P1)
  - **Given:** the Typer CLI is loaded
  - **When:** `tea run --help` is invoked
  - **Then:** stdout contains a `--debug-state` line with a one-line description

### AC-2 — After each node completes, write `<dir>/<NN>-after-<node_name>.json`

**Coverage: FULL (planned)**

- **Integration** — `TEA-DX-001.3-INT-002` (P0, alias of AC-10)
  - **Given:** a 3-sequential-node YAML workflow and an empty dump dir
  - **When:** `tea run --debug-state /tmp/d3 ...` runs to completion
  - **Then:** exactly 3 files exist — `01-after-n1.json`, `02-after-n2.json`, `03-after-n3.json` in lexical order
- **Unit** — `TEA-DX-001.3-UNIT-002` (P1)
  - **Given:** the filename formatter helper
  - **When:** called with `(step=7, node="my_node", suffix="after")` and overflow `(step=100, ...)`
  - **Then:** returns `"07-after-my_node.json"` and `"100-..."` respectively (zero-pad to 2, no truncation at 100)
- **Unit** — `TEA-DX-001.3-UNIT-003` (P1)
  - **Given:** a state dict and the dump writer
  - **When:** the writer serializes for one node
  - **Then:** file bytes equal `json.dumps(state, cls=TeaJSONEncoder, indent=2)` byte-for-byte (locks in reuse of `cli.py:1838` serializer)

### AC-3 — On node failure, write `<dir>/<NN>-FAILED-<node_name>.json` with pre-node state + traceback

**Coverage: FULL (planned)**

- **Integration** — `TEA-DX-001.3-INT-003` (P0, alias of AC-11)
  - **Given:** a 3-node workflow where node 2 raises `RuntimeError("boom")`
  - **When:** the workflow runs with `--debug-state`
  - **Then:** files `01-after-n1.json` and `02-FAILED-n2.json` exist (no `03-*`); the FAILED file is JSON with keys `{"state", "traceback"}` where `state` reflects the input to n2 and `traceback` includes `RuntimeError: boom`
- **Unit** — `TEA-DX-001.3-UNIT-004` (P0)
  - **Given:** `(state_pre_node, exception, tb_str)`
  - **When:** the FAILED-payload builder runs
  - **Then:** result dict has exactly `{"state", "traceback"}`; `state` is the pre-node snapshot, not post-mutation
- **Integration** — `TEA-DX-001.3-INT-004` (P1)
  - **Given:** a workflow whose first node raises immediately
  - **When:** the workflow runs with `--debug-state`
  - **Then:** `01-FAILED-n1.json` is the only file; its `state` field equals workflow input

### AC-4 — Reuse the secrets-redaction logic that excludes secrets from checkpoints

**Coverage: FULL (planned, but AT RISK — premise unresolved)**

- **Unit** — `TEA-DX-001.3-UNIT-005` (P0)
  - **Given:** a state `{"api_key": "sk-CANARY-XYZ", "user": "alice", "config": {"api_key": "sk-CANARY-XYZ"}}`
  - **When:** `redact_state(state, secret_keys=["api_key"])` is called
  - **Then:** result is `{"api_key": "***", "user": "alice", "config": {"api_key": "***"}}` (top-level + nested redaction)
- **Integration** — `TEA-DX-001.3-INT-005` (P0)
  - **Given:** a workflow that places `sk-CANARY-XYZ` into state
  - **When:** the workflow runs with `--debug-state`
  - **Then:** no `*.json` file in the dump dir contains the literal substring `sk-CANARY-XYZ`
- **Integration** — `TEA-DX-001.3-INT-006` (P0)
  - **Given:** a node body raises `RuntimeError(f"key sk-CANARY-XYZ rejected")`
  - **When:** the FAILED dump is written
  - **Then:** the `traceback` field does not contain `sk-CANARY-XYZ` (masked through `mask_credentials`)

> **Trace caveat:** The NFR assessment showed that the "secrets-redaction pass already used for checkpoints" referenced in AC-4 does not exist (`checkpoint.py:111` uses raw `pickle.dump`; `cache.py:83` `mask_credentials` is text-only). UNIT-005/INT-005/INT-006 are written against **Option A** (build `redact_state` + run tracebacks through `mask_credentials`). If implementation picks Option B (document only) or Option C (`--debug-state-redact-keys` user list), these mappings need rewriting before tests are authored.

### AC-5 — Directory created if missing; existing files preserved

**Coverage: FULL (planned)**

- **Integration** — `TEA-DX-001.3-INT-007` (P1)
  - **Given:** (a) a non-existent dump dir, and (b) a pre-existing `99-marker.txt` placed before run
  - **When:** the workflow runs with `--debug-state` pointing at each
  - **Then:** (a) the dir is created and dumps land inside; (b) `99-marker.txt` is still present after the run

### AC-6 — When flag absent, behavior is unchanged

**Coverage: FULL (planned)**

- **Integration** — `TEA-DX-001.3-INT-001` (P0)
  - **Given:** a 3-node workflow and no `--debug-state` flag
  - **When:** `tea run` executes
  - **Then:** no dump files are written anywhere, and `--output` is byte-equal to a pre-change baseline

### AC-7 — Works with `--quiet`, `--stream`, `--show-graph`

**Coverage: PARTIAL (planned — single matrix cell, edges of compatibility surface not enumerated)**

- **Integration** — `TEA-DX-001.3-INT-008` (P1)
  - **Given:** a 3-node workflow
  - **When:** the workflow is run under the matrix `{--debug-state} × {--quiet, --stream, --show-graph, no-flag}`
  - **Then:** in every cell exactly 3 dump files are produced and stdout/NDJSON output is unchanged versus the same flags without `--debug-state`

> **Gap (minor):** INT-008 enumerates the *output streams* but not the interaction matrix in pairs (e.g., `--quiet --stream`). Acceptable risk for v1 — combinations are independent. Logged as a watch-item, not a blocker.

### AC-8 — Compatible with `--checkpoint`

**Coverage: FULL (planned)**

- **Integration** — `TEA-DX-001.3-INT-009` (P1)
  - **Given:** both `--debug-state /tmp/d3` and `--checkpoint /tmp/cp` set, with disjoint dirs
  - **When:** the workflow runs
  - **Then:** checkpoints land in `/tmp/cp`, dumps land in `/tmp/d3`, no overlap; resume-counter behavior is asserted (continue from resume step **or** restart at 01 — pinned to whichever the implementer chooses)

### AC-9 — Parallel/`dynamic_parallel` parent-node serialization timing

**Coverage: FULL (planned, but AT RISK — engine-event mapping unresolved)**

- **Integration** — `TEA-DX-001.3-INT-010` (P0)
  - **Given:** a graph `entry → fanout → [a,b,c] → fanin → end` with `--debug-state`
  - **When:** the workflow runs to completion
  - **Then:** exactly 2 files: `01-after-fanout.json` and `02-after-fanin.json` (NOT 5 — branches do not trigger their own dumps)
- **Integration** — `TEA-DX-001.3-INT-011` (P0)
  - **Given:** the same graph shape but expressed as `dynamic_parallel`
  - **When:** the workflow runs
  - **Then:** same 2-file assertion as INT-010
- **Unit** — `TEA-DX-001.3-UNIT-006` (P1)
  - **Given:** a synthetic event stream `[state(n1), parallel_state(branch_a), branch_complete(branch_a), parallel_state(branch_b), branch_complete(branch_b), parallel_complete(fanout), state(fanin), final]`
  - **When:** the dump dispatcher consumes it
  - **Then:** exactly 2 dump-write calls (`fanout`, `fanin`) — confirms parent-only dispatch logic in isolation from real graph

> **Trace caveat:** NFR assessment found StateGraph emits `"state"` / `"parallel_state"` / `"branch_complete"` / `"parallel_complete"` / `"final"` / `"error"` — there is **no** `"node_complete"` event despite the story's Technical Notes referring to one. The CLI synthesizes equivalents from `"state"` (`cli.py:1821-1824`). UNIT-006/INT-010/INT-011 assume the dispatcher will key off `state` (sequential) + `parallel_complete` (parent of parallel fan-out). If a different mapping is chosen, these tests need rewriting.

### AC-10 — Unit test: 3 nodes + `--debug-state` produces 3 files in order

**Coverage: FULL (planned)**

- Alias of `TEA-DX-001.3-INT-002` (see AC-2). Test design intentionally hoisted this from "unit" to "integration" because the assertion requires the real CLI run-loop, not a mocked dispatcher. The story's "unit test" wording is interpreted as "small functional test."

### AC-11 — Integration test: failure in node 2 produces matched files with traceback

**Coverage: FULL (planned)**

- Alias of `TEA-DX-001.3-INT-003` (see AC-3).

### AC-12 — `--debug-state` documented with redaction warning

**Coverage: PARTIAL (planned — doc review + WARNING line, but redaction-scope language is conditional on AC-4 outcome)**

- **Doc** — `TEA-DX-001.3-DOC-001` (P2)
  - **Given:** the user-facing CLI reference at `docs/python/...` and the troubleshooting section
  - **When:** the PR reviewer reads the docs
  - **Then:** `--debug-state` is listed with explicit redaction-scope language and a "development-only" callout
- **Integration** — `TEA-DX-001.3-INT-012` (P0)
  - **Given:** `--debug-state` is set on the CLI invocation
  - **When:** the run starts (regardless of `--quiet`)
  - **Then:** a single `WARNING` line appears on stderr at startup

> **Gap (medium):** the redaction-scope sentence in the docs cannot be written until AC-4's contract is picked. DOC-001 is a manual reviewer step, so the assertion is "language explicitly documents what is and isn't redacted." If the implementer ships before that contract is fixed, DOC-001 will be hand-waved.

### AC-13 (proposed) — Sequential dumps on `state`; parallel parents dump once on `parallel_complete`; per-branch events do not trigger dumps

**Coverage: FULL (planned, contingent on acceptance into story)**

- Aliases `INT-010`, `INT-011`, `UNIT-006` — see AC-9 mappings.

> **Trace status:** This AC is *proposed* by the NFR assessment, not yet accepted into the Acceptance Criteria section. The test design has already authored test specs against it. Accept the AC into the story before the next sprint begins.

### AC-14 (proposed) — FAILED-dump write wrapped in its own `try/except OSError`; original node exception propagates on dump-write failure

**Coverage: FULL (planned, contingent on acceptance)**

- **Integration** — `TEA-DX-001.3-INT-013` (P1)
  - **Given:** a `chmod 0o555` read-only dump dir (POSIX-only) and a node that raises `RuntimeError("real-error")`
  - **When:** the workflow runs and the FAILED-write hits `OSError`
  - **Then:** process exits non-zero with `RuntimeError("real-error")` (not `OSError`); stderr notes the dump-write failure as a secondary message

### AC-15 (proposed) — Node-name path component sanitized (`[^A-Za-z0-9_-]` → `_`)

**Coverage: FULL (planned, contingent on acceptance)**

- **Unit** — `TEA-DX-001.3-UNIT-007` (P0)
  - **Given:** the sanitizer helper
  - **When:** called with `"../../etc/passwd"`, `"with space"`, `"dotted.name"`, `"ok-name_1"`
  - **Then:** returns `"______etc_passwd"`, `"with_space"`, `"dotted_name"`, `"ok-name_1"` respectively
- **Integration** — `TEA-DX-001.3-INT-014` (P1)
  - **Given:** a `dynamic_parallel` graph that produces a node named `"../escape"`
  - **When:** the workflow runs with `--debug-state`
  - **Then:** no file is written outside the dump dir; the resulting filename contains the sanitized form

### Definition of Done — original `plan_batches` repro

**Coverage: FULL (planned)**

- **E2E** — `TEA-DX-001.3-E2E-001` (P1)
  - **Given:** a 6-node workflow modeled after `plan_batches` with an intentional Jinja error at node 3
  - **When:** the user runs it via `subprocess.run([sys.executable, "-m", "the_edge_agent.cli", "run", ...])` with `--debug-state`
  - **Then:** the dump dir contains at minimum `02-after-n2.json` (last good) and `03-FAILED-n3.json` (failure point); the user can post-mortem inspect both without re-running

---

## Coverage Summary by AC

| AC | Description | Coverage | Test IDs | P0 count |
|----|-------------|----------|----------|----------|
| AC-1 | `--debug-state` flag exists | FULL | UNIT-001 | 0 |
| AC-2 | Per-node `<NN>-after-<node>.json` | FULL | INT-002, UNIT-002, UNIT-003 | 1 |
| AC-3 | FAILED file with state + traceback | FULL | INT-003, UNIT-004, INT-004 | 2 |
| AC-4 | Secrets redaction reuse | FULL (at risk) | UNIT-005, INT-005, INT-006 | 3 |
| AC-5 | Dir creation; preserve existing files | FULL | INT-007 | 0 |
| AC-6 | Flag absent → unchanged | FULL | INT-001 | 1 |
| AC-7 | Works with `--quiet`/`--stream`/`--show-graph` | PARTIAL | INT-008 | 0 |
| AC-8 | Compatible with `--checkpoint` | FULL | INT-009 | 0 |
| AC-9 | Parallel parent-only dumps | FULL (at risk) | INT-010, INT-011, UNIT-006 | 2 |
| AC-10 | Happy path: 3 nodes → 3 files | FULL | INT-002 (alias) | 1 |
| AC-11 | Failure: matched files w/ traceback | FULL | INT-003 (alias) | 1 |
| AC-12 | Docs + WARNING | PARTIAL | DOC-001, INT-012 | 1 |
| AC-13 (proposed) | Engine-event mapping | FULL | INT-010/011, UNIT-006 (alias) | 2 |
| AC-14 (proposed) | FAILED-write isolation | FULL | INT-013 | 0 |
| AC-15 (proposed) | Filename sanitization | FULL | UNIT-007, INT-014 | 1 |
| DoD | `plan_batches` repro | FULL | E2E-001 | 0 |

**Totals:** 15 requirements / 13 FULL · 2 PARTIAL · 0 NONE.

---

## Critical Gaps

### 1. Implementation gap (entire story)

- **Gap:** No tests have been authored. No `tea run --debug-state` flag exists. Story is in **Draft**.
- **Risk:** Critical (in the sense that 0% of designed coverage is realized); but expected for a draft.
- **Action:** Resolve the two pre-implementation blockers below, then author tests in priority order from the test-design execution plan.

### 2. AC-4 redaction contract unresolved (blocker for SEC scenarios)

- **Gap:** UNIT-005 / INT-005 / INT-006 are written against Option A (build `redact_state` + traceback masking). Story still refers to a non-existent "secrets-redaction pass already used for checkpoints" (NFR FAIL).
- **Risk:** **High.** SEC-001 (risk profile) plus NFR Security FAIL. If the implementer picks Option A but never reads NFR, they won't build the helper. If the team picks Option B (document-only), all three SEC scenarios degrade to a single "WARNING is emitted" assertion + a docs review — that needs to be a deliberate AC change, not a silent slip.
- **Action:** Story owner picks Option A/B/C explicitly in Acceptance Criteria; rewrite AC-4 accordingly; if A/C, the SEC tests stand. If B, replace UNIT-005/INT-005/INT-006 with a single WARN+docs assertion.

### 3. Engine-event mapping unresolved (blocker for AC-9 / AC-13 scenarios)

- **Gap:** Story says "on every event of type `node_complete` (or the equivalent emitted by stategraph.py)". `node_complete` does not exist; the CLI synthesizes from `"state"`. INT-010 / INT-011 / UNIT-006 are pinned to a specific dispatcher mapping (`state` → sequential, `parallel_complete` → parallel parent). If implementation diverges (e.g., dumps off `branch_complete`), parallel runs produce 5 files instead of 2 — INT-010 fails.
- **Risk:** **Medium.** TECH-001 in risk profile.
- **Action:** Pin the event mapping in the story's Technical Notes section before implementation starts. Promote AC-13 from proposed to accepted.

### 4. AC-7 matrix is single-cell (minor)

- **Gap:** INT-008 covers `--debug-state × {--quiet, --stream, --show-graph, no-flag}` independently but not pairs (e.g., `--quiet --stream`). Pairwise interactions are typically independent in this CLI but not proven.
- **Risk:** **Low.**
- **Action:** Acceptable for v1. If a regression slips through, add pairwise cases in a follow-up.

### 5. AC-12 doc redaction language pending AC-4 outcome

- **Gap:** DOC-001 cannot finalize the redaction-scope paragraph until AC-4 is fixed.
- **Risk:** **Medium** (process risk, not technical).
- **Action:** Block DOC-001 sign-off until AC-4 is resolved; author docs at the same time as the redaction implementation lands.

---

## Risk-to-Trace Mapping

| Risk (from risk profile) | Severity | Covered by | Status |
|--------------------------|----------|------------|--------|
| SEC-001 — secret leakage to disk | High | UNIT-005, INT-005, INT-006 | Designed; **gated on AC-4** |
| TECH-001 — parallel branch double-dump | Medium | INT-010, INT-011, UNIT-006 | Designed; **gated on event mapping** |
| OPS-001 — accidentally enabled in prod | Medium | INT-012 (WARN line) | Designed |
| DATA-001 — disk fill | Medium | (accepted v1) | Documented as dev-only via DOC-001 |
| SEC-002 — path traversal | Low | UNIT-007, INT-014 | Designed; **gated on AC-15 acceptance** |
| TECH-002 — sync I/O slow | Low | (accepted v1) | Watched; not blocked |
| TECH-003 — FAILED-write masks original error | Low | INT-013 | Designed; **gated on AC-14 acceptance** |
| DATA-002 — stale dumps from prior runs | Low | UNIT-002 (counter overflow only) | Partial — counter monotonicity asserted; cross-run pollution is documented behavior (`rm -rf` between runs per AC-5) |
| DATA-003 — filename injection | Low | UNIT-007, INT-014 (dup of SEC-002) | Designed |
| OPS-002 — empty `--output` confusion | Minimal | (accepted) | Documented behavior |

All P0/P1 risks have a planned test mapping. None of the planned mappings have been implemented yet.

---

## Test Design Recommendations

1. **Resolve AC-4 first.** Until the redaction contract is picked, three P0 SEC tests are unwritable. This is the single largest blocker on the trace.
2. **Author UNIT-005, UNIT-007, UNIT-006, UNIT-004 in that order.** They are the cheapest, lock down the helpers, and unblock the integration tests.
3. **Author INT-001 second.** The "no flag → byte-identical baseline" guard is dirt-cheap and prevents accidental coupling.
4. **Treat INT-010 / INT-011 as the AC-9 spec, not as a unit test.** They will catch real engine-event mismatches that UNIT-006 cannot.
5. **Promote DOC-001 to a PR-review checklist item.** Manual doc review is otherwise easy to skip in YOLO mode.
6. **Consider a follow-up story `--debug-state-max-bytes`** to retire the v1 disk-fill caveat (DATA-001).

---

## Risk Assessment (post-trace)

- **High:** AC-4 unresolved (SEC-001 carries through).
- **Medium:** Engine-event mapping unresolved (TECH-001 / AC-9 / AC-13); AC-12 docs depend on AC-4.
- **Low:** AC-7 matrix is single-cell; AC-15 / AC-14 are still proposed.
- **None blocked at the AC → test mapping level** — every AC has a planned test path. The blockers are *upstream* of test authoring (story content), not in the test design itself.

---

## Gate YAML Block

```yaml
trace:
  totals:
    requirements: 15
    full: 13
    partial: 2
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.3-test-design-20260501.md'
  uncovered: []
  partial_coverage:
    - ac: 'AC-7'
      reason: 'INT-008 covers each of {--quiet, --stream, --show-graph} independently against --debug-state but not their pairwise combinations.'
      severity: low
    - ac: 'AC-12'
      reason: 'DOC-001 redaction-scope paragraph cannot be authored until AC-4 redaction contract is resolved (Option A/B/C).'
      severity: medium
  blockers:
    - 'AC-4 redaction contract unresolved (gates SEC tests UNIT-005/INT-005/INT-006).'
    - 'Engine-event mapping unresolved (gates AC-9 tests INT-010/INT-011/UNIT-006 and proposed AC-13).'
  proposed_acs_pending_acceptance:
    - 'AC-13: sequential dumps on `state`; parallel parents on `parallel_complete`; per-branch events do not dump.'
    - 'AC-14: FAILED-write wrapped in own try/except OSError; original node exception propagates.'
    - 'AC-15: node-name path component sanitized to [A-Za-z0-9_-].'
  notes: 'See docs/qa/assessments/TEA-DX-001.3-trace-20260501.md. All 18 designed scenarios are unimplemented (story is Draft).'
```

---

## Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.3-trace-20260501.md
```

---

## Gate Recommendation (trace pass)

**CONCERNS** — coverage *plan* is complete and risk-aligned, but:

1. AC-4 redaction contract must be picked before the three P0 SEC tests can be authored.
2. Engine-event mapping must be pinned before the three AC-9/AC-13 tests can be authored.
3. AC-13 / AC-14 / AC-15 must be promoted from proposed to accepted.

Once those are resolved, the gate would upgrade to **PASS** at the trace layer. (Note: separate gates from risk profile (CONCERNS) and NFR assessment (FAIL on Security) still stand independently — story is **not safe to implement as currently written**.)

TRACE_REQUIREMENTS_COMPLETED
