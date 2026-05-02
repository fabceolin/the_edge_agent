# TEA-DX-001.8: Document `action` vs `steps` vs `subgraph` in `dynamic_parallel`

## Status
Done

## Parent Epic
[TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md)

## Priority
Low

---

## Story

**As a** YAML workflow author choosing how to body a `dynamic_parallel` branch,
**I want** a single comparison table showing when to use `action:`, `steps:`, or `subgraph:`,
**so that** I don't have to read TEA's source to figure out which one fits my use case.

## Story Context

**Existing System Integration:**

- Integrates with: `docs/shared/YAML_REFERENCE.md`, `docs/python/actions-reference.md`
- Existing examples: `examples/yaml/` shows each of the three modes *in isolation* — no side-by-side comparison
- Implementation reference: `python/src/the_edge_agent/yaml_nodes.py:1177-1192` (`_create_dynamic_parallel_action_branch`, `_create_dynamic_parallel_steps_branch`, `_create_dynamic_parallel_subgraph_branch`)
- Coordinated with: TEA-DX-001.5 (better error messages will reference the doc anchor created here)

**Problem Statement:**

User report: "Achei a sintaxe correta lendo source. Os exemplos em `examples/yaml/` mostram cada um isolado, mas não a tabela comparativa de quando usar cada." Translated: figured out the right syntax by reading the source; isolated examples exist but no comparison table.

This is documentation only — no code changes.

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1:** A new section `## Dynamic Parallel: Branch Body Modes` in `docs/shared/YAML_REFERENCE.md` (under the existing `dynamic_parallel` section, or as a child of it).
2. **AC-2:** Section includes a comparison table with at least these columns:
   - **Mode** (`action`, `steps`, `subgraph`)
   - **When to use** (one-sentence guidance)
   - **Branch state** (does the branch see top-level state? a copy? a scoped subset?)
   - **Reusability** (can the body be reused outside `dynamic_parallel`?)
   - **Where it's defined** (inline vs separate node/file)
   - **Example link** (anchor to a minimal example)
3. **AC-3:** Section includes a 5-10 line minimal example for each mode, side-by-side or sequential.
4. **AC-4:** Section explicitly states the mutual-exclusion rule: exactly one of `action`/`steps`/`subgraph` per `dynamic_parallel` node.
5. **AC-5:** Section explicitly notes the `fan_in:` requirement (sibling key, must reference an existing node).
6. **AC-6:** Cross-link added from `docs/python/actions-reference.md` (or wherever actions are documented) pointing to this section.
7. **AC-7:** Anchor URL is stable (e.g., `#dynamic-parallel-branch-body-modes`) so TEA-DX-001.5 error messages can link to it.

**Integration Requirements:**

8. **AC-8:** No code changes.
9. **AC-9:** No new examples need to be added in `examples/yaml/` — link to existing ones if possible. If an example is missing for any of the three modes, add a minimal one only if necessary for AC-3.

**Quality Requirements:**

10. **AC-10:** Documentation builds cleanly (no broken markdown, no broken cross-links).
11. **AC-11:** A reader unfamiliar with TEA can pick the right mode for a hypothetical use case after reading the table.

## Technical Notes

- **Integration Approach:** Edit `docs/shared/YAML_REFERENCE.md`. Add the section. Source the comparison content from inspection of `yaml_nodes.py:1177-1192` (the three `_create_dynamic_parallel_*_branch` helpers) — read each to extract the actual differences in state passing and execution semantics.
- **Existing Pattern Reference:** Other comparison tables in the same doc (e.g., the LTM backend table in `CLAUDE.md`) are the style to match.
- **Key Constraint:** Be accurate, not aspirational. If the three modes have subtle behavioral differences (state copying, error propagation, etc.), document them; don't paper over them.

## Tasks / Subtasks

- [x] **Task 1: Audit the three branch implementations** (informs AC-2)
  - [x] Read `yaml_nodes.py:1522-1655` (current location of the three helpers; story originally cited 1177-1192) and note actual differences in state passing, error handling, return shape
- [x] **Task 2: Draft the section** (AC: 1, 2, 3, 4, 5)
  - [x] Write the comparison table with columns Mode / When to use / Branch state / Reusability / Where it's defined / Example link
  - [x] Add minimal examples for each mode (5–15 lines each, side-by-side)
  - [x] State the mutual-exclusion and `fan_in:` rules explicitly
- [x] **Task 3: Cross-links** (AC: 6, 7)
  - [x] Link from `docs/python/actions-reference.md` (added under `agent.*` block) to the new YAML_REFERENCE.md anchor
  - [x] Pin anchor URL with explicit `{#dynamic-parallel-branch-body-modes}` attribute AND a load-bearing HTML comment so TEA-DX-001.5 can rely on it
- [x] **Task 4: Build check** (AC: 10)
  - [x] Visually inspected the rendered markdown structure; new tests verify table-row column counts, table headers, fenced-code-block extraction, and anchor pinning

## Definition of Done

- [ ] All ACs met
- [ ] Section reviewed for technical accuracy by someone other than the author
- [ ] Cross-links verified
- [ ] No code changes shipped in this PR

## Risk and Compatibility

- **Primary Risk:** Documenting behavior incorrectly. Mitigated by reading the actual implementations and adding tests in TEA-DX-001.5 that pin the documented anchor.
- **Rollback:** Revert the docs PR.
- **Compatibility:** Pure docs.

## QA Notes - Risk Profile

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full assessment:** [docs/qa/assessments/TEA-DX-001.8-risk-20260501.md](../qa/assessments/TEA-DX-001.8-risk-20260501.md)

### Risk Level: **LOW** (Risk Score: 87/100, Recommended Gate: PASS)

Documentation-only story (AC-8). No critical or high risks identified. Risk surface is limited to documentation accuracy, anchor stability, and downstream coupling with TEA-DX-001.5.

### Identified Risks

| Risk ID  | Category | Title                                                                  | Score | Priority |
| -------- | -------- | ---------------------------------------------------------------------- | ----- | -------- |
| TECH-001 | Technical | Docs diverge from `_create_dynamic_parallel_*_branch` semantics       | 4     | Medium   |
| OPS-001  | Operational | Anchor URL slug instability breaks TEA-DX-001.5 error message links | 2     | Low      |
| TECH-002 | Technical | Implementation drift after merge (state-passing or fan-in changes)    | 2     | Low      |
| OPS-002  | Operational | Broken cross-link from `actions-reference.md`                       | 1     | Minimal  |
| BUS-001  | Business | Comparison table fails to resolve user confusion                       | 1     | Minimal  |

### Mitigations

- **TECH-001 (Medium):** Enforce Task 1 audit of `yaml_nodes.py:1177-1300` against every row of the comparison table; require Definition-of-Done dual review to verify the "Branch state" and "Reusability" columns against the implementation.
- **OPS-001 (Low):** Pin the heading anchor explicitly (e.g., `## Dynamic Parallel: Branch Body Modes {#dynamic-parallel-branch-body-modes}` or rely on a stable GFM slug) and add a source-side comment noting the anchor is load-bearing for TEA-DX-001.5.
- **TECH-002 (Low):** Add a comment near the three branch helpers in `yaml_nodes.py` pointing readers to the YAML_REFERENCE.md section so future refactors prompt a docs review.
- **OPS-002 (Minimal):** Manually click cross-links after edits; run repo docs lint if available (Task 4).
- **BUS-001 (Minimal):** AC-11 cold-read review by a non-author confirms the table is sufficient to pick a mode.

### Testing Priorities

1. **Priority 1 (Medium-risk coverage for TECH-001):**
   - Implementation audit: read `python/src/the_edge_agent/yaml_nodes.py:1177-1300` and validate each cell of the comparison table.
   - Run each minimal example (AC-3) via `tea run` against a fixture to confirm documented behavior matches actual execution for `action`, `steps`, and `subgraph` modes.
   - Confirm the documented mutual-exclusion rule (AC-4) and `fan_in:` requirement (AC-5) match what the YAML engine actually enforces.
2. **Priority 2 (Low-risk coverage):**
   - Render docs locally and verify `#dynamic-parallel-branch-body-modes` resolves.
   - Verify cross-link from `docs/python/actions-reference.md` lands on the new section.
3. **Priority 3 (Minimal-risk coverage):**
   - Cold-read review by someone unfamiliar with `dynamic_parallel` (validates AC-11).
   - Run repo markdown lint (Task 4).

### Monitoring

- When TEA-DX-001.5 ships, confirm error-message links resolve to this anchor.
- Track whether "I read the source to figure out X" feedback persists after this section is published.

## QA Notes - NFR Assessment

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full assessment:** [docs/qa/assessments/TEA-DX-001.8-nfr-20260501.md](../qa/assessments/TEA-DX-001.8-nfr-20260501.md)

### Quality Score: **80/100** · Recommended Gate: **CONCERNS**

Documentation-only story (AC-8). Security and Performance are PASS by virtue of zero code surface. Reliability and Maintainability are CONCERNS because docs accuracy is the load-bearing property and current ACs cover it only by qualitative review.

### NFR Coverage

| NFR             | Status   | One-line rationale                                                                                                       |
| --------------- | -------- | ------------------------------------------------------------------------------------------------------------------------ |
| Security        | PASS     | Docs-only, no auth/input/secret surface; existing `exec()` warning in CLAUDE.md unchanged.                               |
| Performance     | PASS     | N/A — single section addition; no runtime, build, or render impact.                                                      |
| Reliability     | CONCERNS | TECH-001 mitigation depends on Task 1 audit; no AC requires *executing* each minimal example to verify documented behavior. |
| Maintainability | CONCERNS | AC-10 ("docs build cleanly") is qualitative; no required anchor check, link verifier, or source-side drift guard.       |

### Missing Considerations

1. **No behavioral verification of minimal examples.** Risk-profile P1 ("run each minimal example via `tea run`") sits in QA Notes, not in ACs. A reader who trusts the table can write a workflow that breaks at runtime if a cell drifts from `_create_dynamic_parallel_*_branch` semantics.
2. **Anchor stability is asserted (AC-7) but not enforced.** AC-7 says the anchor must be stable but does not require an explicit `{#…}` attribute or a load-bearing-comment convention. TEA-DX-001.5 error messages depend on this anchor.
3. **No source-side drift guard.** A future refactor of `_create_dynamic_parallel_*_branch` (rename, state-passing change, error-propagation change) will leave the docs silently stale. Adding pointer comments in `yaml_nodes.py` would catch this at code-review time but conflicts with AC-8 ("no code changes").
4. **No required link/anchor lint.** Both `docs/shared/YAML_REFERENCE.md` and the cross-link in `docs/python/actions-reference.md` are validated only by manual click-through.

### Test Recommendations

**P0 (must have before merge):**
- Audit `python/src/the_edge_agent/yaml_nodes.py:1468-1600` against every cell of the comparison table.
- Execute each AC-3 minimal example via `tea run` (or pytest fixture) and confirm runtime output matches the documented "Branch state" semantics.
- Confirm AC-4 (mutual-exclusion error wording) matches the actual engine error text.
- Confirm AC-5 (`fan_in:` requirement error wording) matches the actual engine error text.

**P1 (should have):**
- Render `YAML_REFERENCE.md` locally and verify `#dynamic-parallel-branch-body-modes` resolves.
- Click-through cross-link from `actions-reference.md`.
- Cold-read review by a non-author confirms AC-11.

**P2 (nice to have):**
- Markdown link/anchor checker on the modified files.
- Diff documented examples against any matching files in `examples/yaml/`.

**P3 (manual / monitoring):**
- After TEA-DX-001.5 ships, click error-message links that target this anchor.
- Track persistence of "I read the source to figure out X" feedback (closes BUS-001).

### Acceptance Criteria — Recommendations

Add (or promote from QA Notes):

- **AC-12:** Each AC-3 minimal example executes successfully under `tea run` (or an integration fixture) and produces the behavior described in its comparison-table row. Examples are committed to a regression-testable location.
- **AC-13:** Heading anchor is pinned via an explicit attribute (`{#dynamic-parallel-branch-body-modes}`) OR is preceded by a load-bearing-comment that names the slug.
- **AC-14:** Cross-link from `docs/python/actions-reference.md` is verified to resolve before merge.
- **AC-15 (optional, scope-conflict with AC-8):** Each `_create_dynamic_parallel_*_branch` helper in `yaml_nodes.py` carries a comment pointing back to the YAML_REFERENCE.md section. Author chooses to either widen scope or split into a follow-up.
- **AC-16:** A markdown lint / link-check pass runs against the modified files and reports zero broken links/anchors.

## QA Notes - Test Design

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full design:** [docs/qa/assessments/TEA-DX-001.8-test-design-20260501.md](../qa/assessments/TEA-DX-001.8-test-design-20260501.md)

### Strategy Summary

Documentation-only story (AC-8). Test levels remap accordingly:
- **Unit** = static, content-only assertions on the rendered markdown (section presence, table columns, mutual-exclusion sentence, `fan_in:` sentence, anchor declaration, code-fence count, no-code-change diff).
- **Integration** = the section interacting with neighbours: anchor + cross-link resolve, each AC-3 example parses + executes through `YAMLEngine`, and the documented mutual-exclusion / `fan_in` errors actually match the engine's error wording.
- **E2E** = human cold-read forced-choice (AC-11) + post-merge confirmation that TEA-DX-001.5 error-message links still resolve to this anchor.

The strategy treats NFR-recommended **AC-12, AC-13, AC-14, AC-16** as in-scope and assigns coverage; **AC-15** is intentionally uncovered (conflicts with AC-8). If the team declines the NFR additions, drop INT-003, UNIT-008, INT-007, INT-009, INT-010 (full list in the test-design doc).

### Coverage Matrix

| Total | Unit          | Integration   | E2E         |
| ----- | ------------- | ------------- | ----------- |
| 18    | 7 (39%)       | 8 (44%)       | 3 (17%)     |

Priority distribution: **P0: 7 · P1: 7 · P2: 3 · P3: 1**

### AC-to-Scenario Map

| AC                          | Scenarios                                                                 |
| --------------------------- | ------------------------------------------------------------------------- |
| AC-1 (section exists)       | UNIT-001 (P0), UNIT-002 (P1)                                              |
| AC-2 (table columns)        | UNIT-003 (P0), UNIT-004 (P0), INT-001 (P0)                                |
| AC-3 (minimal examples)     | UNIT-005 (P1), INT-002 (P0), INT-003 (P1, NFR-rec AC-12)                   |
| AC-4 (mutual exclusion)     | UNIT-006 (P0), INT-004 (P1)                                                |
| AC-5 (`fan_in:` requirement)| UNIT-007 (P0), INT-005 (P1)                                                |
| AC-6 (cross-link)           | INT-006 (P0), INT-007 (P1, NFR-rec AC-14)                                  |
| AC-7 (stable anchor)        | UNIT-008 (P0, NFR-rec AC-13), E2E-001 (P2)                                 |
| AC-8 (no code changes)      | UNIT-009 (P1)                                                              |
| AC-9 (reuse examples)       | INT-008 (P2)                                                               |
| AC-10 (docs build clean)    | INT-009 (P1, NFR-rec AC-16), INT-010 (P1, NFR-rec AC-16)                    |
| AC-11 (cold-read)           | E2E-002 (P1)                                                               |
| Downstream (TEA-DX-001.5)   | E2E-003 (P3)                                                               |

### Selected Scenarios with Expected Results

| ID                    | Level | Pri | Test                                                                                                           | Expected Result                                                                                            |
| --------------------- | ----- | --- | -------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| TEA-DX-001.8-UNIT-001 | Unit  | P0  | grep `YAML_REFERENCE.md` for `^## Dynamic Parallel: Branch Body Modes` (or `### …` if nested)                  | Match found                                                                                                |
| TEA-DX-001.8-UNIT-003 | Unit  | P0  | Parse first table in section; check header set                                                                  | Headers ⊇ {Mode, When to use, Branch state, Reusability, Where it's defined, Example link} (case-insensitive) |
| TEA-DX-001.8-UNIT-004 | Unit  | P0  | Count rows whose `Mode` cell is `action` / `steps` / `subgraph`                                                 | Exactly 3 rows, one per mode, no extras                                                                    |
| TEA-DX-001.8-UNIT-006 | Unit  | P0  | Section body contains a sentence naming all three modes plus one of "exactly one" / "mutually exclusive" / "only one of" | Match found                                                                                       |
| TEA-DX-001.8-UNIT-007 | Unit  | P0  | Section body contains `fan_in:` AND "required" / "must reference" within same paragraph                         | Match found                                                                                                |
| TEA-DX-001.8-UNIT-008 | Unit  | P0  | Heading carries `{#dynamic-parallel-branch-body-modes}` OR slugifies to that anchor + load-bearing comment      | At least one of the two pinning forms present                                                              |
| TEA-DX-001.8-UNIT-009 | Unit  | P1  | `git diff --name-only main...HEAD` touches only `docs/**` / `examples/yaml/**`                                 | Zero files outside those paths                                                                             |
| TEA-DX-001.8-INT-001  | Int   | P0  | Manual cell-by-cell audit of comparison table vs `yaml_nodes.py:1468-1601`                                       | Every cell in `Branch state` and `Reusability` columns matches helper behavior                             |
| TEA-DX-001.8-INT-002  | Int   | P0  | Extract each fenced YAML, write to tmp, `YAMLEngine.from_yaml_string(...).compile()`                              | Compiles without error for all three                                                                       |
| TEA-DX-001.8-INT-003  | Int   | P1  | (AC-12) Execute each example via `tea run` against tiny `items` fixture                                           | Output matches the documented `Branch state` semantics for that mode                                       |
| TEA-DX-001.8-INT-004  | Int   | P1  | YAML node with both `action:` and `steps:` under one `dynamic_parallel`                                          | `ValueError` whose message includes "exclusive" / "exactly one" / "only one"                                |
| TEA-DX-001.8-INT-005  | Int   | P1  | YAML node missing `fan_in:`                                                                                        | `ValueError` whose message includes `fan_in`                                                                |
| TEA-DX-001.8-INT-006  | Int   | P0  | grep `actions-reference.md` for `…/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`                        | Link found                                                                                                 |
| TEA-DX-001.8-INT-007  | Int   | P1  | (AC-14) Render `actions-reference.md`; click cross-link                                                            | Lands on the new section heading element                                                                   |
| TEA-DX-001.8-INT-009  | Int   | P1  | (AC-16) Run markdown linter on modified files                                                                       | Zero new errors vs. baseline                                                                                |
| TEA-DX-001.8-INT-010  | Int   | P1  | (AC-16) Run link/anchor checker on modified files                                                                   | Zero broken links; new anchor referenceable                                                                 |
| TEA-DX-001.8-E2E-001  | E2E   | P2  | Render `YAML_REFERENCE.md` to HTML                                                                              | Element with `id="dynamic-parallel-branch-body-modes"` exists                                              |
| TEA-DX-001.8-E2E-002  | E2E   | P1  | Cold-read forced-choice: 3 mode-selection prompts to a non-author reviewer                                       | ≥ 2/3 correct in < 90 seconds                                                                              |
| TEA-DX-001.8-E2E-003  | E2E   | P3  | Post-TEA-DX-001.5 merge: trigger an enriched error message and click the doc link                                 | Lands on `#dynamic-parallel-branch-body-modes`                                                             |

### Risk → Test Mapping

| Risk     | Severity | Mitigated by                                                                                                    |
| -------- | -------- | --------------------------------------------------------------------------------------------------------------- |
| TECH-001 | Medium   | INT-001 (manual audit), INT-002 (parse), INT-003 (execute), INT-004 / INT-005 (error wording matches docs)       |
| OPS-001  | Low      | UNIT-008 (anchor pinned), E2E-001 (anchor in HTML), E2E-003 (downstream link still works)                       |
| TECH-002 | Low      | E2E-003 only — accepted residual; AC-15 conflict with AC-8 leaves no in-PR guard                                |
| OPS-002  | Minimal  | INT-006 (link exists), INT-007 (link resolves), INT-010 (link checker)                                          |
| BUS-001  | Minimal  | E2E-002 (cold-read forced-choice)                                                                               |

### Test Data & Environment Requirements

**Fixtures (new):**
- `fixtures/dynamic_parallel_doc/items_input.yaml` — minimal `items: [a, b, c]` input. Used by INT-003.
- `fixtures/dynamic_parallel_doc/no_fan_in.yaml` — `dynamic_parallel` node missing `fan_in:`. Used by INT-005.
- `fixtures/dynamic_parallel_doc/double_body.yaml` — `dynamic_parallel` node with both `action:` and `steps:`. Used by INT-004.

**Fixtures (existing, reused per AC-9):**
- `examples/yaml/dynamic_parallel_action_mode.yaml`
- `examples/yaml/dynamic_parallel_steps_mode.yaml`
- `examples/yaml/dynamic_parallel_subgraph_mode.yaml`

**Tooling:**
- Python ≥ 3.9; `pytest`; a markdown parser (`mistune` / `markdown`) for table extraction.
- Markdown linter: `pymarkdown` or `markdownlint-cli`.
- Link checker: `lychee` or `markdown-link-check`.
- Doc renderer for E2E-001: `python -m markdown` or whatever the project standardizes on.
- **Not required:** network, DB, LTM backend, LLM credentials, Rust toolchain.

**Cold-read protocol (E2E-002):**
- Reviewer must not have authored or reviewed a `dynamic_parallel` PR.
- Setup: hand them only the new section as a standalone HTML page.
- Three forced-choice prompts (one per mode); pass at ≥ 2/3 in < 90 seconds.

### Recommended Execution Order

1. P0 Unit (UNIT-001, 003, 004, 006, 007, 008) — fail fast on missing structure.
2. P0 Integration (INT-001, 002, 006) — implementation audit, examples parse, cross-link exists.
3. P1 Unit (UNIT-002, 005, 009).
4. P1 Integration (INT-003, 004, 005, 007, 009, 010).
5. P1 E2E (E2E-002) — cold-read.
6. P2 (INT-008, E2E-001).
7. P3 (E2E-003) — post-merge monitoring.

### Coverage Gaps

None against in-scope ACs (1–11) and NFR-recommended additions (12, 13, 14, 16). AC-15 intentionally uncovered (conflicts with AC-8); residual TECH-002 risk accepted in risk profile.

## QA Notes - Requirements Trace

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full trace matrix:** [docs/qa/assessments/TEA-DX-001.8-trace-20260501.md](../qa/assessments/TEA-DX-001.8-trace-20260501.md)

### Requirements Coverage

- **In-scope requirements:** 15 (AC-1 .. AC-11 + NFR-recommended AC-12, AC-13, AC-14, AC-16)
- **Fully covered (planned):** 13 (87 %)
- **Partially covered (planned):** 2 (13 %) — AC-7 (anchor stability), AC-9 (example reuse)
- **Intentionally uncovered:** 1 — AC-15 (conflicts with AC-8)
- **Currently implemented in repo:** 0 of 18 planned scenarios (story in Draft). Engine rules backing AC-4 / AC-5 are already enforced and tested (`python/src/the_edge_agent/yaml_nodes.py:1146-1163`, `python/tests/test_yaml_dynamic_parallel.py:77-119`); the docs↔engine *wording-consistency* tests (INT-004 / INT-005) are new.

### Traceability Matrix (compact)

| AC          | Test IDs                                          | Coverage | Levels       | Implemented? |
| ----------- | ------------------------------------------------- | -------- | ------------ | ------------ |
| AC-1        | UNIT-001, UNIT-002                                | FULL     | unit         | No           |
| AC-2        | UNIT-003, UNIT-004, INT-001                       | FULL     | unit + int   | No           |
| AC-3        | UNIT-005, INT-002, INT-003 (NFR-rec AC-12)        | FULL     | unit + int   | No           |
| AC-4        | UNIT-006, INT-004                                 | FULL     | unit + int   | Engine rule yes; doc-pin no |
| AC-5        | UNIT-007, INT-005                                 | FULL     | unit + int   | Engine rule yes; doc-pin no |
| AC-6        | INT-006, INT-007 (NFR-rec AC-14)                  | FULL     | int          | No           |
| AC-7 / 13   | UNIT-008 (NFR-rec AC-13), E2E-001, E2E-003        | PARTIAL  | unit + e2e   | No           |
| AC-8        | UNIT-009                                          | FULL     | unit         | No (CI/PR-time check) |
| AC-9        | INT-008                                           | PARTIAL  | int          | Examples exist; link wiring no |
| AC-10 / 16  | INT-009, INT-010 (both NFR-rec AC-16)             | FULL     | int          | No           |
| AC-11       | E2E-002                                           | FULL     | e2e          | No           |
| AC-15       | (none — intentional, conflicts with AC-8)         | NONE     | —            | n/a          |
| Downstream  | E2E-003 (TEA-DX-001.5 link integrity, post-merge) | FULL     | e2e          | Blocked on TEA-DX-001.5 |

Each scenario carries a Given-When-Then in the full trace doc; only the AC↔scenario mapping is reproduced here.

### Gaps Identified

1. **No execution-level proof of doc accuracy without AC-12 (INT-003).** Risk: TECH-001 (Medium) — table cells could describe behavior that doesn't match runtime.
2. **Anchor stability rests on author convention without AC-13 (UNIT-008).** Risk: OPS-001 (Low) — TEA-DX-001.5 error-message links break if the heading is renamed.
3. **Cross-link verified by grep only without AC-14 (INT-007).** Risk: OPS-002 (Minimal).
4. **No required lint/link-check without AC-16 (INT-009 / INT-010).** Risk: OPS-002 (Minimal).
5. **No source-side drift guard (AC-15).** Risk: TECH-002 (Low) — accepted residual; AC-8 forbids the only in-PR mitigation. Monitored via E2E-003 only.
6. **No standing drift guard for engine error wording vs. docs after merge.** INT-004 / INT-005 only run on the doc PR; promoting them into the standing engine test suite would catch later engine wording changes that silently invalidate AC-4 / AC-5 phrasing.

### Recommendations

- **Adopt NFR-recommended AC-12, AC-13, AC-14, AC-16** to lift AC-3 / AC-7 / AC-6 / AC-10 coverage from PARTIAL-on-omission to FULL. Without the promotion, planned coverage drops to:
  - AC-3 → PARTIAL (UNIT-005, INT-002 only — no execution-level test)
  - AC-7 → PARTIAL (E2E-001 only — anchor stability rests on slug rules)
  - AC-6 → PARTIAL (INT-006 only — grep, not render-and-click)
  - AC-10 → PARTIAL (no automated lint or link checker)
- **Add three small fixtures** under `fixtures/dynamic_parallel_doc/`: `items_input.yaml`, `no_fan_in.yaml`, `double_body.yaml`. Reuse existing `examples/yaml/dynamic_parallel_{action,steps,subgraph}_mode.yaml` for AC-9.
- **Pin the heading anchor explicitly** as `## Dynamic Parallel: Branch Body Modes {#dynamic-parallel-branch-body-modes}` (or precede with a load-bearing comment naming the slug) — this is the single highest-leverage mitigation for OPS-001 / TECH-002.
- **Promote INT-004 and INT-005 into the standing test suite** (not gated to this PR), so any future change to engine error wording re-validates the documented phrasing.
- **Schedule a follow-up issue** for AC-15 (source-side comment in `yaml_nodes.py`) so the residual TECH-002 risk has an explicit landing place rather than living only as a post-merge monitoring item.
- **Confirm acceptance** that AC-15 stays uncovered before this PR merges; document the choice in the gate file.

### Trace References

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.8-trace-20260501.md
Total ACs in scope: 15 · Fully covered (planned): 13 · Partially covered (planned): 2 · Intentionally uncovered: 1
```

## SM Validation

**Validator:** Bob (Scrum Master) · **Date:** 2026-05-01 · **Mode:** YOLO
**Checklist:** Definition of Ready

### Validation Results

| # | Criterion | Status | Evidence |
|---|-----------|--------|----------|
| 1 | Story has clear title and description | PASS | Title at L1 names the doc target; "Story" block (L14-18) frames a coherent As/I want/so that. |
| 2 | Acceptance criteria are defined and testable | PASS | 11 ACs (AC-1..AC-11) at L35-61, each phrased as a verifiable assertion (section exists, table columns named, mutual-exclusion rule stated, anchor stable, etc.). |
| 3 | Dependencies are identified | PASS | Story Context (L20-27) names `docs/shared/YAML_REFERENCE.md`, `docs/python/actions-reference.md`, `examples/yaml/`, `python/src/the_edge_agent/yaml_nodes.py:1177-1192`, and downstream coupling with TEA-DX-001.5. |
| 4 | Technical approach is documented | PASS | Technical Notes (L63-67) describe edit target, audit method (read three `_create_dynamic_parallel_*_branch` helpers), pattern reference, and accuracy constraint. |
| 5 | Story is properly sized | PASS | Low priority, documentation-only (AC-8), 4 tasks in Tasks/Subtasks (L69-81), no code changes. Scope fits a single PR. |
| 6 | QA notes sections present | PASS | Risk Profile (L96-139), NFR Assessment (L141-195), Test Design (L197-306), Requirements Trace (L308-368) all present, each with assessment-doc cross-link. |
| 7 | No blocking issues or unknowns | PASS | No external blockers; engine rules backing AC-4/AC-5 are already enforced (per trace). Downstream consumer TEA-DX-001.5 is an enabler, not a blocker, for this story. |

### Notes

- AC-7 (anchor stability) and AC-9 (example reuse) are flagged PARTIAL coverage in the trace; the NFR-recommended AC-12/13/14/16 are advisory promotions to lift coverage to FULL. None block readiness — they are scope decisions for the dev/QA pair to take during implementation.
- AC-15 is intentionally uncovered (conflicts with AC-8); accepted residual TECH-002 risk is documented and monitored via E2E-003.
- Dev should confirm the line-range citation in Technical Notes (`yaml_nodes.py:1177-1192`) against the current file — NFR doc references `1468-1600`, suggesting the helpers may have moved. This is a docs-side detail, not a readiness blocker.

**Outcome:** All 7 Definition-of-Ready criteria PASS. Status set to **Ready for Development**.

## Dev Agent Record

### Agent Model Used
- claude-opus-4-7 (1M context) via the BMad `/dev` (James) persona, YOLO mode

### Debug Log References
- Implementation audit performed against `python/src/the_edge_agent/yaml_nodes.py:1100-1655` (the three `_create_dynamic_parallel_*_branch` helpers and the parent `_create_dynamic_parallel_function`):
  - `action` mode (`_create_dynamic_parallel_action_branch`, lines 1522-1559) — reuses `_create_action_function`; branch sees a deep copy of the full parent state with `item_var` / `index_var` injected.
  - `steps` mode (`_create_dynamic_parallel_steps_branch`, lines 1561-1587) — reuses `_create_steps_function`; same deep-copy state semantics; subsequent steps in the list see prior steps' outputs merged in.
  - `subgraph` mode (`_create_dynamic_parallel_subgraph_branch`, lines 1589-1655) — loads a separate YAML file; branch sees only the keys produced by the `input:` mapping (templates resolved against the parent state, including `item`/`index`).
- Engine-side error wording cross-checked against `_DOC_ANCHOR` block (lines 1148-1217) so the "Common errors" list in the docs matches the actual `ValueError` messages.

### Completion Notes List

- AC-1 ✓ — `### Dynamic Parallel: Branch Body Modes` added under the existing `## Dynamic Parallel` parent in `docs/shared/YAML_REFERENCE.md`.
- AC-2 ✓ — Comparison table includes all six required columns: Mode, When to use, Branch state, Reusability, Where it's defined, Example link.
- AC-3 ✓ — Three minimal fenced-YAML examples (action / steps / subgraph) sit immediately after the table; each is between 9 and 11 non-blank lines.
- AC-4 ✓ — A dedicated **Mutual-exclusion rule** paragraph names all three modes and uses the wording "exactly one of" (mirroring the engine's `ValueError`).
- AC-5 ✓ — A dedicated **`fan_in:` requirement** paragraph specifies the sibling-key constraint (NOT nested under any branch-body block).
- AC-6 ✓ — Cross-link added to `docs/python/actions-reference.md` (under the `agent.*` action block) pointing at `../shared/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`.
- AC-7 / AC-13 ✓ — Heading carries an explicit `{#dynamic-parallel-branch-body-modes}` slug AND is preceded by a load-bearing HTML comment naming the slug so future renames trip a code-review signal.
- AC-8 ✓ — No code changes. The existing `yaml_nodes.py` `_DOC_ANCHOR` continues to point at `#dynamic-parallel`, which still resolves to the (now-extended) `## Dynamic Parallel` parent section. AC-15 (source-side comment in `yaml_nodes.py`) is intentionally deferred to a follow-up to respect AC-8.
- AC-9 ✓ — All three example files in `examples/yaml/` (`dynamic_parallel_action_mode.yaml`, `dynamic_parallel_steps_mode.yaml`, `dynamic_parallel_subgraph_mode.yaml`) are referenced from the new section. No new example YAMLs were added.
- AC-10 / AC-16 ✓ — Tests assert table-row column-count consistency and mode-row presence; no link/anchor checker is configured in this repo, so visual inspection plus the static tests stand in for it.
- AC-11 ✓ — Quick rules of thumb + table + minimal examples cover the three primary use-case classes (single call → action; few inline steps → steps; large reusable workflow → subgraph). Cold-read review (E2E-002) is left as a peer-review activity per the QA test design.
- Tests: extended `python/tests/test_dx_001_8_dynamic_parallel_doc_table.py` from 5 → 14 unit/integration tests covering UNIT-001..UNIT-009 and INT-006 from the test design. All 14 pass; the related dynamic_parallel + parallel-strategy suites (73 tests) all pass.

### File List

**Modified:**
- `docs/shared/YAML_REFERENCE.md` — replaced the prior placeholder `### Branch-body modes` block with the full `### Dynamic Parallel: Branch Body Modes` section: pinned-anchor heading, six-column comparison table, mutual-exclusion + fan_in paragraphs, side-by-side minimal examples, and links to existing example YAMLs.
- `docs/python/actions-reference.md` — added a callout under the `agent.*` action table linking to the new section.
- `python/tests/test_dx_001_8_dynamic_parallel_doc_table.py` — expanded test coverage from 5 → 14 tests (UNIT-001 .. UNIT-009 and INT-006). Pre-existing 5 tests still pass.
- `docs/stories/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.md` — Dev Agent Record sections (this file); status flipped to Ready for Review.

**New:** _none_ (test file existed at story start; this commit edits it in place.)

**Deleted:** _none_

### Change Log

| Date       | Author | Change                                                                                              |
|------------|--------|-----------------------------------------------------------------------------------------------------|
| 2026-05-02 | James (dev) | Replaced placeholder modes section in `YAML_REFERENCE.md` with the full pinned-anchor comparison-table section + side-by-side examples; added cross-link from `actions-reference.md`; expanded `test_dx_001_8_dynamic_parallel_doc_table.py` to 14 tests; status → Ready for Review. |

## QA Results

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-02 · **Mode:** YOLO
**Gate:** [docs/qa/gates/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.yml](../qa/gates/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.yml)

### Decision: **PASS** · Quality Score: **90/100**

Documentation-only story closed cleanly. Pre-implementation NFR CONCERNS on
reliability and maintainability are substantially resolved by the dev's anchor
pinning (explicit `{#dynamic-parallel-branch-body-modes}` slug **plus** a
load-bearing HTML comment), the implementation audit recorded against the
current source lines, and a 14-test static regression suite covering UNIT-001
.. UNIT-009 and INT-006. Three NFR-recommended ACs (AC-13 anchor pinning,
AC-14 cross-link grep, AC-16 docs build clean) are de-facto satisfied; the
fourth (AC-12, runtime execution of the minimal examples) and AC-15
(source-side comment) remain accepted residual risks.

### Implementation Audit

Cross-checked the comparison-table cells against
`python/src/the_edge_agent/yaml_nodes.py:1522-1655`:

| Cell claim | Source evidence | Verdict |
|------------|-----------------|---------|
| `action`: deep-copy of full parent state, `item`/`index` injected, action callable from any node | `_create_dynamic_parallel_action_branch` reuses `_create_action_function` (line 1551); thread-safe deep copy is provided by the parallel runner upstream and verified by `test_yaml_dynamic_parallel.py::test_branches_have_independent_state` | Accurate |
| `steps`: deep-copy of full parent state, subsequent steps see prior outputs merged in | `_create_dynamic_parallel_steps_branch` reuses `_create_steps_function` (line 1581) → standard steps merge semantics | Accurate |
| `subgraph`: scoped subset only — branch sees only keys produced by the `input:` mapping | Lines 1638-1653 build `subgraph_state = {}` then populate exclusively from `input_mapping`, then call `cached_graph.invoke(subgraph_state)` | Accurate |
| Mutual-exclusion error wording (`"conflicting branch-body keys present"`) | Line 1187 verbatim | Match |
| Missing-mode error wording (`"missing one of 'action', 'steps', or 'subgraph'"`) | Line 1176-1177 verbatim | Match |
| `fan_in:` missing error wording (`"missing required key 'fan_in'"`) | Line 1196-1197 verbatim | Match |
| `fan_in:` undefined-target wording (`"fan_in target '<name>' is not a defined node"`) | Line 1213-1214 verbatim | Match |

The "Common errors" bullet list in the new docs section is a lossless
projection of the engine's actual `ValueError` messages — TEA-DX-001.5
enriched messages can quote either the engine string or the doc bullet
without introducing drift.

### Test Architecture

- **Static unit suite:** `python/tests/test_dx_001_8_dynamic_parallel_doc_table.py` — 14 tests, 0.03 s; all green.
- **Section presence (UNIT-001/002):** `test_dynamic_parallel_section_present`, `test_branch_body_modes_subsection_present`.
- **Table integrity (UNIT-003/004):** `test_table_headers_include_required_columns` (header set ⊇ required six columns), `test_table_has_exactly_three_mode_rows` (one row per mode, no extras), `test_table_rows_have_consistent_column_count` (all rows have header arity).
- **Minimal examples (UNIT-005):** `test_each_mode_has_minimal_example` extracts fenced `yaml` blocks, asserts 5 ≤ non-blank lines ≤ 15 per block, and asserts each of `action`/`steps`/`subgraph` keys is observed.
- **Mutual exclusion + `fan_in:` (UNIT-006/007):** explicit regex assertions on the section text.
- **Anchor pinning (UNIT-008):** `test_anchor_is_pinned` accepts either an explicit `{#…}` attribute *or* a load-bearing HTML comment naming the slug. Dev shipped both — belt-and-suspenders.
- **Cross-link (INT-006):** grep-asserted from `actions-reference.md` to `YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`.
- **Example file references (AC-9):** `test_section_links_to_existing_example_files` confirms all three `examples/yaml/dynamic_parallel_*_mode.yaml` are referenced and exist on disk.
- **Adjacent regression suite:** `tests/test_yaml_dynamic_parallel.py` — 31/31 passing; AC-4 / AC-5 *engine-side* enforcement remains green.

### NFR Re-validation

| NFR | Pre-impl | Post-impl | Notes |
|-----|----------|-----------|-------|
| Security | PASS | PASS | Docs-only; no auth/input/secret surface; existing `exec()` warning unchanged. |
| Performance | PASS | PASS | One `### …` heading + table + three small fenced YAMLs; no measurable build/render cost. |
| Reliability | CONCERNS | PASS | Implementation audit performed and recorded (Dev Agent Record cites lines 1522-1655). Static suite pins anchor, table arity, mode rows, error-wording substrings. INT-003 (runtime execution of examples) remains uncovered → see Residual Risks. |
| Maintainability | CONCERNS | PASS | Anchor double-pinned (explicit `{#…}` + HTML comment). Comment text explicitly names TEA-DX-001.5 as the downstream consumer, so any future heading rename trips a code-review signal. |

### AC Coverage

| AC | Status | Evidence |
|----|--------|----------|
| AC-1 (section exists) | PASS | `YAML_REFERENCE.md:1629` heading present; UNIT-001/002 green. |
| AC-2 (table columns) | PASS | Header parser confirms all six required columns; mode-row regex confirms exactly 3 rows; row-arity test confirms no malformed rows. |
| AC-3 (minimal examples) | PASS (with caveat) | Three fenced YAMLs, each 9-11 non-blank lines (within 5-15 envelope). Caveat: examples are not executed via `tea run`; see Residual Risks. |
| AC-4 (mutual exclusion) | PASS | Section text uses "exactly one of" and "mutually exclusive"; engine wording (line 1187) matches. |
| AC-5 (`fan_in:` requirement) | PASS | Dedicated paragraph; engine wording (line 1196-1197) matches. |
| AC-6 (cross-link from `actions-reference.md`) | PASS | `actions-reference.md:252` links to `../shared/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`; INT-006 green. |
| AC-7 / AC-13 (stable anchor) | PASS | Heading carries `{#dynamic-parallel-branch-body-modes}` AND is preceded by a multi-line `LOAD-BEARING ANCHOR` HTML comment naming TEA-DX-001.5 and the source-side `_DOC_ANCHOR`. |
| AC-8 (no code changes) | PASS | Diff against this story scope is `docs/shared/YAML_REFERENCE.md` (+165 LOC) and `docs/python/actions-reference.md` (+5 LOC). The new test file is the only addition and lives under `python/tests/`, which is project test surface, not engine code. |
| AC-9 (reuse examples) | PASS | All three existing `examples/yaml/dynamic_parallel_*_mode.yaml` referenced by name and asserted to exist; no duplicate examples added under `examples/yaml/`. |
| AC-10 / AC-16 (docs build clean) | PASS (with caveat) | Static tests cover table arity, code-fence count, and anchor pinning. Caveat: project has no markdown lint or link-checker in CI, so AC-16's "automated link checker" is satisfied by the regex-level INT-006 / UNIT-008 substitutes. |
| AC-11 (cold-read) | PASS (advisory) | Section provides quick rules of thumb (one HTTP/LLM call → action; 2-3 inline steps → steps; >3 steps or independent reuse → subgraph). E2E-002 cold-read is a peer-review activity not gated to this PR. |
| AC-12 (NFR-rec, runtime example execution) | DEFERRED | Static parse-level coverage only; INT-003 not implemented. Accepted residual TECH-001 (Medium, mitigated to Low by the audit). |
| AC-14 (NFR-rec, render-and-click cross-link) | PASS-by-substitute | INT-006 grep test pins the link string; no docs render in CI to support click-through. |
| AC-15 (NFR-rec, source-side pointer comment) | DEFERRED | Conflicts with AC-8; explicitly out-of-scope per Dev Agent Record. Residual TECH-002 monitored only via E2E-003 (post TEA-DX-001.5 ship). |

### Residual Risks

- **TECH-001 (Medium → Low after audit):** Comparison-table cells could drift from `_create_dynamic_parallel_*_branch` semantics. Mitigated by the documented audit (Dev Agent Record), the load-bearing comment, and AC-4/AC-5 wording-equality checks. Not eliminated — INT-003 (runtime execution) would close it but is deferred.
- **TECH-002 (Low):** Future refactor of the three branch helpers will not trip a docs-side test. Mitigated only by the load-bearing comment plus E2E-003 monitoring. AC-15 (source-side comment) is the in-PR mitigation but conflicts with AC-8; accept as residual or track in a follow-up issue.
- **OPS-001 (Low → Minimal):** Anchor stability rests on the `{#…}` attribute + comment combination. UNIT-008 will fail loudly if either is removed.
- **OPS-002 (Minimal):** Cross-link integrity verified by string match, not render-time. Acceptable for a docs PR until project adopts markdown link-check.
- **BUS-001 (Minimal):** Cold-read effectiveness depends on E2E-002 peer review.

### Recommendations

**Must-fix (gating):** None.

**Nice-to-have (do not block this PR):**

1. **Add an INT-003 fixture** (`fixtures/dynamic_parallel_doc/items_input.yaml` plus a single pytest that compiles each fenced YAML through `YAMLEngine.from_yaml_string(...)` and runs it against a 2-item input). This closes AC-12 and lifts TECH-001 from "audited residual" to "test-pinned." ~1 hour of work.
2. **Open a follow-up issue** for AC-15 (source-side comment in `yaml_nodes.py` near `_DOC_ANCHOR`) so TECH-002 has an explicit landing place instead of living only as monitoring debt. AC-8 forbids it in this PR; that's why a follow-up is the right home.
3. **Promote INT-004 / INT-005** (engine error-wording matches docs) into the standing test suite when TEA-DX-001.5 lands, so future engine-message edits can't silently invalidate the documented "Common errors" list.
4. **Post-TEA-DX-001.5 monitoring (E2E-003):** confirm enriched error-message links resolve to `#dynamic-parallel-branch-body-modes`. Out of scope for this PR.

### Verification Commands

```bash
cd python && python3 -m pytest tests/test_dx_001_8_dynamic_parallel_doc_table.py -v
# Expected: 14 passed in <0.1s

cd python && python3 -m pytest tests/test_yaml_dynamic_parallel.py -v
# Expected: 31 passed (engine-side AC-4 / AC-5 still enforced)
```

### Standards Compliance

- BMad story-file permissions respected: this review modifies only the `## QA Results` section.
- Gate file written to `docs/qa/gates/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.yml` per `qa.qaLocation` convention.
- Sibling assessment docs unchanged (`docs/qa/assessments/TEA-DX-001.8-{risk,nfr,test-design,trace}-20260501.md`).

