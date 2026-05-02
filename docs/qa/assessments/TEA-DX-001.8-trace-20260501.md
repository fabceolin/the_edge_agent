# Requirements Traceability Matrix

## Story: TEA-DX-001.8 — Document `action` vs `steps` vs `subgraph` in `dynamic_parallel`

**Reviewer:** Quinn (Test Architect)
**Date:** 2026-05-01
**Mode:** YOLO
**Story status at trace time:** Draft (implementation not yet started)
**Related artifacts:**
- Risk profile: [TEA-DX-001.8-risk-20260501.md](./TEA-DX-001.8-risk-20260501.md)
- NFR assessment: [TEA-DX-001.8-nfr-20260501.md](./TEA-DX-001.8-nfr-20260501.md)
- Test design: [TEA-DX-001.8-test-design-20260501.md](./TEA-DX-001.8-test-design-20260501.md)

---

## Coverage Summary

- **Total in-scope requirements:** 15 (AC-1 .. AC-11 plus NFR-recommended AC-12, AC-13, AC-14, AC-16; AC-15 explicitly out-of-scope by AC-8)
- **Total planned scenarios:** 18 (UNIT × 7, INTEGRATION × 8, E2E × 3)
- **Fully covered (planned):** 13 (87 %)
- **Partially covered (planned):** 2 (13 %) — AC-7 (anchor stability), AC-9 (example reuse)
- **Not covered:** 1 (AC-15, intentional — conflicts with AC-8)
- **Currently implemented in repo at trace time:** 0 of 18 — story is in Draft, no PR yet, no AC-1 section in `docs/shared/YAML_REFERENCE.md`. Engine-side rules referenced by INT-004 / INT-005 are *partially* exercised by existing tests in `python/tests/test_yaml_dynamic_parallel.py` (`test_multiple_execution_modes_raises_error`, `test_missing_fan_in_raises_error`) but those assert engine behavior, not docs-↔-engine consistency.

Every planned scenario carries a Given-When-Then mapping below. "Implementation status" reflects whether code/text/test artifact currently exists in the repo as of 2026-05-01.

---

## Requirement Mappings

### AC-1 — New section `## Dynamic Parallel: Branch Body Modes` exists in `docs/shared/YAML_REFERENCE.md`

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED** (`grep -n "Dynamic Parallel: Branch Body Modes" docs/shared/YAML_REFERENCE.md` returns no match)

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-001 (P0)** — `grep` for `^## Dynamic Parallel: Branch Body Modes` (or `^### …` if nested) in `YAML_REFERENCE.md`
  - **Given:** the docs PR has been merged
  - **When:** ripgrep is run against `docs/shared/YAML_REFERENCE.md`
  - **Then:** the heading line is found exactly once
- **Unit · TEA-DX-001.8-UNIT-002 (P1)** — placement check: section appears after the existing `dynamic_parallel` overview row (line ~762)
  - **Given:** the rendered file
  - **When:** the line offset of the new section is compared to the existing `dynamic_parallel` reference row
  - **Then:** the new section is below it, not orphaned at the top of file

### AC-2 — Comparison table includes the six required columns

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED**

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-003 (P0)** — parse the section's first markdown table; assert headers ⊇ {Mode, When to use, Branch state, Reusability, Where it's defined, Example link}
  - **Given:** the rendered section
  - **When:** the first markdown table's header row is parsed (case-insensitive)
  - **Then:** all six required columns are present (extra columns allowed)
- **Unit · TEA-DX-001.8-UNIT-004 (P0)** — table body has exactly three rows for `action`, `steps`, `subgraph`
  - **Given:** the parsed table
  - **When:** rows are filtered by `Mode` cell
  - **Then:** exactly one row each for `action`, `steps`, `subgraph` — no duplicates, no extras
- **Integration · TEA-DX-001.8-INT-001 (P0)** — manual cell-by-cell audit against `python/src/the_edge_agent/yaml_nodes.py:1468–1601` (the three `_create_dynamic_parallel_*_branch` helpers)
  - **Given:** the comparison table and the three helpers
  - **When:** each `Branch state` and `Reusability` cell is read alongside the helper body
  - **Then:** the cell text matches the helper's actual state-passing and reuse semantics — primary mitigation for **TECH-001 (Medium)**

### AC-3 — 5–10 line minimal example for each mode

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED**

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-005 (P1)** — exactly three fenced ```yaml blocks of length 5–10 lines (excluding fences)
  - **Given:** the rendered section
  - **When:** fenced YAML blocks are extracted and counted
  - **Then:** exactly three blocks, each 5–10 lines long
- **Integration · TEA-DX-001.8-INT-002 (P0)** — extract each fenced YAML, write to tmp, `YAMLEngine.from_yaml_string(...).compile()`
  - **Given:** the three extracted snippets
  - **When:** each is loaded and compiled by the engine
  - **Then:** all three compile without error
- **Integration · TEA-DX-001.8-INT-003 (P1, NFR-rec AC-12)** — execute each example via `tea run` against an `items: [a,b,c]` fixture
  - **Given:** a tiny `items` fixture (`fixtures/dynamic_parallel_doc/items_input.yaml`)
  - **When:** each example is executed end-to-end
  - **Then:** runtime output matches the documented `Branch state` semantics for that mode (closes Risk-Profile P1 and NFR Reliability)

### AC-4 — Section explicitly states mutual-exclusion of `action` / `steps` / `subgraph`

**Coverage: FULL (planned)** · **Implementation status: PARTIAL** — engine rule already enforced and tested at `python/src/the_edge_agent/yaml_nodes.py:1146–1158` and `python/tests/test_yaml_dynamic_parallel.py::test_multiple_execution_modes_raises_error`; the *docs↔engine consistency* test (INT-004) does not yet exist.

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-006 (P0)** — wording check: section names all three modes with one of "exactly one" / "mutually exclusive" / "only one of"
  - **Given:** the section body
  - **When:** the body is regex-matched
  - **Then:** at least one sentence names `action`, `steps`, AND `subgraph` plus the exclusion phrase
- **Integration · TEA-DX-001.8-INT-004 (P1)** — load a node specifying both `action:` and `steps:`; assert engine raises `ValueError` whose message matches the docs phrasing
  - **Given:** `fixtures/dynamic_parallel_doc/double_body.yaml`
  - **When:** `YAMLEngine.from_yaml_string(...).compile()` is called
  - **Then:** a `ValueError` is raised whose text contains "exclusive" / "exactly one" / "only one" — pinning the doc rule against actual engine wording

### AC-5 — Section explicitly notes the `fan_in:` requirement

**Coverage: FULL (planned)** · **Implementation status: PARTIAL** — engine rule already enforced at `yaml_nodes.py:1160–1163` and tested at `test_yaml_dynamic_parallel.py::test_missing_fan_in_raises_error` (matches `requires 'fan_in' target node`); the docs-↔-engine pinning test (INT-005) does not yet exist.

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-007 (P0)** — section body contains the literal `fan_in:` plus "required" / "must reference" within the same paragraph
  - **Given:** the section body
  - **When:** the body is regex-matched per paragraph
  - **Then:** at least one paragraph satisfies both predicates
- **Integration · TEA-DX-001.8-INT-005 (P1)** — load a `dynamic_parallel` node missing `fan_in:`; assert engine raises an error mentioning `fan_in`
  - **Given:** `fixtures/dynamic_parallel_doc/no_fan_in.yaml`
  - **When:** `YAMLEngine.from_yaml_string(...).compile()` is called
  - **Then:** a `ValueError` is raised whose text contains `fan_in`

### AC-6 — Cross-link from `docs/python/actions-reference.md`

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED**

Given-When-Then mappings:

- **Integration · TEA-DX-001.8-INT-006 (P0)** — grep `actions-reference.md` for `…/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`
  - **Given:** the merged `actions-reference.md`
  - **When:** ripgrep is run with the relative-link pattern
  - **Then:** at least one occurrence of the link is found
- **Integration · TEA-DX-001.8-INT-007 (P1, NFR-rec AC-14)** — render `actions-reference.md`, click the cross-link
  - **Given:** the rendered docs
  - **When:** the link in `actions-reference.md` is followed
  - **Then:** the browser lands on the new section's heading element

### AC-7 — Anchor URL is stable

**Coverage: PARTIAL (planned)** · **Implementation status: NOT STARTED**

Caveat: AC-7 asserts stability but does not by itself require an explicit `{#…}` attribute or comment-pin. Promotion to AC-13 (NFR-rec) makes the test deterministic. UNIT-008 covers the pinning either way; E2E-001 covers HTML-render correctness; downstream coupling (E2E-003) is post-merge-only.

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-008 (P0, NFR-rec AC-13)** — heading carries `{#dynamic-parallel-branch-body-modes}` OR slugifies to that value with a load-bearing comment
  - **Given:** the heading line and its preceding comment (if any)
  - **When:** the heading is parsed
  - **Then:** the anchor `dynamic-parallel-branch-body-modes` is either explicitly attached or deterministically derivable
- **E2E · TEA-DX-001.8-E2E-001 (P2)** — render `YAML_REFERENCE.md` to HTML; assert `id="dynamic-parallel-branch-body-modes"` exists
  - **Given:** the rendered HTML
  - **When:** the DOM is queried for the anchor id
  - **Then:** an element with that id exists on the new section
- **E2E · TEA-DX-001.8-E2E-003 (P3, post-merge monitoring)** — after TEA-DX-001.5 ships, click an enriched error message link
  - **Given:** TEA-DX-001.5 has been merged
  - **When:** an enriched `dynamic_parallel` error is triggered and the embedded link is clicked
  - **Then:** it resolves to `#dynamic-parallel-branch-body-modes`

### AC-8 — No code changes

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED**

Given-When-Then mappings:

- **Unit · TEA-DX-001.8-UNIT-009 (P1)** — `git diff --name-only main...HEAD` touches only `docs/**` (and optionally `examples/yaml/**`)
  - **Given:** the merge candidate branch
  - **When:** the diff is taken against `main`
  - **Then:** zero files outside `docs/**` / `examples/yaml/**`

### AC-9 — Reuse existing `examples/yaml/` files where possible

**Coverage: PARTIAL (planned)** · **Implementation status: PRE-WORK COMPLETE** — `examples/yaml/dynamic_parallel_action_mode.yaml`, `dynamic_parallel_steps_mode.yaml`, `dynamic_parallel_subgraph_mode.yaml` already exist. The *linkage* between them and the new table column is not yet verifiable (no section yet).

Caveat: scenario INT-008 verifies the link target resolves but does not enforce that *if* an example exists, the table *must* link to it. Author-discretion clause in AC-9 is what keeps coverage at PARTIAL.

Given-When-Then mappings:

- **Integration · TEA-DX-001.8-INT-008 (P2)** — `Example link` cell either points to one of the existing `examples/yaml/dynamic_parallel_*_mode.yaml` files OR to a same-doc anchor for the inline AC-3 snippet
  - **Given:** the comparison table
  - **When:** each `Example link` cell is followed
  - **Then:** the link target resolves (file exists or anchor present); zero 404s

### AC-10 — Documentation builds cleanly

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED**

Given-When-Then mappings:

- **Integration · TEA-DX-001.8-INT-009 (P1, NFR-rec AC-16)** — markdown linter (`markdownlint` / `pymarkdown`) on modified files
  - **Given:** the modified docs
  - **When:** the linter runs
  - **Then:** zero new errors vs. baseline
- **Integration · TEA-DX-001.8-INT-010 (P1, NFR-rec AC-16)** — link/anchor checker (`lychee` / `markdown-link-check`) on modified files
  - **Given:** the modified docs
  - **When:** the checker runs
  - **Then:** zero broken links and the new anchor is referenceable

### AC-11 — Reader unfamiliar with TEA can pick the right mode

**Coverage: FULL (planned)** · **Implementation status: NOT STARTED** — requires a human reviewer post-PR.

Given-When-Then mappings:

- **E2E · TEA-DX-001.8-E2E-002 (P1)** — cold-read forced-choice
  - **Given:** a reviewer who has not previously authored or reviewed a `dynamic_parallel` PR; only the rendered new section is provided
  - **When:** they answer three forced-choice prompts (per-item LLM, 4-step pipeline per item, reuse existing workflow)
  - **Then:** ≥ 2/3 correct in < 90 seconds (closes BUS-001)

### AC-15 — Source-side comment in `yaml_nodes.py` (NFR-recommended, optional)

**Coverage: NONE (intentional)** · **Implementation status: WILL NOT IMPLEMENT in this PR**

Conflicts with AC-8 ("no code changes"). Risk-profile TECH-002 residual is accepted and monitored only via E2E-003 post-merge.

---

## Traceability Matrix (compact)

| AC          | Test IDs                                          | Coverage | Levels       | Implemented? |
| ----------- | ------------------------------------------------- | -------- | ------------ | ------------ |
| AC-1        | UNIT-001, UNIT-002                                | FULL     | unit         | No           |
| AC-2        | UNIT-003, UNIT-004, INT-001                       | FULL     | unit + int   | No           |
| AC-3        | UNIT-005, INT-002, INT-003                        | FULL     | unit + int   | No           |
| AC-4        | UNIT-006, INT-004                                 | FULL     | unit + int   | Engine rule yes; doc-pin no |
| AC-5        | UNIT-007, INT-005                                 | FULL     | unit + int   | Engine rule yes; doc-pin no |
| AC-6        | INT-006, INT-007                                  | FULL     | int          | No           |
| AC-7 / 13   | UNIT-008, E2E-001, E2E-003                        | PARTIAL  | unit + e2e   | No           |
| AC-8        | UNIT-009                                          | FULL     | unit         | No (will be a CI/PR check) |
| AC-9        | INT-008                                           | PARTIAL  | int          | Examples exist; link wiring no |
| AC-10 / 16  | INT-009, INT-010                                  | FULL     | int          | No           |
| AC-11       | E2E-002                                           | FULL     | e2e          | No           |
| AC-12       | INT-003                                           | FULL     | int          | No           |
| AC-14       | INT-007                                           | FULL     | int          | No           |
| AC-15       | (none — intentional)                              | NONE     | —            | n/a          |
| Downstream  | E2E-003 (TEA-DX-001.5 link integrity, post-merge) | FULL     | e2e          | Blocked on TEA-DX-001.5 |

---

## Critical Gaps

1. **No execution-level proof of doc accuracy without AC-12 (INT-003)**
   - Risk: TECH-001 (Medium) — table cells could describe behavior that doesn't match runtime.
   - Action: Adopt NFR-recommended AC-12 so INT-003 is mandated, not optional.
2. **Anchor stability rests on author convention without AC-13 (UNIT-008)**
   - Risk: OPS-001 (Low) — TEA-DX-001.5 error-message links break if the heading is renamed.
   - Action: Adopt NFR-recommended AC-13 (explicit `{#…}` attribute or load-bearing comment).
3. **Cross-link verified by grep only without AC-14 (INT-007)**
   - Risk: OPS-002 (Minimal) — grep passes, real renderer 404s.
   - Action: Adopt NFR-recommended AC-14 (render-and-click).
4. **Build-clean asserted only by AC-10 wording (no required lint/link-check) without AC-16 (INT-009 / INT-010)**
   - Risk: OPS-002 (Minimal) — silent markdown-render degradation.
   - Action: Adopt NFR-recommended AC-16 (lint + link/anchor checker).
5. **No source-side drift guard (AC-15)**
   - Risk: TECH-002 (Low) — accepted residual; AC-8 forbids the only mitigation in-PR.
   - Action: Track via post-merge monitoring (E2E-003) and / or schedule a follow-up issue when `_create_dynamic_parallel_*_branch` is next refactored.
6. **No drift guard for engine error wording vs. docs after merge**
   - Risk: TECH-001 residual — INT-004 / INT-005 only run on the doc PR. A future engine error-text change would silently invalidate AC-4 / AC-5 wording.
   - Action: Optional — promote INT-004 / INT-005 into the standing test suite (not gated to TEA-DX-001.8 PR), so any engine change re-validates the docs phrasing.

---

## Implementation Status Snapshot (2026-05-01)

- `docs/shared/YAML_REFERENCE.md` does **not** contain `## Dynamic Parallel: Branch Body Modes`.
- `docs/python/actions-reference.md` does **not** contain a link with anchor `#dynamic-parallel-branch-body-modes`.
- Engine rules backing AC-4 and AC-5 are **already implemented and tested** (see `python/src/the_edge_agent/yaml_nodes.py:1146-1163` and `python/tests/test_yaml_dynamic_parallel.py:77-119`); INT-004 / INT-005 will reuse those mechanisms but assert *docs↔engine wording consistency*, which is currently unasserted.
- Existing `examples/yaml/dynamic_parallel_{action,steps,subgraph}_mode.yaml` files satisfy AC-9 reuse if linked from the table.
- Story status is **Draft** — no PR, no implementation work yet observable.

---

## Test Design Recommendations

Promote NFR-recommended ACs (12, 13, 14, 16) to keep the trace at FULL coverage for AC-7 and to make INT-003 / UNIT-008 / INT-007 / INT-009 / INT-010 mandatory rather than optional. Without the promotion, planned coverage drops to:

- AC-3 → PARTIAL (UNIT-005, INT-002 only — no execution-level test)
- AC-7 → PARTIAL (E2E-001 only — anchor stability rests on slug rules)
- AC-6 → PARTIAL (INT-006 only — grep, not render-and-click)
- AC-10 → PARTIAL (no automated lint or link checker)

Adopt the three new fixtures called out in the test design (`items_input.yaml`, `no_fan_in.yaml`, `double_body.yaml`) and reuse the three existing `examples/yaml/dynamic_parallel_*_mode.yaml` files for AC-9. No new mocks, networking, or LLM credentials needed.

---

## Risk Assessment

- **High Risk:** none — no AC has zero coverage by accident. AC-15 is intentionally uncovered with documented rationale.
- **Medium Risk:** AC-7 (anchor stability) if AC-13 is not adopted — anchor relies on slugification rules and author care; downstream TEA-DX-001.5 depends on it.
- **Low Risk:** AC-9 — partial coverage by design (author may inline a snippet rather than link an example).
- **Accepted Residual:** TECH-002 (drift after merge) — only post-merge E2E-003 covers it.

---

## Gate YAML Block

```yaml
trace:
  totals:
    requirements: 15
    full: 13
    partial: 2
    none: 1
  scenarios_total: 18
  by_level:
    unit: 7
    integration: 8
    e2e: 3
  by_priority:
    p0: 7
    p1: 7
    p2: 3
    p3: 1
  planning_ref: 'docs/qa/assessments/TEA-DX-001.8-test-design-20260501.md'
  uncovered:
    - ac: 'AC-15'
      reason: 'Source-side comment in yaml_nodes.py conflicts with AC-8 (no code changes); residual TECH-002 risk accepted and monitored via E2E-003 post-merge'
  partial:
    - ac: 'AC-7'
      reason: 'Anchor stability is asserted but only enforced if NFR-rec AC-13 is adopted'
    - ac: 'AC-9'
      reason: 'Author-discretion clause: example reuse not strictly required when an inline snippet suffices'
  notes: 'Story is in Draft at trace time — none of the 18 scenarios are implemented. Engine rules backing AC-4 / AC-5 already enforced (yaml_nodes.py:1146-1163) and tested (test_yaml_dynamic_parallel.py); doc↔engine wording-consistency tests (INT-004/INT-005) are new. See docs/qa/assessments/TEA-DX-001.8-trace-20260501.md.'
```

---

## Trace References

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.8-trace-20260501.md
Total ACs in scope: 15 · Fully covered (planned): 13 · Partially covered (planned): 2 · Intentionally uncovered: 1
```
