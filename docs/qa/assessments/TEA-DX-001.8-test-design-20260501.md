# Test Design: Story TEA-DX-001.8 — Document `action` vs `steps` vs `subgraph` in `dynamic_parallel`

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO

## Test Strategy Overview

- **Total test scenarios:** 18
- **Unit tests (static doc checks):** 7 (39%)
- **Integration tests (lint + example execution):** 8 (44%)
- **E2E tests (cold-read + downstream link):** 3 (17%)
- **Priority distribution:** P0: 7, P1: 7, P2: 3, P3: 1

Strategy notes:
- This is a **documentation-only** story (AC-8). The "system under test" is a markdown section plus its cross-link and a small set of minimal example snippets. Test levels are mapped accordingly:
  - **Unit** = static, content-only assertions against the rendered markdown (presence of section, table columns, mutual-exclusion sentence, `fan_in:` sentence, anchor declaration, code-fence count). These are file-grep / parser checks — fast, deterministic, no environment.
  - **Integration** = the section interacting with neighbours: anchor resolves in the rendered HTML, cross-link from `actions-reference.md` lands on the right anchor, each AC-3 minimal example is loadable + executable by the YAML engine, and the documented mutual-exclusion / `fan_in` errors actually match the engine's error wording.
  - **E2E** = human cold-read (AC-11) + verification that TEA-DX-001.5 error messages will resolve to this anchor once it ships.
- Risk surface from `TEA-DX-001.8-risk-20260501.md` is dominated by **TECH-001 (medium): docs diverge from `_create_dynamic_parallel_*_branch` semantics**. Two scenarios (INT-002 and INT-003) carry this mitigation by executing each example and asserting documented state semantics hold at runtime.
- The NFR assessment (`TEA-DX-001.8-nfr-20260501.md`) recommends adding AC-12..AC-16. This test design treats those as **in-scope** and assigns coverage to each; if the team chooses not to adopt them, drop the corresponding scenarios marked `(NFR-rec)`.

## Test Scenarios by Acceptance Criteria

### AC-1 — New section `## Dynamic Parallel: Branch Body Modes` exists in `YAML_REFERENCE.md`

| ID                   | Level | Priority | Test                                                                                                                       | Justification                                                       |
| -------------------- | ----- | -------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| TEA-DX-001.8-UNIT-001 | Unit  | P0       | grep `docs/shared/YAML_REFERENCE.md` for an H2 line matching `^## Dynamic Parallel: Branch Body Modes` (or `^### …` if nested under existing dynamic_parallel section) | Cheapest existence check; the entire story collapses if this fails  |
| TEA-DX-001.8-UNIT-002 | Unit  | P1       | The new section appears **after** the existing `dynamic_parallel` overview row (line ~762) — i.e., readers see the comparison in context, not orphaned at top of file | Placement constraint stated in AC-1                                 |

### AC-2 — Comparison table with required columns

| ID                   | Level | Priority | Test                                                                                                                                                              | Justification                                                                  |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| TEA-DX-001.8-UNIT-003 | Unit  | P0       | Parse the section's first markdown table; assert header row contains, case-insensitive: `Mode`, `When to use`, `Branch state`, `Reusability`, `Where it's defined`, `Example link` | Six required columns from AC-2; failing any one means the table is incomplete  |
| TEA-DX-001.8-UNIT-004 | Unit  | P0       | Table body has exactly three rows whose `Mode` cell is one of `action`, `steps`, `subgraph` (no extras, no missing)                                                | Three-mode contract; defends against partial documentation                     |
| TEA-DX-001.8-INT-001  | Integration | P0 | For each of the three table rows, manually verify the cell text against `python/src/the_edge_agent/yaml_nodes.py:1468-1601` (the three `_create_dynamic_parallel_*_branch` helpers) — particularly `Branch state` and `Reusability` columns | Direct mitigation for **TECH-001 (Medium)**: docs↔implementation divergence    |

### AC-3 — 5–10 line minimal example for each mode

| ID                   | Level | Priority | Test                                                                                                                                                              | Justification                                                                                |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| TEA-DX-001.8-UNIT-005 | Unit  | P1       | Section contains exactly three fenced YAML blocks (` ```yaml `) whose body lengths fall in `[5, 10]` lines (excluding fences)                                       | Pins the "5–10 line minimal example" wording in AC-3                                          |
| TEA-DX-001.8-INT-002  | Integration | P0 | Extract each of the three fenced YAML examples, write to tmp file, load with `YAMLEngine` and `compile()`; assert no validation errors                              | Examples must be runnable, not aspirational; minimum bar before AC-12 promotion              |
| TEA-DX-001.8-INT-003  | Integration | P1 | (NFR-rec, AC-12) Execute each example via `tea run` against a tiny `items` fixture; assert outputs match the documented `Branch state` column for that mode         | Behavioral verification of the comparison table; closes Risk-Profile P1 and NFR Reliability  |

### AC-4 — Section explicitly states mutual-exclusion of `action` / `steps` / `subgraph`

| ID                   | Level | Priority | Test                                                                                                                                                       | Justification                                                                |
| -------------------- | ----- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| TEA-DX-001.8-UNIT-006 | Unit  | P0       | Section body contains a sentence (or callout) that names `action`, `steps`, AND `subgraph` AND uses one of: "exactly one", "mutually exclusive", "only one of" | Wording contract from AC-4; cheap regex                                       |
| TEA-DX-001.8-INT-004  | Integration | P1 | Build a YAML node specifying both `action:` and `steps:` (or any two of the three) under one `dynamic_parallel`; load via `YAMLEngine`; assert engine raises a clear error and the error text either matches or is consistent with the docs phrasing | Pins documented rule against actual engine behavior; if engine **doesn't** enforce, the doc is lying — this test surfaces it |

### AC-5 — Section explicitly notes the `fan_in:` requirement

| ID                   | Level | Priority | Test                                                                                                                                                  | Justification                                                                |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| TEA-DX-001.8-UNIT-007 | Unit  | P0       | Section body contains the literal string `fan_in:` AND a phrase like "required" / "must reference" within the same paragraph                            | Wording contract from AC-5; cheap regex                                       |
| TEA-DX-001.8-INT-005  | Integration | P1 | Build a `dynamic_parallel` node missing `fan_in:`; load via `YAMLEngine`; assert engine raises an error whose text matches the documented requirement | Pins documented rule against engine behavior (parallels INT-004 for AC-4)     |

### AC-6 — Cross-link from `docs/python/actions-reference.md`

| ID                   | Level       | Priority | Test                                                                                                                                                                       | Justification                                                                  |
| -------------------- | ----------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| TEA-DX-001.8-INT-006  | Integration | P0       | grep `docs/python/actions-reference.md` for a relative link whose target is `…/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes` (or the pinned slug)                   | AC-6 + NFR-rec AC-14; simple existence check                                  |
| TEA-DX-001.8-INT-007  | Integration | P1       | Render `actions-reference.md` (e.g., via `python -m markdown` or local docs preview) and click-resolve the cross-link; assert it lands on the new section heading element   | End-to-end resolution of the cross-link, not just textual match               |

### AC-7 — Anchor URL is stable

| ID                   | Level       | Priority | Test                                                                                                                                                                                  | Justification                                                                                                                  |
| -------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| TEA-DX-001.8-UNIT-008 | Unit        | P0       | (NFR-rec, AC-13) Section heading is followed by an explicit `{#dynamic-parallel-branch-body-modes}` attribute, OR the heading text deterministically slugifies to `dynamic-parallel-branch-body-modes` AND a load-bearing comment above the heading names the slug | Pinning anchor is the highest-leverage mitigation for **OPS-001**; either explicit attribute or comment-pin satisfies AC-7+AC-13 |
| TEA-DX-001.8-E2E-001  | E2E         | P2       | Render `YAML_REFERENCE.md` to HTML; assert `id="dynamic-parallel-branch-body-modes"` exists on the new section element                                                                 | Real renderer check; defends against renderer-specific slug rules                                                              |

### AC-8 — No code changes

| ID                   | Level | Priority | Test                                                                                                                       | Justification                                          |
| -------------------- | ----- | -------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------ |
| TEA-DX-001.8-UNIT-009 | Unit  | P1       | `git diff --name-only main...HEAD` on the merge candidate touches only `docs/**` and (optionally) `examples/yaml/**`; touches zero `python/src/**` or `rust/src/**` files | Hard contract from AC-8; trivial CI check              |

### AC-9 — Reuse existing `examples/yaml/` files where possible

| ID                   | Level       | Priority | Test                                                                                                                                                                                                                | Justification                                                            |
| -------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| TEA-DX-001.8-INT-008  | Integration | P2       | The `Example link` column in the comparison table either (a) links to one of `examples/yaml/dynamic_parallel_{action,steps,subgraph}_mode.yaml`, OR (b) links to a same-doc anchor for the inline AC-3 snippet      | AC-9: prefer reuse; verifies links resolve and don't 404                  |

### AC-10 — Documentation builds cleanly

| ID                   | Level       | Priority | Test                                                                                                                                                                                                              | Justification                                                                  |
| -------------------- | ----------- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| TEA-DX-001.8-INT-009  | Integration | P1       | (NFR-rec, AC-16) Run a markdown linter (e.g., `markdownlint` or `pymarkdown`) on the modified files; assert zero new errors vs. baseline                                                                            | AC-10 / AC-16; broken markdown silently degrades the comparison-table render   |
| TEA-DX-001.8-INT-010  | Integration | P1       | (NFR-rec, AC-16) Run a link/anchor checker (e.g., `lychee` or `markdown-link-check`) on the two modified files; assert zero broken links and the new anchor is referenceable                                       | Catches OPS-001 + OPS-002 in one pass                                          |

### AC-11 — Reader unfamiliar with TEA can pick the right mode

| ID                   | Level | Priority | Test                                                                                                                                                                                                                                                | Justification                                                                  |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| TEA-DX-001.8-E2E-002  | E2E   | P1       | Cold-read review: a reviewer who has not previously used `dynamic_parallel` reads only the new section and answers three forced-choice prompts ("which mode for: a one-call-per-item LLM batch?", "a 4-step pipeline per item?", "reusing a workflow already deployed elsewhere?"); ≥ 2/3 correct without consulting source | The only real test of AC-11; closes BUS-001                                    |

### Downstream coupling — TEA-DX-001.5 link integrity (post-merge)

| ID                   | Level | Priority | Test                                                                                                                                                                                                                                                | Justification                                                                  |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| TEA-DX-001.8-E2E-003  | E2E   | P3       | After TEA-DX-001.5 ships, trigger one of its enriched error messages and click the embedded doc link; assert it resolves to `#dynamic-parallel-branch-body-modes`                                                                                     | Confirms the load-bearing anchor still resolves end-to-end; closes Risk-Profile monitoring item |

## Risk Coverage

Maps to risks in `docs/qa/assessments/TEA-DX-001.8-risk-20260501.md`:

| Risk ID  | Severity | Mitigated by                                                                                                                                                |
| -------- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| TECH-001 | Medium   | INT-001 (manual cell-by-cell audit), INT-002 (examples parse), INT-003 (examples execute and behavior matches table), INT-004/INT-005 (rule wording matches engine errors) |
| OPS-001  | Low      | UNIT-008 (anchor pinned), E2E-001 (anchor resolves in HTML), E2E-003 (downstream TEA-DX-001.5 link still resolves)                                          |
| TECH-002 | Low      | E2E-003 (post-merge monitoring); no in-PR test guards against future drift — accepted residual risk per AC-8                                                 |
| OPS-002  | Minimal  | INT-006 (cross-link exists), INT-007 (cross-link resolves), INT-010 (link/anchor checker)                                                                   |
| BUS-001  | Minimal  | E2E-002 (cold-read forced-choice review)                                                                                                                    |

## Test Data & Environment Requirements

**Fixtures:**

1. **`fixtures/dynamic_parallel_doc/items_input.yaml`** — minimal input state with `items: [a, b, c]` for INT-003 example execution. Reusable for all three modes.
2. **`fixtures/dynamic_parallel_doc/no_fan_in.yaml`** — a `dynamic_parallel` node missing `fan_in:`. Used by INT-005.
3. **`fixtures/dynamic_parallel_doc/double_body.yaml`** — a `dynamic_parallel` node specifying both `action:` and `steps:`. Used by INT-004.
4. **Existing examples** referenced where possible (AC-9): `examples/yaml/dynamic_parallel_action_mode.yaml`, `dynamic_parallel_steps_mode.yaml`, `dynamic_parallel_subgraph_mode.yaml`. Re-used by INT-008.

**Tooling:**

- `pytest` (for INT-002..005, INT-009..010, UNIT-001..009).
- `markdown` or `mistune` Python library for table parsing in UNIT-003/UNIT-004.
- Markdown linter — `pymarkdown` (already a Python dep candidate) or `markdownlint-cli` (npx) for INT-009.
- Link checker — `lychee` (Rust binary, already used in some workflows) or `markdown-link-check` for INT-010.
- A tiny doc-renderer for E2E-001 (`python -m markdown` or `mkdocs build --strict` if the project ships one; Sphinx is fine too).
- No DB, LTM backend, network, or LLM credentials needed.

**Test data shape (UNIT-003 — column header parse):**

```python
import re, mistune
md = open("docs/shared/YAML_REFERENCE.md").read()
section = re.search(
    r"^## Dynamic Parallel: Branch Body Modes.*?(?=^## )",
    md, re.M | re.S,
).group(0)
table_headers = [h.strip().lower() for h in re.findall(r"^\| (.+?) \|", section, re.M)[0].split("|")]
required = {"mode", "when to use", "branch state", "reusability", "where it's defined", "example link"}
assert required.issubset(set(table_headers))
```

**Test data shape (INT-002 — examples parse):**

```python
from the_edge_agent.yaml_engine import YAMLEngine
for snippet in extract_yaml_fences(section):
    engine = YAMLEngine.from_yaml_string(snippet)
    engine.compile()  # must not raise
```

**Test data shape (INT-004 — mutual-exclusion error matches docs):**

```python
yaml = """
nodes:
  - name: bad
    type: dynamic_parallel
    items: "{{ state.items }}"
    fan_in: collect
    action: { uses: noop }
    steps:  [{ run: "pass" }]
  - name: collect
    run: "return state"
"""
with pytest.raises(ValueError) as exc:
    YAMLEngine.from_yaml_string(yaml).compile()
assert any(word in str(exc.value).lower() for word in ("exclusive", "exactly one", "only one"))
```

**Test data shape (INT-005 — missing fan_in):**

```python
yaml = """
nodes:
  - name: bad
    type: dynamic_parallel
    items: "{{ state.items }}"
    action: { uses: noop }
"""
with pytest.raises(ValueError) as exc:
    YAMLEngine.from_yaml_string(yaml).compile()
assert "fan_in" in str(exc.value)
```

**Cold-read protocol (E2E-002):**

- Reviewer: any team member who has not authored or reviewed a `dynamic_parallel` PR.
- Setup: hand them only the rendered new section as a standalone HTML page (no source, no surrounding doc).
- Three forced-choice questions (each maps to a row of the table):
  1. "I want to call one LLM action per item in parallel — which mode?" → `action`
  2. "I want to run a 4-step pipeline per item — which mode?" → `steps`
  3. "I want to reuse an existing workflow YAML for each item — which mode?" → `subgraph`
- Pass if ≥ 2/3 correct in < 90 seconds.

## Recommended Execution Order

1. **P0 Unit** (UNIT-001, UNIT-003, UNIT-004, UNIT-006, UNIT-007, UNIT-008) — file-grep / static checks; fail fast on missing structure.
2. **P0 Integration** (INT-001, INT-002, INT-006) — implementation audit, examples parse, cross-link exists.
3. **P1 Unit** (UNIT-002, UNIT-005, UNIT-009) — placement, snippet length, no-code-change diff.
4. **P1 Integration** (INT-003, INT-004, INT-005, INT-007, INT-009, INT-010) — example execution, error-wording pinning, cross-link render, lint+link-check.
5. **P1 E2E** (E2E-002) — cold-read review.
6. **P2** (INT-008, E2E-001) — example-link check, HTML anchor presence.
7. **P3** (E2E-003) — post-TEA-DX-001.5-merge monitoring.

## Coverage Gaps

None against the in-scope AC set (AC-1 .. AC-11) and the NFR-recommended additions (AC-12 / AC-13 / AC-14 / AC-16). AC-15 (source-side comment in `yaml_nodes.py`) is intentionally **uncovered** because adopting it conflicts with AC-8 ("no code changes"); the residual TECH-002 risk is accepted in the risk profile.

If the team chooses NOT to adopt the NFR-recommended ACs, the following scenarios become out-of-scope and may be dropped:
- INT-003 (was AC-12 promotion) — without it, you have no execution-level proof the table cells match runtime behavior; only the manual audit (INT-001) defends TECH-001.
- UNIT-008 (was AC-13 enforcement) — without it, anchor stability rests on GitHub's GFM slug rules and is fragile to heading edits.
- INT-007 (was AC-14 promotion) — without it, the cross-link is verified by grep only.
- INT-009 / INT-010 (were AC-16 promotion) — without them, AC-10 ("docs build cleanly") is asserted by visual inspection only.

## Gate YAML Block

```yaml
test_design:
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
  coverage_gaps: []
  notes:
    - "AC-12/13/14/16 (NFR-recommended) are covered; if not adopted, drop INT-003, UNIT-008, INT-007, INT-009, INT-010"
    - "AC-15 intentionally uncovered (conflicts with AC-8); residual TECH-002 risk accepted"
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.8-test-design-20260501.md
P0 tests identified: 7
```

## Quality Checklist

- [x] Every AC (1–11) has at least one test scenario; every NFR-recommended AC (12, 13, 14, 16) also has coverage; AC-15 explicitly excluded with rationale
- [x] Test levels are appropriate (static doc shape → unit; engine + lint + link → integration; cold-read + post-merge link → e2e)
- [x] No duplicate coverage across levels (cell-by-cell audit at INT-001 is the only manual step; UNIT tests are pure structural; execution-level lives only in INT-003)
- [x] Priorities align with risk profile (TECH-001 mitigations are P0; OPS/BUS items P2/P3)
- [x] Test IDs follow naming convention `TEA-DX-001.8-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent — no test depends on another's side effects
