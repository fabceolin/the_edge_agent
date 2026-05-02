# NFR Assessment: TEA-DX-001.8

Date: 2026-05-01
Reviewer: Quinn
Mode: YOLO (non-interactive; default core four NFRs)
Story: `docs/stories/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.md`

## Summary

- Security: PASS — Documentation-only change (AC-8). No new auth/input/secret surface; YAML examples in the section are illustrative and do not change `exec()` semantics already documented in CLAUDE.md.
- Performance: PASS — N/A. Pure prose addition to `docs/shared/YAML_REFERENCE.md`; no runtime, build-time, or page-load concern at the scale of a single section.
- Reliability: CONCERNS — Documentation accuracy is the load-bearing reliability property here (TECH-001 from risk profile). Story mitigates via Task 1 implementation audit, but no AC requires a *behavioral check* (e.g., running each minimal example via `tea run`) before the docs ship. Risk Profile Priority-1 test is in QA Notes, not promoted to AC.
- Maintainability: CONCERNS — AC-10 ("documentation builds cleanly") is qualitative — no required markdown lint, anchor-resolution check, or cross-link verifier. No source-side guard prevents `_create_dynamic_parallel_*_branch` semantics from drifting away from the documented table (TECH-002).

## Quality Score: **80** (100 − 10 × 2 CONCERNS)

## Gate YAML Block

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'Docs-only (AC-8). No new code, no new auth/input/secret surface. Inline YAML examples ride existing exec() warning already in CLAUDE.md.'
  performance:
    status: PASS
    notes: 'N/A — single section in YAML_REFERENCE.md. No runtime, build, or render impact at this scale.'
  reliability:
    status: CONCERNS
    notes: 'TECH-001 mitigation depends on Task 1 audit, but no AC requires executing each minimal example to confirm documented "Branch state" / "Reusability" / mutual-exclusion behavior matches yaml_nodes.py:1468-1600. Promote risk-profile P1 behavioral verification into an AC.'
  maintainability:
    status: CONCERNS
    notes: 'AC-10 ("docs build cleanly") is qualitative. No required anchor-resolution check for #dynamic-parallel-branch-body-modes, no cross-link verifier from actions-reference.md, no source-side comment near _create_dynamic_parallel_*_branch helpers to flag docs review on future drift (OPS-001, TECH-002).'
```

## Critical Issues

1. **Behavioral verification of minimal examples not required by ACs** (Reliability)
   - Risk: TECH-001 from risk profile — comparison table cells (Branch state, Reusability, error propagation) drift from actual `_create_dynamic_parallel_*_branch` semantics; reader trusts table and writes a workflow that breaks at runtime.
   - Fix: Add an AC requiring each AC-3 minimal example (one per mode) to be executed against `tea run` (or an integration-test fixture) and produce the documented behavior. Pin the example YAML to a path in `examples/yaml/` or `tests/` so it can be regression-tested.

2. **Anchor stability is unverified** (Maintainability / Reliability)
   - Risk: OPS-001 — `#dynamic-parallel-branch-body-modes` is the contract surface for TEA-DX-001.5 error messages. AC-7 says "anchor URL is stable" but does not require *how* it is pinned (explicit `{#…}` attribute vs. relying on GFM slug derivation).
   - Fix: Promote the risk-profile mitigation into an AC: "Heading uses an explicit anchor attribute or is accompanied by a comment naming the slug as load-bearing for TEA-DX-001.5." Add a smoke test (or note in TEA-DX-001.5's PR) that asserts the anchor resolves.

3. **No source-side drift guard** (Maintainability)
   - Risk: TECH-002 — Future refactor of `_create_dynamic_parallel_*_branch` (renamed helpers, changed state-passing, altered error propagation) leaves the docs silently stale.
   - Fix: Add an AC requiring a comment near each of the three helpers in `yaml_nodes.py` pointing to the YAML_REFERENCE.md section, so the next refactorer is prompted to update both.

## Quick Wins

- Promote risk-profile P1 behavioral test (run each minimal example) into an AC: ~10 min of writing
- Add explicit anchor attribute `{#dynamic-parallel-branch-body-modes}` and document the convention: ~5 min
- Add three-line `# See: docs/shared/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes` comment above each `_create_dynamic_parallel_*_branch` helper: ~5 min (this is a *code* change but trivial; noteworthy because it bumps the story past AC-8 "no code changes" — author should decide whether to scope-creep or split)
- Run a markdown anchor checker (e.g., `markdown-link-check`) as part of CI or a pre-merge command for `docs/shared/YAML_REFERENCE.md`: ~30 min

## Acceptance Criteria — Recommendations

Add (or promote from QA Notes):

- **AC-12:** Each AC-3 minimal example executes successfully under `tea run` (or an equivalent integration fixture) and produces the behavior described by its row in the comparison table. Examples are committed (in `examples/yaml/` or test fixtures) so they remain regression-testable.
- **AC-13:** Heading anchor is pinned via an explicit attribute (e.g., `{#dynamic-parallel-branch-body-modes}`) OR a `<!-- Anchor: load-bearing for TEA-DX-001.5 -->` comment immediately above the heading documents the dependency.
- **AC-14:** Cross-link from `docs/python/actions-reference.md` is verified to resolve (manual click-through or automated link check) before merge.
- **AC-15:** Each of `_create_dynamic_parallel_action_branch`, `_create_dynamic_parallel_steps_branch`, `_create_dynamic_parallel_subgraph_branch` in `yaml_nodes.py` carries a comment pointing back to the YAML_REFERENCE.md section. (NOTE: This adds a code change and conflicts with AC-8 — author must decide whether to expand scope or defer to a follow-up.)
- **AC-16:** A markdown lint or link-check pass (e.g., `markdown-link-check`, `markdownlint`) runs against the modified files and reports zero broken links/anchors.

## Test Recommendations (P0 → P3)

**P0 (must have before merge):**
- Implementation-vs-docs audit: read `python/src/the_edge_agent/yaml_nodes.py:1468-1600` (the three `_create_dynamic_parallel_*_branch` helpers) and verify every cell of the comparison table for `action`, `steps`, `subgraph` (TECH-001).
- Execute each AC-3 minimal example via `tea run` (or pytest fixture) and confirm output matches the documented "Branch state" semantics.
- Verify mutual-exclusion error (AC-4): a YAML with two of `action`/`steps`/`subgraph` raises the engine's actual error and the doc text matches that error's wording.
- Verify `fan_in:` requirement (AC-5): a YAML without `fan_in:` raises the actual engine error matching the doc text.

**P1 (should have):**
- Render `docs/shared/YAML_REFERENCE.md` locally (e.g., `mkdocs serve` if used, or GitHub preview) and confirm `#dynamic-parallel-branch-body-modes` resolves.
- Click-through cross-link from `docs/python/actions-reference.md` to the new section.
- Cold-read review by a non-author confirms AC-11 (reader can pick the right mode from the table).

**P2 (nice to have):**
- Run a markdown link checker against `docs/shared/YAML_REFERENCE.md` and `docs/python/actions-reference.md`.
- Diff the documented examples against any matching files in `examples/yaml/` to catch silent divergence.

**P3 (manual / monitoring):**
- After TEA-DX-001.5 ships, click any error-message link that points at this anchor and confirm it resolves (closes risk-profile monitoring item).
- Track whether "I read the source to figure out X" feedback persists in user channels (closes BUS-001).

---

NFR assessment: docs/qa/assessments/TEA-DX-001.8-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.yml under nfr_validation
