# Risk Profile: Story TEA-DX-001.8

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: docs/stories/TEA-DX-001.8-dynamic-parallel-mode-comparison-doc.md
Mode: YOLO

## Executive Summary

- Total Risks Identified: 5
- Critical Risks: 0
- High Risks: 0
- Medium Risks: 1
- Low Risks: 4
- Risk Score: 87/100 (calculated)
- Overall Risk Level: **LOW**

This is a **documentation-only** story with no code changes (AC-8). The risk surface is limited to documentation accuracy, link stability, and downstream coupling with TEA-DX-001.5 (which depends on a stable anchor produced by this story).

## Critical Risks Requiring Immediate Attention

None. No risks scored ≥ 6.

## Risk Distribution

### By Category

- Technical (TECH): 2 risks (0 critical)
- Operational (OPS): 2 risks (0 critical)
- Business (BUS): 1 risk (0 critical)
- Security (SEC): 0 risks
- Performance (PERF): 0 risks
- Data (DATA): 0 risks

### By Component

- `docs/shared/YAML_REFERENCE.md`: 3 risks
- `docs/python/actions-reference.md` (cross-link): 1 risk
- Coupling with TEA-DX-001.5 (error message anchor): 1 risk

## Detailed Risk Register

| Risk ID  | Category | Title                                                          | Probability | Impact     | Score | Priority |
| -------- | -------- | -------------------------------------------------------------- | ----------- | ---------- | ----- | -------- |
| TECH-001 | Technical | Documented behavior diverges from `_create_dynamic_parallel_*_branch` semantics | Medium (2)  | Medium (2) | 4     | Medium   |
| OPS-001  | Operational | Anchor URL slug instability breaks TEA-DX-001.5 error message links             | Low (1)     | Medium (2) | 2     | Low      |
| TECH-002 | Technical | Implementation drift after merge (state-passing or fan-in semantics change)     | Low (1)     | Medium (2) | 2     | Low      |
| OPS-002  | Operational | Broken cross-link between `actions-reference.md` and the new section            | Low (1)     | Low (1)    | 1     | Minimal  |
| BUS-001  | Business | Comparison table fails to resolve user confusion (effectiveness)                | Low (1)     | Low (1)    | 1     | Minimal  |

## Detailed Risk Analysis

### TECH-001: Documentation drifts from implementation behavior

**Score: 4 (Medium)**

**Probability**: Medium - The three `_create_dynamic_parallel_*_branch` helpers in `python/src/the_edge_agent/yaml_nodes.py:1177-1192` have subtle differences in state copying (deep copy vs scoped subset), error propagation, and return shape. Authoring an accurate comparison without re-reading the implementations is moderately likely to produce small inaccuracies.

**Impact**: Medium - Inaccurate docs produce a worse user experience than the status quo (reading source). Users may pick the wrong mode and hit hard-to-debug runtime issues. AC-11 ("a reader can pick the right mode") becomes false.

**Mitigation** (preventive):
- Task 1 in the story already requires auditing `yaml_nodes.py:1177-1300`; enforce this as a checklist gate.
- Cross-check each table column against the actual helper code (state passing, reusability, definition site).
- Have a second reviewer (per Definition of Done) explicitly verify the "Branch state" and "Reusability" columns against implementation.

**Testing Focus**:
- Manual review against `yaml_nodes.py:1177-1300` for each of the three modes.
- Run each minimal example (AC-3) end-to-end via `tea run` against a small fixture to confirm documented behavior matches actual execution.
- Confirm `parallel_results` shape and `fan_in:` requirement (AC-5) by reference to existing tests for `dynamic_parallel`.

**Residual Risk**: Low - Behavioral subtleties may still be missed but the audit + dual review catch the high-value inaccuracies.

---

### OPS-001: Anchor URL slug instability breaks downstream coupling

**Score: 2 (Low)**

**Probability**: Low - Markdown anchor generation from headings is deterministic in standard renderers, but custom renderers (e.g., MkDocs with custom slug strategies) can differ.

**Impact**: Medium - TEA-DX-001.5 explicitly depends on this anchor for error messages (AC-7). If the anchor changes after TEA-DX-001.5 ships, error messages will silently 404.

**Mitigation** (preventive):
- Pin the anchor explicitly: `## Dynamic Parallel: Branch Body Modes {#dynamic-parallel-branch-body-modes}` if the renderer supports explicit anchors; otherwise standardize on the GitHub-flavored slug.
- Add a comment in the YAML_REFERENCE.md source noting that the anchor is load-bearing and renaming the heading is a breaking change for TEA-DX-001.5.
- TEA-DX-001.5 implementation should include a test that asserts the anchor resolves (HTTP HEAD or markdown lint).

**Testing Focus**:
- Manually verify the anchor resolves in the rendered docs site.
- Verify the anchor remains stable across the supported markdown renderers (GitHub preview, MkDocs if used).

**Residual Risk**: Low - Once pinned and noted, drift requires deliberate action.

---

### TECH-002: Implementation drift after docs merge

**Score: 2 (Low)**

**Probability**: Low - The three branch helpers are stable (no active refactor noted), but state-passing or `fan_in:` semantics could change in a future feature.

**Impact**: Medium - Future code changes could silently invalidate the comparison table; readers would trust stale docs.

**Mitigation** (detective):
- Add a comment in `yaml_nodes.py` near the three helpers pointing to the YAML_REFERENCE.md section: "If state-passing semantics change, update `docs/shared/YAML_REFERENCE.md#dynamic-parallel-branch-body-modes`."
- Track via the documented coordination with TEA-DX-001.5 (the error-message tests will surface anchor breakage but not behavioral drift).

**Testing Focus**:
- No new automated tests required for this story.
- Recommend: future PRs touching `_create_dynamic_parallel_*_branch` include a docs review checklist item.

**Residual Risk**: Low - Process-level mitigation; relies on developer discipline.

---

### OPS-002: Broken cross-link from actions-reference.md

**Score: 1 (Minimal)**

**Probability**: Low - Standard markdown link, easily verified.

**Impact**: Low - One broken navigation step; readers can find the target via search.

**Mitigation** (preventive):
- Verify the cross-link manually after editing both files.
- Run any markdown lint tooling in the repo (mentioned in Task 4) to catch broken links.

**Testing Focus**:
- Visual inspection of rendered markdown.
- If the repo has a docs lint step, run it.

**Residual Risk**: Minimal.

---

### BUS-001: Comparison table doesn't actually solve user confusion

**Score: 1 (Minimal)**

**Probability**: Low - The original user feedback ("Achei a sintaxe correta lendo source") is specific; AC-11 directly addresses it.

**Impact**: Low - Worst case, users still read source. No regression vs current state.

**Mitigation** (detective):
- AC-11 requires the table be useful to a reader unfamiliar with TEA; have the reviewer (per Definition of Done) read the section cold and confirm they could pick a mode for a hypothetical use case.
- Solicit feedback from the original reporter if practical.

**Testing Focus**:
- One-pass cold-read review by someone not involved in authoring the section.

**Residual Risk**: Minimal.

## Risk-Based Testing Strategy

### Priority 1: Critical Risk Tests

None — no critical risks.

### Priority 2: Medium Risk Tests (TECH-001)

1. **Implementation audit**: Read `yaml_nodes.py:1177-1300` and verify each row of the comparison table matches the actual code.
2. **Example execution**: Run each minimal example (AC-3) via `tea run` against a fixture; confirm:
   - `action` mode receives a deep-copied state with `item` and `index` injected.
   - `steps` mode executes a sequential mini-pipeline and merges results.
   - `subgraph` mode delegates to a separate file with `input_mapping`.
3. **Mutual exclusion**: Verify a YAML file with two of `action`/`steps`/`subgraph` raises a clear error (AC-4 — confirms the rule documented matches enforcement).
4. **`fan_in:` requirement**: Verify a `dynamic_parallel` node without a `fan_in:` sibling raises an error referencing an existing node (AC-5).

### Priority 3: Low/Minimal Risk Tests

1. **Anchor stability**: Render the docs locally, navigate to `#dynamic-parallel-branch-body-modes`, confirm it resolves.
2. **Cross-link integrity**: Click the link in `actions-reference.md`, confirm it lands on the new section.
3. **Cold-read review**: A reviewer unfamiliar with `dynamic_parallel` reads the section and picks a mode for a sample scenario; confirms AC-11.
4. **Markdown lint**: Run any repo-level docs lint (Task 4); confirm no warnings.

## Risk Acceptance Criteria

### Must Fix Before Production

None — no risks scored ≥ 6.

### Can Deploy with Mitigation

- TECH-001: Mitigated by Task 1 audit + Definition-of-Done dual review.

### Accepted Risks

- OPS-001, TECH-002, OPS-002, BUS-001: Mitigations are sufficient; no further action required.

## Monitoring Requirements

Post-deployment monitoring for:

- **TEA-DX-001.5 integration**: When the dependent story ships, confirm error messages successfully link to this anchor (HTTP 200 or in-repo anchor resolution).
- **User feedback**: Track whether new "I read the source to figure out X" reports persist after this section ships.

## Risk Review Triggers

Review and update risk profile when:

- `_create_dynamic_parallel_action_branch`, `_create_dynamic_parallel_steps_branch`, or `_create_dynamic_parallel_subgraph_branch` are modified.
- `fan_in:` semantics or the mutual-exclusion rule changes.
- The docs site renderer or slug strategy changes.
- TEA-DX-001.5 ships and references the anchor.

## Gate Mapping

Per deterministic gate mapping (no risk ≥ 9, no risk ≥ 6):

**Recommended Gate: PASS**

---

## risk_summary (paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 0
    medium: 1
    low: 4
  highest:
    id: TECH-001
    score: 4
    title: 'Documented behavior diverges from _create_dynamic_parallel_*_branch semantics'
  recommendations:
    must_fix: []
    monitor:
      - 'Audit yaml_nodes.py:1177-1300 against the comparison table during review (Task 1)'
      - 'Pin the section anchor explicitly so TEA-DX-001.5 error messages remain valid'
      - 'Add a code comment near the three branch helpers pointing back to the docs section to discourage drift'
```

---

Risk profile: docs/qa/assessments/TEA-DX-001.8-risk-20260501.md
