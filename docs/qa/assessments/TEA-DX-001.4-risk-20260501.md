# Risk Profile: Story TEA-DX-001.4

**Story:** `variables` accessible in Python `run:` blocks
**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO

---

## Executive Summary

- **Total Risks Identified:** 5
- **Critical Risks:** 0
- **High Risks:** 0
- **Medium Risks:** 0
- **Low Risks:** 1
- **Minimal Risks:** 4
- **Risk Score:** 94/100 (very low risk)
- **Recommended Gate Outcome:** **PASS**

This is a minimally invasive, additive change: a single line inserting `engine.variables` into the `exec_globals` dict at `python/src/the_edge_agent/yaml_nodes.py:715`. The pattern mirrors existing Jinja exposure (`yaml_engine.py:533`). Backward compatibility is preserved by the placement *before* `**kwargs` (per AC-5).

---

## Risk Matrix

| Risk ID  | Category | Description                                                         | Probability | Impact   | Score | Priority |
|----------|----------|---------------------------------------------------------------------|-------------|----------|-------|----------|
| TECH-006 | Tech     | Parallel-flow race writing to shared `engine.variables` dict        | Medium (2)  | Low (1)  | 2     | Low      |
| TECH-001 | Tech     | User shadowing `variables` with local var/kwarg confuses scope      | Low (1)     | Low (1)  | 1     | Minimal  |
| TECH-002 | Tech     | In-block mutation of `variables` produces side effects across nodes | Low (1)     | Low (1)  | 1     | Minimal  |
| SEC-001  | Security | Engine state reference exposed inside arbitrary-Python sandbox      | Low (1)     | Low (1)  | 1     | Minimal  |
| OPS-001  | Ops      | Discoverability gap if `actions-reference.md` not updated (AC-10)   | Low (1)     | Low (1)  | 1     | Minimal  |

---

## Risk Detail & Mitigations

### TECH-006 — Parallel-flow race on shared `variables` dict
- **Score:** 2 (Low)
- **Probability:** Medium — TEA explicitly supports parallel fan-out; if multiple parallel branches write to the same key in `variables`, ordering is non-deterministic.
- **Impact:** Low — `engine.variables` is intended as configuration, not as a parallel-write inbox; users have `state` (deep-copied per parallel branch per CLAUDE.md) for that purpose.
- **Mitigation:**
  - **Documentation:** In the AC-10 docs update, explicitly call out: "`variables` is shared by reference across nodes; treat as read-mostly. For per-flow data use `state`. Writes from parallel flows race."
  - **Detective:** Optional follow-up — add a parallel-flow `variables`-write test that demonstrates the non-determinism so behavior is documented, not implicit.
- **Testing focus:** Add a regression test where two parallel flows both `variables["x"] = ...` and a fan-in node reads it; assert the value is one of the written values (not asserting *which*).
- **Residual risk:** Minimal once documented.

### TECH-001 — Shadowing confusion
- **Score:** 1 (Minimal)
- **Description:** A `with:` mapping passing kwarg `variables`, or a local `variables = ...` inside the `run:` block, will shadow `engine.variables`.
- **Mitigation:** AC-5 explicitly preserves kwarg precedence; covered by the unit test in Task 2 ("kwarg-override test"). Document the precedence in actions-reference.
- **Residual risk:** Negligible.

### TECH-002 — Cross-node mutation surprises
- **Score:** 1 (Minimal)
- **Description:** Per AC-3, writes are observable in subsequent nodes' Jinja templating — this is intended and matches the existing `extraction_prompt` precedent (`yaml_engine.py:1263`). Could surprise users expecting per-node isolation.
- **Mitigation:** AC-9's "write visible to next node" test makes the semantics executable documentation. Call out the read/write semantics in the docs section.
- **Residual risk:** Negligible — matches existing Jinja behavior.

### SEC-001 — Engine state in exec sandbox
- **Score:** 1 (Minimal)
- **Description:** Exposes `engine.variables` (a mutable dict) into an `exec()` scope.
- **Why minimal:** Python `run:` blocks already run arbitrary code per CLAUDE.md ("Only load YAML from trusted sources"). The trust boundary is the YAML file itself, which already has full Python execution authority. No new attack surface.
- **Mitigation:** None required beyond the existing trust model.
- **Residual risk:** None added.

### OPS-001 — Discoverability
- **Score:** 1 (Minimal)
- **Description:** If AC-10 (docs update) is skipped, the new scope name remains undiscoverable; users continue using the ugly Jinja-into-Python pattern.
- **Mitigation:** Definition of Done already gates on "Docs updated"; reviewer should verify `docs/python/actions-reference.md` is touched in the same PR.
- **Residual risk:** None if DoD enforced.

---

## Risk Distribution

### By Category
- Technical: 3 risks (0 critical/high)
- Security: 1 risk (0 critical/high)
- Operational: 1 risk (0 critical/high)
- Performance: 0 risks
- Data: 0 risks
- Business: 0 risks

### By Component
- `python/src/the_edge_agent/yaml_nodes.py` (single line at L715): all 5 risks
- Documentation (`docs/python/actions-reference.md`): OPS-001

---

## Risk-Based Testing Strategy

### Priority 1 — Must-have (covers ACs 8, 9, plus parallel race)
1. **Read test (AC-8):** YAML with `variables: {x: 5}`; `run:` returns `{"y": variables["x"] + 1}` → state `{"y": 6}`.
2. **Write test (AC-9):** `run:` mutates `variables["new"] = "v"`; subsequent node's Jinja `{{ variables.new }}` resolves to `"v"`.
3. **Kwarg-override test (AC-5):** Node with `with: {variables: {sentinel: true}}` — inside `run:`, `variables["sentinel"]` is `True`, engine variables are *not* visible.
4. **Parallel race documentation test (TECH-006):** Two parallel branches both write `variables["x"]` to different values; fan-in node reads `variables["x"]`; assert it is one of the written values.

### Priority 2 — Regression
- Re-run `python/tests/` full suite — no regressions expected.
- Smoke-run all `examples/yaml/*.yaml` workflows (AC-6) to confirm zero behavior change.

### Priority 3 — Nice to have
- Lua/Prolog runtime test asserting `variables` is *not* injected (AC-7) — protects against the change accidentally bleeding into other runtimes.

---

## Risk Acceptance

### Must Fix Before Merge
- None (no critical/high risks).

### Can Merge with Mitigation
- TECH-006: requires the docs callout describing parallel-write semantics.

### Accepted Risks
- TECH-002 (cross-node mutation): accepted by design per AC-3.

---

## Monitoring Requirements

Post-merge:
- Monitor GitHub issues / discussions for confusion around `variables` vs `state` semantics — recurring questions would signal a docs improvement opportunity.
- No runtime metrics required.

---

## Risk Review Triggers

Re-review this profile if:
- The `exec_globals` injection list grows further (e.g., `secrets`, `data`) — same race-on-mutation question applies.
- Parallel-flow execution model changes (e.g., process-based isolation introduced).
- A YAML sandboxing / least-privilege effort begins — exposure of engine internals to `run:` blocks would need re-evaluation.

---

## Gate YAML Block

```yaml
risk_summary:
  totals:
    critical: 0
    high: 0
    medium: 0
    low: 1
  highest:
    id: TECH-006
    score: 2
    title: 'Parallel-flow race writing to shared engine.variables'
  recommendations:
    must_fix: []
    monitor:
      - 'Confirm AC-10 docs update calls out variables read/write + parallel race semantics'
      - 'Add parallel-flow regression test demonstrating non-deterministic write ordering'
```

Risk profile: docs/qa/assessments/TEA-DX-001.4-risk-20260501.md
