# Risk Profile: Story TEA-DX-001.5

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: docs/stories/TEA-DX-001.5-dynamic-parallel-error-messages.md
Mode: YOLO (rapid pass)

## Executive Summary

- Total Risks Identified: 7
- Critical Risks: 0
- High Risks: 0
- Medium Risks: 2
- Low Risks: 5
- Risk Score: 90/100 (low overall risk)

This is a low-risk, developer-experience story that rewrites four `ValueError`
messages in `python/src/the_edge_agent/yaml_nodes.py` (`_create_dynamic_parallel_function`)
and adds one new validation check. No runtime semantics, control flow, or public
types change. The dominant failure mode is breaking existing tests that assert
exact error-message substrings.

## Critical Risks Requiring Immediate Attention

None.

## Risk Matrix

| Risk ID  | Description                                                            | Probability | Impact     | Score | Priority |
| -------- | ---------------------------------------------------------------------- | ----------- | ---------- | ----- | -------- |
| TECH-001 | Existing tests assert old exact error strings — will break on rewrite  | High (3)    | Low (1)    | 3     | Low      |
| TECH-002 | `fan_in` node-existence check needs access to declared-node registry   | Medium (2)  | Medium (2) | 4     | Medium   |
| TECH-003 | Helper-function placement diverges from TEA-DX-001.6 (`tea validate`)  | Medium (2)  | Low (1)    | 2     | Low      |
| TECH-004 | Message templates drift from real syntax as YAML schema evolves        | Low (1)     | Medium (2) | 2     | Low      |
| OPS-001  | Doc anchor URL points at section that doesn't exist yet (DX-001.8 dep) | Medium (2)  | Low (1)    | 2     | Low      |
| BUS-001  | New messages still fail to help if examples are too generic/abstract   | Low (1)     | Medium (2) | 2     | Low      |
| DATA-001 | Multi-line / wrapped messages break log-parsing pipelines downstream   | Low (1)     | Medium (2) | 2     | Low      |

## Risk Distribution

### By Category

- Technical (TECH): 4 risks (0 critical, 1 medium)
- Operational (OPS): 1 risk (0 critical)
- Business (BUS): 1 risk (0 critical)
- Data (DATA): 1 risk (0 critical)
- Security (SEC): 0 risks
- Performance (PERF): 0 risks

### By Component

- `yaml_nodes.py::_create_dynamic_parallel_function`: 5 risks
- Test suite (`python/tests/`): 1 risk (TECH-001)
- Docs (`docs/shared/YAML_REFERENCE.md`): 1 risk (OPS-001)

## Detailed Risk Register

### TECH-001 — Existing tests assert old exact error strings

**Score: 3 (Low)**
**Probability**: High — substring asserts on `"requires 'items' expression"`,
`"requires exactly one of"`, `"requires 'fan_in' target node"`, and
`"max_concurrency must be"` are likely already present in the test suite, given
the AC mapping in the original implementation (lines 1141, 1156, 1161, 1170).
**Impact**: Low — purely a test-fixture update; no production behavior at risk.
**Mitigation**:
- Pre-merge: `grep -RIn "dynamic_parallel node" python/tests/` and update each
  match to the new assertion shape (helper-style: assert *parts* — node name +
  doc anchor + presence of example — rather than full strings).
- Add per-error-code unit tests (Task 4 / AC-9) using stable, narrow asserts.
**Testing Focus**: Run full `pytest python/tests/` before pushing; expect a
short triage of fixture updates rather than logic regressions.

### TECH-002 — `fan_in` node-existence check needs access to declared-node registry

**Score: 4 (Medium)**
**Probability**: Medium — AC-4 introduces a *new* check ("fan_in target 'X' is
not a defined node — declared nodes: [...]") that requires knowing the set of
all declared node names at validation time. `_create_dynamic_parallel_function`
is currently called per-node and may not have visibility into siblings.
**Impact**: Medium — getting the timing wrong could either (a) raise spurious
"unknown fan_in" errors when the fan_in node is declared later in the YAML, or
(b) silently miss invalid references because validation runs before the
registry is populated.
**Mitigation**:
- Defer the existence check until after all nodes are registered (post-pass
  validation), or pass a "known node names" snapshot/closure into the
  per-node validator.
- Add tests for: (i) fan_in declared *before* dynamic_parallel, (ii) fan_in
  declared *after*, (iii) fan_in misspelled, (iv) fan_in matches a
  reserved/special name (`__end__`).
**Testing Focus**: Order-of-declaration test cases; round-trip with a real
multi-node YAML fixture.

### TECH-003 — Helper-function placement diverges from TEA-DX-001.6

**Score: 2 (Low)**
**Probability**: Medium — story explicitly flags the coordination point
("If TEA-DX-001.6 lands first, those validators should reuse this helper").
**Impact**: Low — at worst a cosmetic refactor when the second story lands.
**Mitigation**:
- Define the helper with a stable signature and module-private scope so it can
  be lifted into a future `yaml_validation.py` without breaking callers.
- Avoid embedding it inside the `YAMLEngine` class; keep it a module-level
  function so it's importable.
**Testing Focus**: None additional; design review only.

### TECH-004 — Message templates drift from real syntax

**Score: 2 (Low)**
**Probability**: Low — change cadence on `dynamic_parallel` keys is low.
**Impact**: Medium — wrong examples in error messages would actively mislead
authors, exactly the failure mode this story exists to fix.
**Mitigation**:
- Co-locate the templates table with the validator so reviewers naturally
  update both.
- One unit test per error code asserts the example fragment is present and
  parses as valid YAML (cheap structural check, not full execution).
**Testing Focus**: Round-trip a "broken-then-fixed-by-following-the-message"
example for at least one error code as an integration smoke test.

### OPS-001 — Doc anchor URL points at section that doesn't exist yet

**Score: 2 (Low)**
**Probability**: Medium — Task 5 explicitly depends on TEA-DX-001.8.
**Impact**: Low — broken anchor produces a 404 on click; not a runtime failure.
**Mitigation**:
- Land a placeholder `#dynamic-parallel` anchor in `YAML_REFERENCE.md` as part
  of this PR (story already calls this out).
- Anchor format should be a relative repo path (`docs/shared/YAML_REFERENCE.md#dynamic-parallel`)
  so it resolves on GitHub, GitLab, and locally without depending on a hosted
  docs site.
**Testing Focus**: Manual: click the link from a rendered error in a terminal
that supports OSC-8; or grep the docs file for the anchor.

### BUS-001 — New messages still fail to help if examples are too generic

**Score: 2 (Low)**
**Probability**: Low — ACs are concrete (named keys, exact example fragments).
**Impact**: Medium — the *whole point* of the story is reducing
source-grepping; vague examples would defeat the purpose.
**Mitigation**:
- Each example fragment must be copy-pasteable and minimally complete (e.g.,
  `items: "{{ state.batches }}"` not `items: <expression>`).
- Manual DoD check: deliberately break a YAML in each of the five ways and
  verify the message names the fix without source-diving (already in DoD).
**Testing Focus**: Human-readable review; ideally a teammate who hasn't read
the source tries to fix a broken YAML using only the error message.

### DATA-001 — Multi-line / wrapped messages break log-parsing pipelines

**Score: 2 (Low)**
**Probability**: Low — `ValueError` messages are typically surfaced in tracebacks
and CLI output, not structured logs in this project.
**Impact**: Medium — if downstream consumers (CI, log-aggregators) match on
single-line patterns, multi-line messages could split incorrectly.
**Mitigation**:
- Keep the *first line* of every message a complete, grep-able summary
  containing the node name and error code, so single-line tooling still works.
- Subsequent lines (example, doc anchor) are continuation lines.
- AC-8 already constrains to ~80 chars / plain ASCII.
**Testing Focus**: Assert in tests that `str(exc).split("\n")[0]` contains the
node name and the error category.

## Risk-Based Testing Strategy

### Priority 1: Critical Risk Tests
None.

### Priority 2: Medium Risk Tests
- **TECH-002 (fan_in registry)**:
  - fan_in declared before dynamic_parallel → success
  - fan_in declared after dynamic_parallel → success
  - fan_in misspelled (typo of declared name) → new clear error w/ candidate list
  - fan_in references `__end__` / reserved name → defined behavior (allow or reject — pick one and test)

### Priority 3: Low / Standard Tests (one per error code, AC-9)
- Missing `items:` → message includes `items: "{{ state.batches }}"` example
- Zero of {action, steps, subgraph} → message names all three keys
- Two of {action, steps, subgraph} → message names *which* two conflict (AC-2)
- Missing `fan_in:` → message says it goes as sibling of `items:` (AC-3)
- Invalid `max_concurrency` → message shows valid range and example
- Every message contains the doc anchor `docs/shared/YAML_REFERENCE.md#dynamic-parallel`
- Every message's first line contains the node name (single-line grep-ability)
- Existing valid `dynamic_parallel` workflows still execute end-to-end (AC-6 regression)

### Test data requirements
- Five "deliberately broken" YAML fixtures, one per error code
- One golden valid YAML for the regression test
- No new dependencies (AC-10) — use existing pytest fixtures only

## Risk Acceptance Criteria

### Must Fix Before Merge
- All AC-9 unit tests passing
- TECH-001 fixture sweep complete (no stale string matches in `python/tests/`)
- OPS-001 anchor either real or placeholder section landed in same PR

### Can Merge with Mitigation
- TECH-003 helper placement (refactor on TEA-DX-001.6 landing is acceptable)
- TECH-004 template-drift (covered by structural test, not blocking)

### Accepted Risks
- BUS-001 residual subjectivity in "helpful enough" — manual DoD is sufficient

## Monitoring Requirements

Post-merge:
- Watch CI for any flake in newly-added per-error-code tests over the first week
- No production telemetry needed (validation runs at YAML-load time)

## Risk Review Triggers

Re-evaluate this profile when:
- TEA-DX-001.6 (`tea validate`) lands and the helper is lifted to a shared module
- TEA-DX-001.8 (YAML reference docs) rewrites the `#dynamic-parallel` anchor
- The `dynamic_parallel` key set changes (new mode, deprecated key, renamed field)

## Gate Mapping

Per deterministic mapping (max risk score = 4):
- No score ≥ 9 → not FAIL
- No score ≥ 6 → not CONCERNS
- → **Gate: PASS** (with one Medium item to track: TECH-002)

## risk_summary (paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 0
    medium: 1
    low: 6
  highest:
    id: TECH-002
    score: 4
    title: "fan_in node-existence check needs access to declared-node registry"
  recommendations:
    must_fix:
      - "Sweep python/tests/ for old dynamic_parallel error-string asserts before merge (TECH-001)"
      - "Decide fan_in validation timing: post-pass over registry vs. closure-injected snapshot (TECH-002)"
      - "Land placeholder #dynamic-parallel anchor in YAML_REFERENCE.md in same PR (OPS-001)"
    monitor:
      - "Coordinate helper-function shape with TEA-DX-001.6 to avoid double-refactor (TECH-003)"
      - "Add structural test that example fragments parse as valid YAML (TECH-004)"
```

---

Risk profile: docs/qa/assessments/TEA-DX-001.5-risk-20260501.md
