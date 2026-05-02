# NFR Assessment: TEA-DX-001.5

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Story: [docs/stories/TEA-DX-001.5-dynamic-parallel-error-messages.md](../../stories/TEA-DX-001.5-dynamic-parallel-error-messages.md)
Mode: YOLO (non-interactive)
Scope assessed: core four (security, performance, reliability, maintainability)

## Summary

- **Security: PASS** — No surface change; ValueError messages contain only the
  YAML node name (already exposed) plus static example/doc-anchor strings.
- **Performance: PASS** — Parse-time-only path; templated string formatting is
  negligible. No runtime hot-path impact.
- **Reliability: PASS** — Exception type contract preserved (AC-7 = `ValueError`).
  AC-6 protects existing valid workflows. Fan_in registry timing (TECH-002)
  must be resolved with explicit tests but does not threaten reliability of
  shipped behavior.
- **Maintainability: CONCERNS** — AC-9 covers per-error-code unit tests, but
  two maintainability-critical safeguards are recommendations only and should
  be lifted into ACs before the story leaves Draft: (a) structural test that
  every error template's example fragment parses as valid YAML (TECH-004), and
  (b) explicit doc-anchor placeholder commitment (OPS-001) since the anchor
  referenced from error messages is owned by a sibling story (TEA-DX-001.8).

## Critical Issues

None. No FAILs.

## Concerns (Maintainability)

1. **Drift between error-template examples and real YAML syntax** (TECH-004)
   - Risk: Templates contain example fragments such as `items: "{{ state.batches }}"`.
     If the YAML schema evolves and templates aren't regenerated, error messages
     mislead authors — exactly the failure mode this story is meant to cure.
   - Fix: Promote the risk-profile recommendation into an AC. Add a parametrized
     unit test that yaml-parses every example fragment in the constants table.

2. **Doc anchor coordination across stories** (OPS-001)
   - Risk: AC-1 requires every error message to include a doc anchor
     (`docs/shared/YAML_REFERENCE.md#dynamic-parallel`). That anchor does not
     exist yet — the doc currently references `dynamic_parallel` only at line 762.
     If TEA-DX-001.8 (full YAML reference rewrite) lands later, every emitted
     error points at a 404-equivalent.
   - Fix: Treat the placeholder anchor as part of this story's DoD. Either land
     the placeholder section in the same PR, or block the merge on TEA-DX-001.8.

## Quick Wins

- Add an example-fragment YAML-parse test: ~30 minutes (parametrized over the
  constants table; one assertion per row).
- Add the doc-anchor placeholder section: ~10 minutes (a stub heading
  `## Dynamic Parallel` in `YAML_REFERENCE.md` at line ~762).
- Promote both into the story's ACs before flipping status to Ready: ~5 minutes
  of editing.

## NFR-by-NFR Detail

### Security — PASS

**Evidence:**
- Error messages emit (a) the node name from `node_config["name"]`, already
  exposed today at `yaml_nodes.py:1143`, (b) static example strings authored by
  TEA developers, and (c) a static doc anchor URL.
- No user-supplied data beyond the node name is interpolated. No secret
  material flows through `node_config` at this layer.
- AC-7 keeps the exception type as `ValueError` — no new error-handling surface
  for callers to mishandle.

**Watch (advisory, not blocking):**
- If the implementer is tempted to dump the full `node_config` into error
  messages for debugging, that could leak tokens/URLs from sibling keys. Keep
  context interpolation explicit and minimal.

### Performance — PASS

**Evidence:**
- All four `raise ValueError` sites execute exclusively at parse time
  (`_create_dynamic_parallel_function`), never on the per-iteration runtime
  path inside `run_dynamic_parallel` (`yaml_nodes.py:1196+`).
- Constants-table lookup + format-string substitution is sub-microsecond and
  occurs at most once per dynamic_parallel node per graph load.
- No new I/O, no new dependencies (AC-10), no caching changes.

**No targets defined** — acceptable. Parse-time error formatting is not a
latency-sensitive surface; CONCERNS would be inappropriate here.

### Reliability — PASS

**Evidence:**
- AC-6: existing valid workflows must continue to work — explicit regression
  protection.
- AC-7: error type unchanged — caller `except ValueError` blocks unaffected.
- The story improves *developer-loop* reliability by surfacing actionable
  diagnostics; runtime fault-tolerance behavior is untouched.

**Risk to monitor (TECH-002):**
- AC-4 introduces a new check: `fan_in` references a node that exists. This
  requires access to the declared-node registry. If the check runs *during*
  `_create_dynamic_parallel_function` (i.e., before all nodes are registered),
  legitimate workflows that declare `fan_in` after `dynamic_parallel` will
  raise spurious errors.
- Risk profile already calls for tests covering: (1) fan_in declared before
  dynamic_parallel, (2) fan_in declared after, (3) misspelled target with
  candidate-list assertion. These tests are sufficient to catch regression but
  must actually be implemented — track on the gate.

### Maintainability — CONCERNS

**Evidence supporting CONCERNS:**
- AC-9 covers per-error-code unit tests, which is the right floor.
- However, two safeguards remain in the Risk Profile as "monitor only" rather
  than acceptance criteria:
  - TECH-004: structural test that example fragments parse as YAML.
  - OPS-001: doc anchor placeholder presence.
- Without those promoted to ACs, the story can ship green tests while error
  messages still mislead authors (TECH-004) or point at non-existent anchors
  (OPS-001).

**Positive signals:**
- Constants-table refactor pattern (Technical Notes) keeps call sites thin and
  one-source-of-truth.
- TECH-003 mitigation (stable helper signature) preserves portability into the
  future `yaml_validation.py` module owned by TEA-DX-001.6.
- `python/tests/` test discipline is established; AC-9's "one test per error
  code" maps cleanly to existing patterns.

## Acceptance Criteria for NFR Closure

To move maintainability from CONCERNS → PASS, add the following to the story
before flipping Status from Draft to Ready:

1. **AC-11 (proposed):** A parametrized test loads each example fragment from
   the error-template constants table and asserts it parses as valid YAML
   (`yaml.safe_load`).
2. **AC-12 (proposed):** A placeholder `## Dynamic Parallel` heading exists in
   `docs/shared/YAML_REFERENCE.md` such that the anchor `#dynamic-parallel`
   resolves. Test: simple regex assertion in a docs sanity test, or manual DoD
   checkbox.

Optionally:

3. **AC-13 (proposed):** Every error message's first line is a single,
   grep-friendly summary line containing both the node name and the error
   category (mitigates DATA-001 from risk profile, supports log-pipeline
   compatibility).

## Test Recommendations (NFR-driven)

| ID | Layer | Purpose | NFR |
|----|-------|---------|-----|
| NFR-T1 | Unit | Each example fragment in constants table parses via `yaml.safe_load` | Maintainability (TECH-004) |
| NFR-T2 | Unit | Every error message contains the doc-anchor substring | Maintainability (OPS-001) |
| NFR-T3 | Unit | Every error message's first line contains the node name | Maintainability + Reliability (DATA-001) |
| NFR-T4 | Unit | fan_in declared before dynamic_parallel → no error | Reliability (TECH-002) |
| NFR-T5 | Unit | fan_in declared after dynamic_parallel → no error | Reliability (TECH-002) |
| NFR-T6 | Unit | fan_in misspelled → error includes candidate list | Reliability + Maintainability |
| NFR-T7 | Integration | Existing valid `dynamic_parallel` examples in `examples/` execute end-to-end unchanged | Reliability (AC-6) |
| NFR-T8 | Docs | Anchor `#dynamic-parallel` resolves in `YAML_REFERENCE.md` | Maintainability (OPS-001) |

## Quality Score

```
quality_score = 100
  - 0  FAILs (× 20)
  - 10 CONCERNS (Maintainability × 10)
  = 90 / 100
```

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'No surface change; ValueError messages carry only node name + static templates'
  performance:
    status: PASS
    notes: 'Parse-time-only path; constants-table lookup is sub-µs'
  reliability:
    status: PASS
    notes: 'AC-6/AC-7 preserve existing contract; track TECH-002 fan_in registry timing via tests'
  maintainability:
    status: CONCERNS
    notes: 'Promote example-fragment YAML-parse test (TECH-004) and doc-anchor placeholder (OPS-001) into ACs'
```

---

NFR assessment: docs/qa/assessments/TEA-DX-001.5-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.5-dynamic-parallel-error-messages.yml under nfr_validation
