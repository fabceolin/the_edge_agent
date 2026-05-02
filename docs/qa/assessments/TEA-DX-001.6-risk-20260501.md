# Risk Profile: Story TEA-DX-001.6 — `tea validate` workflow command

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Mode: YOLO

## Executive Summary

- Total Risks Identified: 9
- Critical Risks (score 9): 0
- High Risks (score 6): 1
- Medium Risks (score 4): 2
- Low Risks (score 2–3): 4
- Minimal Risks (score 1): 2
- **Overall Risk Score: 72 / 100** (Moderate)
- **Recommended Quality Gate: CONCERNS** (driven by TECH-001; no blocking criticals)

The story is largely additive (a new CLI subcommand) and explicitly rules out runtime side-effects, which keeps security and data risk low. The dominant risk is the refactor of structural validation out of `yaml_engine.py` / `yaml_nodes.py` into a shared module — error-timing shifts in `tea run` are the most likely regression vector. Secondary concerns are best-effort static analysis (false positives in `--strict`) and the trust hazard if checks miss real failures.

## Risk Matrix

| Risk ID  | Description                                                                         | Category | Probability | Impact     | Score | Priority |
| -------- | ----------------------------------------------------------------------------------- | -------- | ----------- | ---------- | ----- | -------- |
| TECH-001 | Refactor changes `tea run` error timing/message phase, breaking existing assertions | TECH     | Medium (2)  | High (3)   | 6     | High     |
| BUS-001  | `--strict` false positives erode user trust in the validator                        | BUS      | Medium (2)  | Medium (2) | 4     | Medium   |
| BUS-002  | Validator gives false confidence — workflow validates but fails at runtime          | BUS      | Medium (2)  | Medium (2) | 4     | Medium   |
| TECH-003 | `--strict` dead-state-key static analysis produces false positives                  | TECH     | High (3)    | Low (1)    | 3     | Low      |
| SEC-001  | Accidental import path triggers `exec()` of `run:` blocks during validation         | SEC      | Low (1)     | High (3)   | 3     | Low      |
| OPS-001  | Engine-init error timing shift breaks downstream test/CI assertions                 | OPS      | High (3)    | Low (1)    | 3     | Low      |
| TECH-002 | Jinja `condition:` parse check misses runtime-only template errors                  | TECH     | Medium (2)  | Low (1)    | 2     | Low      |
| OPS-002  | CI-integration recipe under-documented; teams inconsistently adopt `tea validate`   | OPS      | Low (1)     | Low (1)    | 1     | Minimal  |
| PERF-001 | Slow validation on very large workflows (1000+ nodes) on cold disk                  | PERF     | Low (1)     | Low (1)    | 1     | Minimal  |

## Critical Risks Requiring Immediate Attention

None. No risk scored 9.

## High Risks (Score 6)

### TECH-001 — Refactor changes `tea run` error timing

**Score: 6 (High)**
**Probability**: Medium — the story explicitly calls this shift out as expected; it *will* happen, and at least some tests in `python/tests/` are likely to assert errors at first node execution (the current behavior).
**Impact**: High — silent regression of user-visible error semantics in the engine path is the most-used code path in TEA. A test-only break is recoverable; a behavior-change in error messages or exit codes for `tea run` users is not.

**Affected Components**:

- `python/src/the_edge_agent/yaml_engine.py` (init path)
- `python/src/the_edge_agent/yaml_nodes.py` (`dynamic_parallel` checks at lines ~1130–1170)
- New `python/src/the_edge_agent/yaml_validation.py`
- Engine test suite

**Mitigation** (preventive + detective):

- Audit existing engine tests for phase-specific error assertions before merge (story already calls this out).
- Keep the shared validator pure-functional — return a `List[ValidationError]`, never raise from inside the validator. The engine wraps results in its existing exception path so wire-format compatibility is preserved.
- Add a regression test that runs every `examples/yaml/*.yaml` through both `tea validate` and `tea run` in a dry mode and asserts identical error sets.
- Snapshot-test current engine error messages before the refactor; diff after.

**Testing Focus**:

- One test per error category at engine init *and* via `tea validate` — confirm both surfaces produce equivalent errors.
- `pytest -k "test_yaml_engine"` and any `test_cli_run.py` baseline before/after.
- Manual smoke: `tea run examples/yaml/*.yaml` against intentionally broken fixtures.

## Medium Risks (Score 4)

### BUS-001 — `--strict` false positives erode trust

**Score: 4 (Medium)**
**Probability**: Medium — static state-key flow analysis in a Jinja-templated DSL with `exec()` blocks is fundamentally over-approximate. False positives on legitimate workflows are likely.
**Impact**: Medium — if `--strict` cries wolf, CI pipelines will disable it, defeating the feature.

**Mitigation** (preventive):

- `--strict` warnings must be off by default; document the false-positive risk in `--help` (story AC-12 already requires this).
- Each `--strict` warning prints a short rationale and a one-line escape hatch (e.g., `# tea-validate: ignore unreferenced-node`).
- Curate a fixture set of known-good workflows; CI runs `tea validate --strict` on them and fails on any new warning.

**Testing Focus**:

- Suite of "tricky-but-valid" YAMLs (state keys set inside `run:` block code, conditions referencing computed keys) — must NOT trip `--strict`.
- Suite of clearly-broken fixtures — MUST trip `--strict`.

### BUS-002 — Validator gives false confidence

**Score: 4 (Medium)**
**Probability**: Medium — the story scopes v1 deliberately narrowly (no semantic check of `run:` block contents). Users may interpret "validate passed" as "workflow will run."
**Impact**: Medium — wasted LLM spend if a "validated" workflow still fails at first node, defeating the story's stated value prop.

**Mitigation** (preventive + corrective):

- `tea validate` success message names what was checked: `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]` (extend AC-2 wording).
- `tea validate --help` must enumerate what is **not** checked (AC-12 already requires limitations docs).
- Document in YAML reference: structural validity ≠ semantic correctness.

**Testing Focus**:

- Test that a structurally valid workflow with a clearly broken `run:` block (e.g., `import does_not_exist`) still passes `tea validate` (this is correct behavior) — but the help/output text makes the limit obvious.

## Low Risks (Score 2–3)

### TECH-003 — `--strict` static analysis precision (Score 3)

State-key tracking through `exec()`-backed `run:` blocks is undecidable in the general case. Keep best-effort; document false-positive risk (story already does, secondary risk note). **Test focus**: scoped fixture suite; do not chase 100% precision.

### SEC-001 — Accidental `run:` block execution (Score 3)

**Probability**: Low — the story explicitly forbids it and the validator should not import the engine's executor. **Impact**: High if it slips — untrusted YAML loaded from a CI matrix could execute attacker-controlled Python.

**Mitigation**:

- Unit test: `tea validate` on a fixture whose `run:` block writes a sentinel file → assert the sentinel file is NOT created.
- Static guard: validator module imports a curated subset (`yaml`, `jinja2`, std-lib only) — no `from .yaml_engine import ...` of executor classes.
- Code-review checklist item: any new import in `yaml_validation.py` is reviewed for side-effect potential.

**Testing Focus**: Sentinel-file fixture, network-call fixture (mock socket and assert no calls), env-secret fixture (assert no LLM client init).

### OPS-001 — Engine init error-timing shift breaks tests (Score 3)

Twinned with TECH-001 — same root cause, but treated separately because the *operational* impact is CI churn rather than user-visible semantics. **Mitigation**: audit + bulk-update tests in the same PR. **Testing focus**: full `pytest python/tests/` green before merge (already a DoD item).

### TECH-002 — Jinja parse vs. runtime errors (Score 2)

`jinja2.Environment.parse()` catches syntax errors but not runtime errors (e.g., `state.missing_key` resolves to undefined, not a parse error). Acceptable v1 limitation. **Test focus**: assert parse-time errors caught (`{{ state.foo }` — missing brace), accept that `{{ state.never_set }}` is not.

## Minimal Risks (Score 1)

### OPS-002 — CI integration adoption (Score 1)

DoD already requires a one-line CI recipe. Low risk if the recipe lands; minimal impact if it doesn't (teams discover the command via `--help`).

### PERF-001 — Large-workflow validation latency (Score 1)

Validator is pure-Python file parsing — even a 1000-node workflow is O(nodes + edges) and well under a second. Not worth pre-optimizing.

## Risk Distribution

### By Category

- Technical (TECH): 3 risks (1 high, 0 medium, 2 low)
- Security (SEC): 1 risk (0 high, 0 medium, 1 low)
- Performance (PERF): 1 risk (minimal)
- Data (DATA): 0 risks
- Business (BUS): 2 risks (0 high, 2 medium)
- Operational (OPS): 2 risks (0 high, 0 medium, 1 low, 1 minimal)

### By Component

- `yaml_validation.py` (new): 4 risks (TECH-001, SEC-001, TECH-002, TECH-003)
- `yaml_engine.py` / `yaml_nodes.py` (refactor): 2 risks (TECH-001, OPS-001)
- `cli.py` (new subcommand): 1 risk (BUS-002)
- Documentation/UX surface: 2 risks (BUS-001, OPS-002)
- Performance envelope: 1 risk (PERF-001)

## Risk-Based Testing Strategy

### Priority 1 — High-Risk Tests (TECH-001)

- **Refactor parity suite**: every `examples/yaml/*.yaml` and every broken fixture run through both `tea validate` and `tea run --dry-run-init` (or first-error path). Assert identical error sets.
- **Engine init regression suite**: snapshot current engine error messages before refactor, diff after.
- **CI gate**: `pytest python/tests/` must be 100% green; explicitly call out any tests touched by the timing shift in the PR description.

### Priority 2 — Medium-Risk Tests (BUS-001, BUS-002)

- **`--strict` false-positive suite**: ≥5 tricky-but-valid YAMLs that must NOT warn (state set inside `run:` blocks, conditional refs to runtime-computed keys, jinja `{% set %}` assignments).
- **`--strict` true-positive suite**: ≥5 clearly-broken YAMLs that MUST warn (unreferenced node, condition references typo'd state key, fan_in pointing nowhere).
- **Output-clarity test**: `tea validate` success message includes "structural checks only" qualifier.
- **`--help` content test**: smoke-test that `--help` enumerates limitations (no semantic check of `run:` block contents).

### Priority 3 — Low/Minimal Tests

- **SEC-001 sentinel test** (1 test, high confidence): YAML with `run:` block that writes `/tmp/tea-validate-sentinel` → assert file absent after `tea validate`.
- **One test per AC-6 error category**: missing required field, undefined edge target, duplicate node name, `dynamic_parallel` mutual-exclusion violation, malformed Jinja in `condition:`. (AC-10 already requires this.)
- **Exit-code tests**: 0 on success, 1 on any error.
- **PERF spot-check**: synthetic 500-node workflow validates in <1s on CI runner (smoke only, no perf gate).

## Risk Acceptance

### Must Fix Before Merge

- TECH-001: refactor parity suite green; no behavior change in `tea run` error messages.
- SEC-001: sentinel-file test green; validator import surface reviewed.

### Acceptable With Mitigation

- BUS-001 / TECH-003: `--strict` false positives accepted *if* documented and `--strict` is opt-in (already the design).
- BUS-002: accepted *if* output and `--help` text are explicit about scope (extend AC-2).
- TECH-002: accepted as documented v1 limitation.

### Accepted

- OPS-002, PERF-001: too low to gate.

## Monitoring & Follow-Up

- After merge, watch CI flake rate on engine tests for two weeks (TECH-001 / OPS-001 leftovers).
- Track issues tagged `tea validate` or `--strict false positive` for BUS-001 signal.
- Consider a TEA-DX-001.6.1 follow-up if `--strict` proves too noisy in the field.

## Risk Review Triggers

Re-run this profile when:

- Engine error-handling path is materially refactored.
- A new YAML feature (new node type, new edge type) is added — extend the validator and re-assess SEC-001.
- `--strict` heuristics are expanded beyond unreferenced-node + dead-state-key.
- A user reports a "validate passed but run failed" scenario (re-evaluate BUS-002 mitigations).

## Gate Mapping

- Highest score: 6 (TECH-001) → **Gate = CONCERNS** (no critical, but a high-impact regression vector demands mitigations land in-PR).
- After mitigations land (parity suite + sentinel test + output-text qualifier): re-evaluate to **PASS**.

## risk_summary (paste into gate file)

```yaml
risk_summary:
  totals:
    critical: 0
    high: 1
    medium: 2
    low: 4
  highest:
    id: TECH-001
    score: 6
    title: 'Refactor changes tea run error timing/phase'
  recommendations:
    must_fix:
      - 'Add refactor parity suite: every example + broken fixture validated through both tea validate and tea run, asserting identical error sets'
      - 'Add SEC-001 sentinel-file test: validate must not exec run: blocks'
      - 'Extend AC-2 success message to qualify "structural checks only"; enumerate non-checks in --help'
    monitor:
      - 'CI flake rate on engine tests for two weeks post-merge (TECH-001/OPS-001 leftovers)'
      - 'User reports of --strict false positives (BUS-001 signal)'
      - 'User reports of "validate passed but run failed" (BUS-002 signal)'
```

---

Risk profile: docs/qa/assessments/TEA-DX-001.6-risk-20260501.md
