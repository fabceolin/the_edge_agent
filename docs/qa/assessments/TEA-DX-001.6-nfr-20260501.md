# NFR Assessment: TEA-DX-001.6

Date: 2026-05-01
Reviewer: Quinn (Test Architect) — `*nfr-assess` (YOLO / non-interactive mode)
Story: [TEA-DX-001.6 — `tea validate` workflow command](../../stories/TEA-DX-001.6-tea-validate-command.md)
Scope assessed: core four (security, performance, reliability, maintainability)
Companion artifact: [risk profile](TEA-DX-001.6-risk-20260501.md)

## Summary

- **Security:** CONCERNS — Validator must run *zero* side-effects (AC-4/AC-5), but SEC-001 (accidental `run:` block exec via stray import) is only mitigated via tests; the import allow-list is not yet codified as an acceptance criterion.
- **Performance:** PASS — Pure-Python file parsing, O(nodes + edges); no hot-path impact. PERF-001 is minimal; a synthetic 500–1000 node spot-check covers the smoke envelope.
- **Reliability:** CONCERNS — Refactor of structural checks out of `yaml_engine.py` / `yaml_nodes.py:1130-1170` (TECH-001) shifts engine error timing; parity-suite is required. BUS-002 (false-confidence: validate passes but `run` fails) needs the success-message scope qualifier promoted into AC-2 wording.
- **Maintainability:** PASS — Extraction of `yaml_validation.py` is a clear modular win; AC-10 covers tests per error category, AC-11 docs, AC-12 `--help`. Reuses TEA-DX-001.5 error formatter — cross-story coupling is documented.

Quality score: **80/100** (100 − 10 × 2 CONCERNS)

## Gate YAML Block

```yaml
# Gate YAML (copy/paste):
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: CONCERNS
    notes: 'AC-4/AC-5 forbid LLM/LTM/secret/network side-effects, but SEC-001 (stray executor import → run: block exec) is only mitigated by a sentinel-file test. Import allow-list not codified as AC. Promote SEC-001 sentinel test + import-allow-list rule to AC-level.'
  performance:
    status: PASS
    notes: 'Pure-Python file parser, O(nodes + edges). PERF-001 minimal — 500-node synthetic workflow validates well under 1s on CI runner. No explicit target needed for v1.'
  reliability:
    status: CONCERNS
    notes: 'TECH-001 — refactor relocates structural checks from yaml_engine.py / yaml_nodes.py into yaml_validation.py; engine init error timing/phase will shift. Parity-suite (every example + broken fixture, both surfaces, identical error sets) is must-fix. BUS-002 — extend AC-2 success message to qualify "structural checks only — run blocks not executed" so validate is not misread as semantic validation.'
  maintainability:
    status: PASS
    notes: 'Refactor extracts validator into a single pure-functional module; AC-10 (tests per error category + --strict), AC-11 (CLI ref + YAML ref section), AC-12 (--help enumerates limitations) all codified. TEA-DX-001.5 coordination spelled out in Technical Notes.'
```

## Critical Issues

None. No FAIL-level NFRs.

## Concerns & Missing Considerations

### Security — Validator side-effect isolation is test-enforced, not contract-enforced (SEC-001)

- **Risk:** AC-4 forbids LLM-client init / secret fetch / LTM open / `run:` block exec. AC-5 forbids API-key / external-service requirements. Both are stated as runtime invariants but not as a structural constraint on the validator's import graph. A future contributor adding `from .yaml_engine import ExecutorMixin` (for a "shared" helper) would silently violate both ACs — and the only thing catching it would be the SEC-001 sentinel test, if it remains in the suite.
- **Why it matters here:** The story's stated value prop is "catch errors *before* a workflow burns LLM calls" — if `tea validate` ever gains the power to make those calls, the value prop inverts (and worse, untrusted YAML in a CI matrix becomes an arbitrary-code-execution vector).
- **Fix (preferred):** Add **AC-13** codifying the import allow-list (see *Recommended AC additions* below). Add **AC-14** promoting the SEC-001 sentinel test from risk-profile P3 to a required test asset. Cost: ~30 min for the test + a code-review checklist line.
- **Fix (minimum):** Add the sentinel test as a required AC even if the import allow-list isn't formalized — it's the last line of defense.

### Reliability — Refactor parity suite is must-fix but not yet an AC (TECH-001)

- **Risk:** Story explicitly anticipates the error-timing shift from "first node execution" to "engine init" as *desirable*, but tests in `python/tests/` may assert errors at specific phases. The story DoD says `pytest python/tests/` must be green and that `tea run` "continues to surface the same errors at init time (no regression)" — but doesn't require the **parity test asset itself** (every example + every broken fixture, both surfaces, identical error sets) be added in the same PR.
- **Why it matters here:** Without the parity asset committed to the suite, the next person to refactor either path can drift the two surfaces apart with no failing test. The mitigation only sticks if it's mechanized.
- **Fix:** Add **AC-15** (see below) requiring a parity test asset that runs every `examples/yaml/*.yaml` and every broken fixture through both `tea validate` and the engine-init error path, asserting identical `List[ValidationError]` (or equivalent normalized form). Cost: ~2h to wire up; reuses fixtures already implied by AC-10.

### Reliability — Success message must name what was checked (BUS-002)

- **Risk:** AC-2 specifies success output as `OK: <workflow.yaml> (N nodes, M edges)`. A user reading just that will assume "validate passed → run will succeed." When their `run:` block has `import does_not_exist` and the workflow then burns LLM calls before failing, the validator is blamed.
- **Why it matters here:** The story's value-prop is exactly "catch errors before LLM spend." A misread on what `validate` covers re-creates the very pain point this story exists to fix.
- **Fix:** Edit AC-2 to: `OK: <workflow.yaml> (N nodes, M edges) [structural checks only — run blocks not executed]`. AC-12 already requires `--help` to enumerate limitations; align AC-2 with it. Cost: ~15 min including a snapshot test of the success line.

### Maintainability — `--strict` warnings need an escape hatch and a curated CI fixture set (BUS-001 / TECH-003)

- AC-7 introduces `--strict` (unreferenced nodes + dead-state-key warnings). Static analysis of state-key flow through `exec()`-backed `run:` blocks is over-approximate — false positives are likely. Without an inline ignore mechanism (`# tea-validate: ignore unreferenced-node`) and a curated-known-good CI fixture suite, teams will disable `--strict` once they hit the first bad warning.
- Already enumerated in the risk profile under BUS-001 / TECH-003. **Acceptable** v1 with documented mitigation, but the inline-ignore comment and curated fixture suite should be planned (not necessarily required in this PR) — see *Acceptance Criteria — Recommendations* below.

### Reliability — Jinja parse vs. runtime error scope (TECH-002)

- AC-6 says `condition:` expressions parse as valid Jinja. `jinja2.Environment.parse()` catches syntax errors (unbalanced braces, malformed filters) but **not** runtime undefineds (`{{ state.never_set }}` parses fine). This is the correct v1 scope but should be explicitly named in `--help` so users don't expect e.g. type-checking. AC-12 covers this with "describes the checks and limitations" — confirm the `--help` text enumerates *Jinja syntax only, not runtime semantics* explicitly.

## Test Recommendations

Risk-profile P0–P3 already enumerates the test slate. NFR-specific must-haves and promotions:

| #  | NFR             | Test                                                                                                     | Priority                              |
| -- | --------------- | -------------------------------------------------------------------------------------------------------- | ------------------------------------- |
| T1 | Security        | Sentinel-file: fixture `run:` block writes `/tmp/tea-validate-sentinel-<uuid>`; assert NOT created       | **P0** (promote from risk-profile P3) |
| T2 | Security        | Network sentinel: monkeypatch `socket.socket` and assert no calls during `tea validate`                  | P1                                    |
| T3 | Security        | Env-secret sentinel: assert `os.environ["ANTHROPIC_API_KEY"]` is unread (or pop it before invoke)        | P1                                    |
| T4 | Reliability     | Parity suite: every `examples/yaml/*.yaml` + each broken fixture → both surfaces → identical error sets  | **P0** (currently risk-profile P1)    |
| T5 | Reliability     | Engine-init snapshot: capture pre-refactor error messages; diff post-refactor; zero unintended diff      | P0                                    |
| T6 | Reliability     | Success-message snapshot: success line includes `[structural checks only — run blocks not executed]`     | P1                                    |
| T7 | Reliability     | One test per AC-6 error category (already required by AC-10) — confirm exit code 1, message format       | P0                                    |
| T8 | Reliability     | Exit-code coverage: 0 on success, 1 on any error, 1 even when only `--strict` warnings exist             | P1                                    |
| T9 | Maintainability | `tea validate --help` snapshot enumerates *what's not checked* (run-block semantics, runtime undefineds) | P1                                    |
| T10 | Maintainability | `--strict` false-positive suite: ≥5 tricky-but-valid YAMLs that must NOT warn                            | P1 (already in risk profile)          |
| T11 | Maintainability | `--strict` true-positive suite: ≥5 broken YAMLs that MUST warn                                           | P1 (already in risk profile)          |
| T12 | Performance     | Synthetic 500-node workflow validates in <1s on CI runner (smoke only, no perf gate)                     | P3                                    |

## Acceptance Criteria — Recommendations

Add these to close the two CONCERNS NFRs:

> **AC-13 (Security — import allow-list):** `python/src/the_edge_agent/yaml_validation.py` must not import any executor-related module from the package — specifically not `yaml_engine.YAMLEngine`, `yaml_nodes._create_*` execution functions, `cli`, `parallel*`, `checkpointers`, or any `actions.*` / `backends.*` module. Allowed package imports: `yaml_config` (for the parsed-config dataclasses only) and the error-formatting helper from TEA-DX-001.5. A unit test inspects `sys.modules` after `import yaml_validation` and asserts none of the disallowed modules were transitively loaded.
>
> **AC-14 (Security — sentinel-file test):** Add a fixture YAML whose `run:` block writes a uniquely-named sentinel file. Test invokes `tea validate <fixture>` and asserts the sentinel file is NOT created and the validator returns success (the YAML is structurally valid; the side-effect is the *forbidden* behavior). Mark this test as a security-critical regression — failure should not be allowed to autofix.
>
> **AC-15 (Reliability — refactor parity suite):** Add a test asset that, for every `examples/yaml/*.yaml` and every broken fixture in the test suite, runs the YAML through both `tea validate` (CLI) and the engine-init validation path, asserts the two produce identical normalized `List[ValidationError]` (same codes, same offending nodes, same line/col where available). This asset is the on-going regression guard for TECH-001 / OPS-001.
>
> **AC-2 (revised wording):** Output on success: `OK: <workflow.yaml> (N nodes, M edges) [structural checks only — run blocks not executed]` and exit 0. The bracketed qualifier must appear verbatim and is asserted by a snapshot test.

Optional, planned for follow-up (not required in this PR):

> **AC-16 (Maintainability — `--strict` ignore comments):** Each `--strict` warning category supports an inline ignore directive (e.g., `# tea-validate: ignore unreferenced-node` placed on the offending line or the node block). Validator parses these as YAML comments and suppresses the matching warning. Documented in `--help` and YAML reference.
>
> **AC-17 (Maintainability — `--strict` CI fixture set):** A curated `python/tests/fixtures/strict_known_good/*.yaml` directory contains representative valid workflows. CI runs `tea validate --strict` against the directory and fails on any new warning, gating false-positive regressions on `--strict` heuristic changes.

## Quick Wins

- Edit AC-2 success-message text (15 min) → closes BUS-002 documentation gap
- Add SEC-001 sentinel-file test (30 min) → closes Security CONCERNS lower bound
- Add `sys.modules` allow-list test for `yaml_validation.py` imports (30 min) → makes Security CONCERNS structurally enforced rather than convention
- Snapshot `tea validate --help` content with explicit "not checked" enumeration (15 min) → closes T9, AC-12 mechanically enforced
- Wire up parity suite (~2 h) → resolves TECH-001 reliability concern; reuses fixtures already needed for AC-10

## Sign-off

NFR posture: **CONCERNS** (security + reliability), driven by:
1. Side-effect isolation enforced only by tests, not by import contract (SEC-001).
2. Refactor parity suite + success-message scope qualifier required to land in-PR (TECH-001 + BUS-002).

Re-evaluate to **PASS** once AC-13, AC-14, AC-15, and revised AC-2 wording land in the story and have implementing tests in the PR. The story is safe to implement; these are pre-merge gates, not blockers to start.

NFR assessment: docs/qa/assessments/TEA-DX-001.6-nfr-20260501.md

Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.6-tea-validate-command.yml under `nfr_validation`
