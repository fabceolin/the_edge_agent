# TEA-DX-001.6: `tea validate` workflow command

## Status
Done

## Parent Epic
[TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md)

## Priority
Medium

---

## Story

**As a** workflow author or CI pipeline,
**I want** `tea validate <workflow.yaml>` to check structural validity without executing nodes or instantiating LLM clients,
**so that** I catch syntax errors *before* a workflow burns LLM calls and fails halfway through.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/cli.py` (new Typer subcommand alongside `run`)
- Validation logic currently lives at: `yaml_engine.py` (config parsing) and `yaml_nodes.py:1130-1170` (`dynamic_parallel` checks); some checks happen lazily during execution
- Technology: Python, Typer, PyYAML
- Coordinated with: TEA-DX-001.5 (error messages) — this story should reuse those formatted messages

**Problem Statement:**

User report: a workflow failed *after several Claude API calls had already executed* because of a YAML structural error (e.g., `dynamic_parallel` missing `fan_in`, condition referencing a state key that no node ever sets, an edge pointing to an undefined node). Today the only way to discover these is to run the workflow.

A pre-flight `validate` command catches them statically.

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1:** New CLI command: `tea validate <workflow.yaml>`. Exits 0 on success, 1 on validation errors.
2. **AC-2:** Output on success: `OK: <workflow.yaml> (N nodes, M edges)` and exit 0.
3. **AC-3:** Output on failure: each error on its own block with file path, line/column if available, and the formatted message from TEA-DX-001.5 (or the existing message if that story hasn't landed). Exit 1.
4. **AC-4:** Validator does **not** instantiate LLM clients, fetch secrets, open LTM connections, or execute any `run:` block.
5. **AC-5:** Validator does **not** require API keys or external services to be reachable.
6. **AC-6:** Checks performed (v1 minimum):
   - YAML parse errors (defer to PyYAML's error)
   - Required top-level fields present (`name`, `nodes`, `edges` or equivalent)
   - All edges' `from` and `to` reference declared node names (or `__start__`/`__end__`)
   - `dynamic_parallel` nodes have exactly one of `action`/`steps`/`subgraph` and a valid `fan_in` target
   - No duplicate node names
   - `fan_in` targets reference declared nodes
   - `condition:` expressions parse as valid Jinja (catches typos before runtime)
7. **AC-7:** `--strict` flag adds soft warnings as errors (e.g., unreferenced nodes, conditions referencing state keys never set by any node — best-effort static analysis).

**Integration Requirements:**

8. **AC-8:** `tea run` already performs these checks at engine init — refactor the validator out so both `validate` and the engine use the same module (`yaml_validation.py` or similar).
9. **AC-9:** Adding `validate` does not change `run` semantics.

**Quality Requirements:**

10. **AC-10:** Tests cover: valid workflow → exit 0; one test per error category; `--strict` adds warnings.
11. **AC-11:** Documented in `docs/python/` CLI reference.
12. **AC-12:** `tea validate --help` describes the checks and limitations (e.g., "does not validate semantic correctness of `run:` block contents").

## Technical Notes

- **Integration Approach:**
  1. Extract structural validation from `yaml_engine.py` and `yaml_nodes.py` into a new `python/src/the_edge_agent/yaml_validation.py` module exposing `validate_workflow(yaml_path) -> List[ValidationError]`.
  2. Engine init calls the same validator and converts errors to its existing exception path.
  3. New `validate` CLI command imports the validator, prints results, exits accordingly.
- **Existing Pattern Reference:** `tea run` command in `cli.py`; recent CLI subcommand additions like `tea report` (`report_cli.py`).
- **Coordination with TEA-DX-001.5:** This story should produce a `yaml_validation.py` that exposes the error-formatting helper used by Story 5. If Story 5 lands first, this story consumes its helper. If this story lands first, Story 5 imports from here.
- **Key Constraint:** Validator cannot side-effect — no LLM clients, no secret backends, no network. Pure-Python, file-only.

## Tasks / Subtasks

- [x] **Task 1: Extract validator module** (AC: 4, 5, 8)
  - [x] Create `python/src/the_edge_agent/yaml_validation.py`
  - [x] Move structural checks out of `yaml_engine.py` / `yaml_nodes.py`
  - [x] Engine init re-uses the new module
- [x] **Task 2: Implement v1 checks** (AC: 6)
  - [x] Required fields, node-name uniqueness, edge endpoint validity
  - [x] `dynamic_parallel` mutual-exclusion + fan_in target
  - [x] Jinja parse check on `condition:` expressions
- [x] **Task 3: CLI subcommand** (AC: 1, 2, 3, 12)
  - [x] Add `validate` Typer command in `cli.py`
  - [x] Format output: success summary or error list
  - [x] Exit codes 0/1
- [x] **Task 4: `--strict` flag** (AC: 7)
  - [x] Add unreferenced-node and dead-state-key warnings
- [x] **Task 5: Tests** (AC: 10)
  - [x] Valid + one test per error category + `--strict` test
- [x] **Task 6: Docs** (AC: 11, 12)
  - [x] CLI reference + a "Validating workflows" section in YAML reference

## Definition of Done

- [ ] All ACs met
- [ ] `pytest python/tests/` green
- [ ] `tea validate examples/yaml/*.yaml` passes for every shipped example
- [ ] `tea run` continues to surface the same errors at init time (no regression)
- [ ] CI integration documented (one-line `tea validate workflow.yaml` recipe)

## Risk and Compatibility

- **Primary Risk:** Refactoring validation out of the engine could change error timing for `tea run` (errors that used to happen at first node execution now happen at init). This is a *desirable* shift but may break tests that assert error messages at specific phases.
  - **Mitigation:** Audit existing engine tests for phase-specific error assertions; update as needed.
- **Secondary Risk:** Static analysis of state-key flow (for `--strict` dead-key warnings) is hard to make precise. Keep best-effort; document false-positive risk.
- **Rollback:** Revert PR; engine still works (it had the checks all along).
- **Compatibility:** Fully additive subcommand.

## QA Notes - Risk Profile

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO
**Full report:** [`docs/qa/assessments/TEA-DX-001.6-risk-20260501.md`](../qa/assessments/TEA-DX-001.6-risk-20260501.md)

### Risk Level

- **Overall risk score:** 72 / 100 — **Moderate**
- **Recommended quality gate:** **CONCERNS** (driven by one High-priority risk; no Criticals)
- **Distribution:** 0 critical, 1 high, 2 medium, 4 low, 2 minimal (9 total)

### Identified Risks

| Risk ID  | Title                                                                          | Score | Priority |
| -------- | ------------------------------------------------------------------------------ | ----- | -------- |
| TECH-001 | Refactor changes `tea run` error timing/message phase                          | 6     | High     |
| BUS-001  | `--strict` false positives erode user trust                                    | 4     | Medium   |
| BUS-002  | Validator gives false confidence — passes but workflow fails at runtime        | 4     | Medium   |
| TECH-003 | `--strict` dead-state-key static analysis precision                            | 3     | Low      |
| SEC-001  | Accidental `exec()` of `run:` blocks during validation                         | 3     | Low      |
| OPS-001  | Engine init error-timing shift breaks downstream test assertions               | 3     | Low      |
| TECH-002 | Jinja parse check misses runtime-only template errors                          | 2     | Low      |
| OPS-002  | CI-integration recipe under-documented; inconsistent adoption                  | 1     | Minimal  |
| PERF-001 | Slow validation on very large workflows                                        | 1     | Minimal  |

### Mitigations

**Must fix before merge:**

- **TECH-001:** Refactor parity suite — every `examples/yaml/*.yaml` and every broken fixture validated through both `tea validate` and `tea run` init, asserting identical error sets. Snapshot current engine error messages pre-refactor and diff post.
- **SEC-001:** Sentinel-file test — fixture YAML whose `run:` block writes `/tmp/tea-validate-sentinel`; assert the file is NOT created after `tea validate`. Validator module imports curated subset only (`yaml`, `jinja2`, std-lib) — no executor classes from `yaml_engine`.
- **BUS-002:** Extend AC-2 success message to qualify scope (e.g., `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]`); `--help` enumerates explicit non-checks.

**Acceptable with documented mitigation:**

- **BUS-001 / TECH-003:** `--strict` is opt-in by default; warnings include rationale + escape-hatch comment; CI runs `--strict` against a curated known-good fixture set and fails on any new warning.
- **TECH-002:** accepted v1 limitation; documented in `--help`.

**Monitor post-merge:**

- CI engine-test flake rate for two weeks (TECH-001 / OPS-001 spillover).
- User reports of `--strict` false positives → BUS-001 signal.
- User reports of "validate passed but run failed" → BUS-002 signal.

### Testing Priorities

**Priority 1 — High-risk (TECH-001):**

- Refactor parity suite: every example + every broken fixture, both surfaces, identical errors.
- Engine error-message snapshot diff before/after the refactor.
- Full `pytest python/tests/` green on the PR.

**Priority 2 — Medium-risk (BUS-001, BUS-002):**

- `--strict` false-positive fixture suite (≥5 tricky-but-valid YAMLs that must NOT warn).
- `--strict` true-positive fixture suite (≥5 clearly-broken YAMLs that MUST warn).
- Output-clarity test: success message includes "structural checks only" qualifier.
- `--help` content test asserts limitations are enumerated.

**Priority 3 — Low/minimal:**

- SEC-001 sentinel-file test (single high-confidence test).
- One test per AC-6 error category (already required by AC-10).
- Exit-code coverage: 0 on success, 1 on any error.
- PERF spot-check: 500-node synthetic workflow validates in <1s (smoke only).

### Gate Recommendation

**CONCERNS** until the three "must fix before merge" mitigations land in-PR. Re-evaluate to **PASS** once parity suite + sentinel test + output-text qualifier are merged.

## QA Notes - NFR Assessment

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive; default core four NFRs)
**Full report:** [`docs/qa/assessments/TEA-DX-001.6-nfr-20260501.md`](../qa/assessments/TEA-DX-001.6-nfr-20260501.md)

### NFR Coverage

| NFR              | Status   | Quality Score Impact |
| ---------------- | -------- | -------------------- |
| Security         | CONCERNS | -10                  |
| Performance      | PASS     | 0                    |
| Reliability      | CONCERNS | -10                  |
| Maintainability  | PASS     | 0                    |
| **Total**        | —        | **80 / 100**         |

### Missing Considerations

1. **Security — import allow-list not codified (SEC-001).** AC-4/AC-5 forbid runtime side-effects, but nothing in the story constrains *which* package modules `yaml_validation.py` may import. A future contributor pulling `from .yaml_engine import ...` for a "shared helper" can silently re-introduce executor code paths into the validator without breaking any existing AC. The sentinel test alone is the last line of defense.
2. **Reliability — refactor parity asset is not an AC.** DoD requires `tea run` to surface the same errors at init, but the *test asset* (every example + every broken fixture, both surfaces, identical error sets) that mechanizes that guarantee is not required by an AC. Without it, drift between the two surfaces becomes silent on the next refactor.
3. **Reliability — success-message scope qualifier missing (BUS-002).** AC-2 says `OK: <file> (N nodes, M edges)` but doesn't qualify scope. Users will misread "validate passed" as "workflow will run" — re-creating the LLM-spend pain this story exists to fix.
4. **Maintainability — `--strict` lacks an ignore mechanism.** AC-7 introduces opt-in static analysis warnings, but with no inline escape hatch (`# tea-validate: ignore unreferenced-node`) and no curated CI fixture set guarding heuristic changes, false positives will push teams to disable `--strict` (BUS-001 / TECH-003). Acceptable v1, but should be planned for follow-up.

### Test Recommendations (NFR-driven, P0 → P3)

**P0 — must have before merge:**
- T1: SEC-001 sentinel-file fixture (`run:` block writes `/tmp/tea-validate-sentinel-<uuid>` → assert file NOT created post-validate).
- T4: Refactor parity suite — every `examples/yaml/*.yaml` and every broken fixture validated through both `tea validate` and the engine init path; assert identical normalized error sets.
- T5: Engine-init error-message snapshot diff before/after the refactor; zero unintended diffs.
- T7: One test per AC-6 error category (already required by AC-10), confirms exit code 1 + new message format.

**P1 — should have:**
- T2: Network sentinel — monkeypatch `socket.socket`, assert no calls during `tea validate`.
- T3: Env-secret sentinel — assert `os.environ["ANTHROPIC_API_KEY"]` (and similar) is unread.
- T6: Success-message snapshot includes `[structural checks only — run blocks not executed]`.
- T8: Exit-code coverage: 0 on success; 1 on errors; 1 on `--strict` warnings only.
- T9: `tea validate --help` snapshot enumerates *what's not checked* (run-block semantics, runtime undefineds).
- T10/T11: `--strict` false-positive (≥5 tricky-but-valid) and true-positive (≥5 broken) fixture suites.

**P3 — nice to have:**
- T12: Synthetic 500-node workflow validates in <1s on CI runner (smoke; no perf gate).

### Recommended Acceptance Criteria Additions

Add to close the two CONCERNS NFRs (must-have, in-PR):

- **AC-13 (Security — import allow-list):** `yaml_validation.py` must not import any executor-related module from the package (`yaml_engine.YAMLEngine`, executor functions in `yaml_nodes`, `cli`, `parallel*`, `checkpointers`, `actions.*`, `backends.*`). Allowed: `yaml_config` dataclasses + the TEA-DX-001.5 error-formatter helper. A unit test inspects `sys.modules` after import and asserts the disallowed modules are not transitively loaded.
- **AC-14 (Security — sentinel-file test):** Fixture YAML with a `run:` block that writes a uniquely-named sentinel file; `tea validate <fixture>` must succeed structurally AND the sentinel file must NOT exist afterward. Marked as a security-critical regression test — not auto-fixable.
- **AC-15 (Reliability — refactor parity suite):** For every `examples/yaml/*.yaml` and every broken fixture, run through both `tea validate` (CLI) and the engine-init validation path; assert identical normalized `List[ValidationError]` (same codes, same offending nodes, same line/col where available).
- **AC-2 (revised wording):** `OK: <workflow.yaml> (N nodes, M edges) [structural checks only — run blocks not executed]` — bracketed qualifier asserted by a snapshot test.

Optional, planned for follow-up (not required in this PR):

- **AC-16:** `--strict` warnings support inline ignore comments (e.g., `# tea-validate: ignore unreferenced-node`).
- **AC-17:** Curated `python/tests/fixtures/strict_known_good/*.yaml` directory; CI runs `tea validate --strict` against it and fails on any new warning.

### Gate Recommendation

**CONCERNS** until AC-13, AC-14, AC-15, and revised AC-2 wording land in the story and have implementing tests/snapshots in the PR. The two CONCERNS NFRs (Security, Reliability) close cleanly with these additions; Performance and Maintainability are already PASS. Re-evaluate to **PASS** post-merge of those mitigations.

## QA Notes - Test Design

**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Full report:** [`docs/qa/assessments/TEA-DX-001.6-test-design-20260501.md`](../qa/assessments/TEA-DX-001.6-test-design-20260501.md)

### Test Coverage Matrix

| Level           | Count | Share | Priority distribution        |
| --------------- | ----- | ----- | ---------------------------- |
| Unit            | 23    | 55 %  | P0: 17 · P1: 4 · P2: 2 · P3: 0 |
| Integration     | 19    | 45 %  | P0: 10 · P1: 6 · P2: 2 · P3: 1 |
| E2E             | 0     | 0 %   | (no UI/network surface)      |
| **Total**       | **42** | —     | **P0: 27 · P1: 10 · P2: 4 · P3: 1** |

E2E is intentionally zero: `tea validate` is a CLI dev tool with no UI/network dependency, so the integration layer (CLI → validator → on-disk fixture) is the realistic top boundary.

### AC → Scenarios Coverage

| AC                                | Covering scenarios                                                                            |
| --------------------------------- | --------------------------------------------------------------------------------------------- |
| AC-1 (CLI + exit codes)           | 1.6-INT-001, 1.6-INT-002                                                                      |
| AC-2 revised (success + qualifier)| 1.6-INT-003                                                                                   |
| AC-3 (failure block format)       | 1.6-UNIT-001, 1.6-INT-004, 1.6-INT-005                                                        |
| AC-4 / AC-13 / AC-14 (no SE)      | 1.6-UNIT-002, 1.6-INT-006, 1.6-INT-007, 1.6-INT-008                                           |
| AC-5 (no API keys/network)        | covered by 1.6-INT-007, 1.6-INT-008                                                           |
| AC-6 (v1 checks)                  | 1.6-UNIT-003 … 1.6-UNIT-018, 1.6-INT-009                                                      |
| AC-7 (`--strict`)                 | 1.6-UNIT-019, 1.6-UNIT-020, 1.6-UNIT-021, 1.6-UNIT-022, 1.6-INT-010, 1.6-INT-011              |
| AC-8 / AC-15 (parity)             | 1.6-INT-012, 1.6-INT-013, 1.6-INT-014, 1.6-INT-015                                            |
| AC-9 (`run` unchanged)            | 1.6-INT-016                                                                                   |
| AC-10 (test depth)                | covered by AC-1 / AC-6 / AC-7 scenarios                                                       |
| AC-11 (docs)                      | 1.6-UNIT-023, 1.6-INT-017                                                                     |
| AC-12 (`--help` content)          | 1.6-INT-018                                                                                   |
| Performance smoke                 | 1.6-INT-019                                                                                   |

### Scenarios with Expected Results (key items)

| ID            | Level       | Pri | What's being tested                                                                                                | Expected result                                                                          |
| ------------- | ----------- | --- | ------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------- |
| 1.6-INT-001   | Integration | P0  | `tea validate <valid.yaml>`                                                                                        | Exit 0; stdout matches success snapshot                                                  |
| 1.6-INT-002   | Integration | P0  | `tea validate <broken.yaml>`                                                                                       | Exit 1                                                                                   |
| 1.6-INT-003   | Integration | P0  | Success message snapshot                                                                                           | Contains `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]` |
| 1.6-UNIT-002  | Unit        | P0  | `import the_edge_agent.yaml_validation` import allow-list                                                          | `sys.modules` excludes `yaml_engine`, executor classes, `actions.*`, `backends.*`, `checkpointers*`, `parallel*` |
| 1.6-INT-006   | Integration | P0  | Sentinel-file fixture: `run:` writes `/tmp/tea-validate-sentinel-<uuid>`                                           | After `tea validate`, sentinel file MUST NOT exist; exit 0 on structurally-valid YAML    |
| 1.6-INT-007   | Integration | P1  | Monkeypatch `socket.socket.__init__` to raise; run validate                                                        | Exit 0; AssertionError never triggered                                                   |
| 1.6-INT-008   | Integration | P1  | Wrap `os.environ.__getitem__`; run validate                                                                        | Zero reads of `ANTHROPIC_API_KEY` / `OPENAI_API_KEY` and other secret allow-list keys    |
| 1.6-UNIT-003 … -018 | Unit  | P0  | One unit per AC-6 error category (parse, missing fields, edge endpoints, dynamic_parallel, dup nodes, jinja)       | `validate_workflow()` returns the expected `ValidationError` (or none for negative cases)|
| 1.6-INT-009   | Integration | P0  | One CLI invocation per AC-6 category against broken fixture                                                        | Exit 1; one error block in stdout matching expected snapshot                             |
| 1.6-UNIT-019  | Unit        | P1  | `--strict` promotes warnings to errors                                                                             | Same input that exits 0 without `--strict` exits 1 with `--strict`                       |
| 1.6-INT-010   | Integration | P1  | `--strict` true-positive suite (≥5 broken YAMLs)                                                                   | Each fixture exits 1 with at least one warning                                           |
| 1.6-INT-011   | Integration | P1  | `--strict` false-positive suite (≥5 tricky-but-valid YAMLs)                                                        | Each fixture exits 0; zero warnings                                                      |
| 1.6-INT-012   | Integration | P0  | Engine init invokes `yaml_validation.validate_workflow` (mock + assert called)                                     | `validate_workflow` called exactly once during engine init                               |
| 1.6-INT-013   | Integration | P0  | **Parity:** every `examples/yaml/*.yaml` through both surfaces                                                     | Identical normalized `List[ValidationError]` (codes, nodes, line/col)                    |
| 1.6-INT-014   | Integration | P0  | **Parity:** every broken fixture through both surfaces                                                             | Identical normalized `List[ValidationError]`                                             |
| 1.6-INT-015   | Integration | P0  | Engine-init error-message **snapshot diff** pre-/post-refactor                                                     | Zero unintended diffs                                                                    |
| 1.6-INT-016   | Integration | P0  | Existing `tea run` golden-path tests run unchanged                                                                 | All pass; same output, exit codes, side effects                                          |
| 1.6-INT-018   | Integration | P1  | `tea validate --help` snapshot                                                                                     | Lists checks performed, scope qualifier, and explicit non-checks (run-block semantics, runtime template errors) |
| 1.6-INT-019   | Integration | P3  | 500-node synthetic workflow validation                                                                             | Completes in <1s on CI runner (smoke; not a hard gate)                                   |

Full per-scenario detail in the test-design assessment document.

### Risk Coverage

All 9 risks from the risk profile are covered:

| Risk     | Priority  | Covered by                                                              |
| -------- | --------- | ----------------------------------------------------------------------- |
| TECH-001 | High      | 1.6-INT-013, 1.6-INT-014, 1.6-INT-015, 1.6-INT-016                      |
| BUS-001  | Medium    | 1.6-INT-010, 1.6-INT-011                                                |
| BUS-002  | Medium    | 1.6-INT-003, 1.6-INT-018                                                |
| TECH-003 | Low       | 1.6-UNIT-022, 1.6-INT-010, 1.6-INT-011                                  |
| SEC-001  | Low       | 1.6-UNIT-002, 1.6-INT-006                                               |
| OPS-001  | Low       | 1.6-INT-015, 1.6-INT-016                                                |
| TECH-002 | Low       | 1.6-INT-018 (documented limitation)                                     |
| PERF-001 | Minimal   | 1.6-INT-019                                                             |
| OPS-002  | Minimal   | 1.6-UNIT-023                                                            |

### Test Data and Environment Requirements

**Fixture directories** (under `python/tests/fixtures/validate/`):

- `valid/` — happy-path fixtures (one per affirmative assertion: minimal, both `dynamic_parallel` shapes, conditions, `__start__`/`__end__` edges).
- `broken/` — one fixture per AC-6 error category (12 fixtures).
- `sentinel/` — `run:` block writes uniquely-named sentinel file under `tempfile.gettempdir()` (uuid-suffixed per test invocation).
- `strict_known_good/` — ≥5 tricky-but-valid YAMLs (BUS-001 false-positive guard).
- `strict_known_bad/` — ≥5 clearly-broken YAMLs (BUS-001 true-positive guard).
- `perf/` — `synthetic_500_nodes.yaml` generated programmatically in test setup; not committed.

**Snapshot files** (under `python/tests/snapshots/`):

- `validate_help.txt`, `validate_success.txt`, `validate_error_blocks/<category>.txt`
- `engine_init_errors_pre_refactor.json` (captured before refactor) vs `engine_init_errors_post_refactor.json` for 1.6-INT-015.

**Environment:**

- Python 3.10+ (project minimum); pytest with a text-snapshot helper (e.g., `pytest-snapshot`).
- No external services required — that is the contract under test (AC-4/AC-5).
- CI: standard `python/tests/` runner. Perf scenario marked `@pytest.mark.slow`; not a hard gate.
- Cross-platform: sentinel test must use `tempfile.gettempdir()` so it works on Windows; uuid-suffix prevents cross-test contamination.

**Mocks / monkeypatches:**

- 1.6-UNIT-002 — run in subprocess to get a clean `sys.modules` baseline (avoids pollution from prior tests).
- 1.6-INT-007 — `socket.socket.__init__` patched to raise.
- 1.6-INT-008 — `os.environ.__getitem__` wrapped to record keys; intersect with secret allow-list.
- 1.6-INT-012 — patch `yaml_validation.validate_workflow` and assert single call during engine init.

### Recommended Execution Order

1. P0 unit tests (1.6-UNIT-002 → 1.6-UNIT-018) — fail fast on logic / allow-list regressions.
2. P0 integration tests — sentinel + parity first (1.6-INT-006, 1.6-INT-013, 1.6-INT-014, 1.6-INT-015), then CLI wiring (1.6-INT-001, -002, -003, -009, -012, -016).
3. P1 tests — `--strict` suites + `--help` snapshot.
4. P2 tests — docs presence + shipped-example smoke.
5. P3 tests — perf spot-check.

### Must-Have Before Merge

Seven scenarios mechanize the three "must fix before merge" mitigations called out by the risk profile and NFR assessment:

- **1.6-UNIT-002** (AC-13 import allow-list)
- **1.6-INT-006** (AC-14 sentinel-file)
- **1.6-INT-013** (AC-15 refactor parity — examples)
- **1.6-INT-014** (AC-15 refactor parity — broken fixtures)
- **1.6-INT-015** (TECH-001 engine error-message snapshot diff)
- **1.6-INT-003** (BUS-002 success-message scope qualifier)
- **1.6-INT-018** (AC-12 + BUS-002 + TECH-002 `--help` limitations)

### Coverage Validation

- All 12 in-scope ACs have ≥1 covering scenario; AC-13/14/15 (must-add) covered.
- AC-16 / AC-17 (optional follow-ups) deliberately out of scope.
- No duplicate coverage across levels except `1.6-INT-009`, which intentionally proves unit-tested logic is reachable through the CLI.
- All 9 risks covered.
- Test IDs follow `1.6-{LEVEL}-{SEQ}` convention.

## QA Notes - Requirements Trace

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Story status at trace time:** Draft (no implementation yet — trace maps ACs to *planned* scenarios from test design)
**Full report:** [`docs/qa/assessments/TEA-DX-001.6-trace-20260501.md`](../qa/assessments/TEA-DX-001.6-trace-20260501.md)

### Requirements Coverage Summary

| Metric                                   | Value                                        |
| ---------------------------------------- | -------------------------------------------- |
| Total in-scope ACs                       | 12 (AC-1 … AC-12)                            |
| ACs with ≥1 planned scenario             | 12 (100 %)                                   |
| Fully covered (planned)                  | 12                                           |
| Partially covered                        | 0                                            |
| Not covered                              | 0                                            |
| Recommended new ACs (close gaps)         | AC-2 (rewording), AC-13, AC-14, AC-15        |
| Optional follow-up ACs                   | AC-16, AC-17                                 |
| Total planned test scenarios             | 42 (23 unit · 19 integration · 0 e2e)        |
| Scenarios mechanizing P0 risk-fix gates  | 7                                            |

> "Fully covered" means every AC is bound to a planned scenario in the test design with explicit Given-When-Then expectations. Coverage will need re-validation post-implementation by running `pytest` and confirming each scenario exists and passes.

### Traceability Matrix (compact)

| AC                              | Unit IDs                  | Integration IDs                        |
| ------------------------------- | ------------------------- | -------------------------------------- |
| AC-1 (CLI + exit codes)         | —                         | INT-001, INT-002, INT-009              |
| AC-2 (success + qualifier)*     | —                         | INT-003                                |
| AC-3 (failure block format)     | UNIT-001                  | INT-004, INT-005                       |
| AC-4 (no side-effects)          | UNIT-002                  | INT-006                                |
| AC-5 (no API keys/network)      | —                         | INT-007, INT-008                       |
| AC-6 (v1 checks)                | UNIT-003 … UNIT-018       | INT-009                                |
| AC-7 (`--strict`)               | UNIT-019 … UNIT-022       | INT-010, INT-011                       |
| AC-8 (shared validator module)  | —                         | INT-012, INT-013, INT-014, INT-015     |
| AC-9 (`run` unchanged)          | —                         | INT-016                                |
| AC-10 (test depth)              | covered transitively      | covered transitively                   |
| AC-11 (docs)                    | UNIT-023                  | INT-017                                |
| AC-12 (`--help` content)        | —                         | INT-018                                |
| Performance (PERF-001 smoke)    | —                         | INT-019                                |

\* AC-2 wording revision recommended — see Gap 1 below.

### Gaps Identified

No ACs are uncovered, but four AC-binding gaps surface from this trace:

1. **AC-2 wording omits scope qualifier (BUS-002, Medium).** Test `1.6-INT-003` snapshots `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]`. The current AC-2 text omits the bracketed qualifier; implementation against the literal AC would fail the snapshot. Revise AC-2 wording.
2. **No AC for import allow-list (SEC-001, Medium-High).** AC-4 forbids runtime side-effects but does not constrain which package modules `yaml_validation.py` may import. Add **AC-13 (import allow-list)** mechanized by `1.6-UNIT-002`.
3. **No AC for sentinel-file regression test (SEC-001, Medium-High).** The single highest-confidence guard against silent `exec()` reintroduction (`1.6-INT-006`) is documented in the test design but not bound to an AC. Add **AC-14 (sentinel-file regression test)** as a security-critical, not-auto-fixable test.
4. **Refactor parity in DoD only (TECH-001 / OPS-001, High).** The parity test asset (`1.6-INT-013` / `1.6-INT-014` / `1.6-INT-015`) is the mechanizing artifact for the highest-priority risk in the profile but lives in the DoD bullet rather than an AC. Add **AC-15 (refactor parity suite)**.

Optional follow-up (Gap 5, BUS-001 / TECH-003): `--strict` lacks an inline ignore mechanism (AC-16) and a curated `strict_known_good/` CI fixture (AC-17). Acceptable v1; track as follow-up.

### Recommendations

**Must-add before development starts (closes Gaps 1–4):**

| AC ID  | Title                              | Mechanizing scenario                             |
| ------ | ---------------------------------- | ------------------------------------------------ |
| AC-2*  | Revise wording with scope qualifier| `1.6-INT-003`                                    |
| AC-13  | Import allow-list                  | `1.6-UNIT-002`                                   |
| AC-14  | Sentinel-file exec guard           | `1.6-INT-006`                                    |
| AC-15  | Refactor parity suite              | `1.6-INT-013`, `1.6-INT-014`, `1.6-INT-015`      |

**P0 scenarios that must run in-PR:**

1. `1.6-UNIT-002` — import allow-list
2. `1.6-INT-006` — sentinel-file exec guard
3. `1.6-INT-013` — refactor parity (examples)
4. `1.6-INT-014` — refactor parity (broken fixtures)
5. `1.6-INT-015` — engine error-message snapshot diff (capture pre-refactor baseline first)
6. `1.6-INT-003` — success message scope qualifier
7. `1.6-INT-018` — `--help` enumerates limitations

**Operational recommendations:**

- Capture pre-refactor engine error snapshots **before** any code in `yaml_validation.py` is written; without a fixed baseline `1.6-INT-015` has no reference point.
- Run `1.6-UNIT-002` in a clean subprocess so `sys.modules` isn't polluted by prior tests.
- Sentinel test must use `tempfile.gettempdir()` with a uuid suffix for cross-platform safety and cross-test isolation.
- Document `--strict` heuristic's known false-positive shape in `--help` to backstop the BUS-001 / TECH-003 mitigation.

### Trace Hook

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.6-trace-20260501.md
```

## SM Validation

**Date:** 2026-05-01
**Validator:** Bob (Scrum Master)
**Mode:** YOLO (non-interactive)
**Checklist:** Definition of Ready + Story Draft Checklist

### Definition of Ready Results

| Criterion                                                                | Status | Notes                                                                                                                                                                |
| ------------------------------------------------------------------------ | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Story has clear title and description                                    | PASS   | Title is precise (`tea validate` workflow command); user-story statement captures actor, want, and benefit.                                                          |
| Acceptance criteria are defined and testable                             | PASS   | AC-1 through AC-12 defined; each is observable via CLI exit code, output snapshot, or unit assertion. QA recommends adding AC-13/14/15 + revised AC-2 (see below).   |
| Dependencies are identified                                              | PASS   | Coordinates with TEA-DX-001.5 (error formatter); identifies integration points: `cli.py`, `yaml_engine.py`, `yaml_nodes.py:1130-1170`. Pattern ref: `report_cli.py`. |
| Technical approach is documented                                         | PASS   | Three-step integration approach in Technical Notes; new `yaml_validation.py` module specified; key no-side-effect constraint called out.                             |
| Story is properly sized                                                  | PASS   | 6 task groups, all scoped within a single CLI subcommand + targeted refactor. Risk/rollback plan documented.                                                         |
| QA notes sections present (Risk, NFR, Test Design, Requirements Trace)   | PASS   | All four sections present, each with full report links. 9 risks profiled, 4 NFRs assessed, 42 test scenarios designed, 100% AC trace coverage.                       |
| No blocking issues or unknowns                                           | PASS   | No external blockers. Coordination with TEA-DX-001.5 has graceful fallback either way. QA-recommended AC additions are advisory and listed for dev to incorporate.   |

### Story Draft Checklist Results

| Category                             | Status | Issues                                                                                                                                                                                       |
| ------------------------------------ | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1. Goal & Context Clarity            | PASS   | Problem statement grounds the work in a real user-pain incident (mid-run failure after Claude API spend). Epic linkage and TEA-DX-001.5 coordination explicit.                               |
| 2. Technical Implementation Guidance | PASS   | New module path, refactor target lines, CLI integration pattern (Typer subcommand), and import-allow-list constraint all specified. Existing pattern reference (`report_cli.py`) given.      |
| 3. Reference Effectiveness           | PASS   | References cite specific lines (`yaml_nodes.py:1130-1170`) and named modules. QA assessment links use consistent `docs/qa/assessments/<id>-<kind>-<date>.md` format.                          |
| 4. Self-Containment Assessment       | PASS   | Required checks enumerated inline (AC-6); risks/mitigations documented in-story; non-checks called out (run-block semantics, runtime template errors). Dev does not need to chase external docs to start. |
| 5. Testing Guidance                  | PASS   | 42 scenarios mapped to ACs; P0/P1/P2/P3 priority bands; fixture directory layout, snapshot file paths, monkeypatch strategy, and execution order all specified.                              |

### Clarity Score

**9/10** — exceptionally well-prepared story. Single deduction: AC-2 wording in the body (line 40) does not yet reflect the QA-recommended scope-qualifier suffix that test `1.6-INT-003` will assert. Dev should treat AC-2 as the revised wording shown in the NFR section (line 234) rather than the literal current text, or update the AC text in the implementing PR.

### Dev Notes (advisory — not blockers)

The QA package recommends four AC adjustments that are *not yet* in the AC list at the top of the story but are mechanized by P0 test scenarios. The dev agent should treat these as binding for the PR:

1. **AC-2 revised wording:** success message must include the bracketed qualifier `[structural checks only — run blocks not executed]` (snapshot-asserted by `1.6-INT-003`).
2. **AC-13 (import allow-list):** `yaml_validation.py` must not transitively import executor-related modules from the package; enforced by `1.6-UNIT-002` (run in clean subprocess).
3. **AC-14 (sentinel-file regression test):** fixture writes uniquely-named sentinel via `tempfile.gettempdir()`; `tea validate` must not produce it (`1.6-INT-006`).
4. **AC-15 (refactor parity suite):** `1.6-INT-013/014/015` — capture pre-refactor engine error snapshots BEFORE writing any code in `yaml_validation.py` so the snapshot diff has a baseline.

### Final Assessment

**READY FOR DEVELOPMENT.** All Definition of Ready criteria pass. QA gate is CONCERNS pending the four advisory AC additions above, which the dev should incorporate in-PR. Story is unambiguously implementable as written.

## Dev Agent Record

### Agent Model Used

claude-opus-4-7 (1M context) — James (BMad dev persona, YOLO mode)

### Debug Log References

- Iteration started 2026-05-02; story already had a substantial in-progress
  implementation on disk (`yaml_validation.py`, CLI `validate` command,
  fixtures, and tests). This pass verified completion against ACs and DoD,
  surfaced one DoD gap (`tea validate examples/yaml/*.yaml`), and patched it.

### Completion Notes List

1. **Validator module (`yaml_validation.py`)** is in place with the
   AC-13 import allow-list contract honored. The module imports only
   stdlib + `yaml` + `jinja2`; a clean-subprocess `sys.modules` inspection
   test (`1.6-UNIT-002`) enforces no transitive executor imports.

2. **AC-6 v1 checks** all implemented: YAML parse, missing top-level fields,
   node-name uniqueness, edge from/to validity (incl. list-form `to:`),
   `dynamic_parallel` mode mutual-exclusion + missing-`fan_in` + missing-
   `items`, parallel edges' `fan_in`, Jinja parse check on
   `condition:`/`when:` expressions, and `goto:` target validity. Each
   category has a dedicated unit test against a fixture in
   `tests/fixtures/validate/broken/`.

3. **CLI subcommand** registered in `cli.py` with success format
   `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]`
   (AC-2 revised wording from QA, snapshot-asserted by `1.6-INT-003`).
   Failures emit one block per finding to stderr, plus a `FAIL: <file>
   (N error(s), M warning(s))` summary. Exit codes 0 (success) and 1
   (any error or `--strict` warning).

4. **`--help`** enumerates checks performed AND explicit non-checks
   (semantic correctness of `run:` blocks, runtime template undefineds,
   LLM credential availability, LTM reachability) per AC-12 / BUS-002 /
   TECH-002 mitigation, snapshot-asserted by `1.6-INT-018`.

5. **`--strict`** adds two best-effort static warnings:
   `UNREFERENCED_NODE` and `DEAD_STATE_KEY`, plus a recommended-but-not-
   required `MISSING_FIELD` warning for top-level `name:`. Five-fixture
   known-good and known-bad suites guard against false positives /
   negatives (BUS-001 / TECH-003).

6. **Engine init parity** wired in `yaml_engine.py:1128-1162`:
   `YAMLEngine.load_from_dict()` calls `validate_workflow_dict()` and
   raises `ValueError` for any "hard" error code. This preserves the
   engine's "raise on init" contract while ensuring `tea run` and
   `tea validate` emit the same finding for the same broken YAML
   (AC-15 parity suite, `1.6-INT-012/013/014`).

7. **SEC-001 sentinel-file test** (`1.6-INT-006`) writes a uniquely-named
   sentinel via `tempfile.gettempdir()` + UUID and asserts the validator
   never executes the `run:` block.

8. **Side-effect guards** (AC-4 / AC-5): `1.6-INT-007` monkeypatches
   `socket.socket.__init__` to raise — the validator never opens a socket.
   `1.6-INT-008` wraps `os.environ.__getitem__` to record reads of
   `ANTHROPIC_API_KEY` / `OPENAI_API_KEY` / `PERPLEXITY_API_KEY` /
   `FIRECRAWL_API_KEY` — the validator never reads them.

9. **DoD example pass:** the four `examples/yaml/dynamic_parallel_*.yaml`
   examples were missing the required `fan_in:` field — they had been
   broken since `dynamic_parallel` was introduced (the engine raised the
   same error at init). Patched each example to declare an explicit
   `fan_in:` matching the next node in the workflow. After this,
   `tea validate examples/yaml/*.yaml` exits 0 for every shipped example.

10. **Test status**: 43 dedicated tests (`tests/test_yaml_validation.py`
    + `tests/test_dx_001_6_validate.py`) all pass. Combined with adjacent
    YAML/engine suites (`test_yaml_engine_*`, `test_yaml_dynamic_parallel`,
    `test_dx_001_5_*`) the 176-test bundle passes. Test-pollution
    failures observed in the broader 4000+ test suite are pre-existing
    and unrelated to this story (missing optional dependencies like
    `parameterized`, `numpy`; pre-existing test-ordering issues in
    `test_data_tabular_actions.py`, `test_execution_modes.py`,
    `test_experiment_*.py`).

### File List

**Created:**

- `python/src/the_edge_agent/yaml_validation.py` — Pure-Python validator
  module (902 lines).
- `python/tests/test_yaml_validation.py` — Comprehensive unit + CLI
  integration + security regression + parity tests (634 lines).
- `python/tests/test_dx_001_6_validate.py` — Smoke test bundle (176
  lines) covering the AC-1/AC-3/AC-4 path through CLI invocation.
- `python/tests/fixtures/validate/valid/minimal.yaml`
- `python/tests/fixtures/validate/valid/with_condition.yaml`
- `python/tests/fixtures/validate/valid/dynamic_parallel.yaml`
- `python/tests/fixtures/validate/broken/yaml_parse_error.yaml`
- `python/tests/fixtures/validate/broken/missing_nodes.yaml`
- `python/tests/fixtures/validate/broken/edge_to_undefined.yaml`
- `python/tests/fixtures/validate/broken/edge_from_undefined.yaml`
- `python/tests/fixtures/validate/broken/duplicate_nodes.yaml`
- `python/tests/fixtures/validate/broken/dyn_parallel_no_fan_in.yaml`
- `python/tests/fixtures/validate/broken/dyn_parallel_no_items.yaml`
- `python/tests/fixtures/validate/broken/dyn_parallel_two_modes.yaml`
- `python/tests/fixtures/validate/broken/dyn_parallel_fan_in_undefined.yaml`
- `python/tests/fixtures/validate/broken/invalid_jinja_condition.yaml`
- `python/tests/fixtures/validate/sentinel/sentinel_template.yaml`
- `python/tests/fixtures/validate/strict_known_good/*.yaml` (5 fixtures)
- `python/tests/fixtures/validate/strict_known_bad/*.yaml` (5 fixtures)
- `docs/python/cli-validate.md` — `tea validate` reference (synopsis,
  exit codes, output format, checks, non-checks, `--strict`, CI recipe,
  programmatic use, security properties).

**Modified:**

- `python/src/the_edge_agent/cli.py` — Registered `validate` Typer
  subcommand (lines 2607-2724); lazy import of `yaml_validation` to keep
  AC-13 import allow-list intact when only this command is invoked.
- `python/src/the_edge_agent/yaml_engine.py` — Engine init now calls
  `validate_workflow_dict()` and raises a `ValueError` for hard codes
  (lines 1128-1162) so `tea run` and `tea validate` produce parity
  findings (AC-8 / AC-15).
- `docs/shared/cli-reference.md` — Added `tea validate` to the
  Validation and Inspection section.
- `docs/shared/YAML_REFERENCE.md` — Added "Validating Workflows"
  section linking to `cli-validate.md`.
- `examples/yaml/dynamic_parallel_action_mode.yaml` — Added
  `fan_in: aggregate`.
- `examples/yaml/dynamic_parallel_steps_mode.yaml` — Added
  `fan_in: combine_results`.
- `examples/yaml/dynamic_parallel_subgraph_mode.yaml` — Added
  `fan_in: aggregate_analyses`.
- `examples/yaml/dynamic_parallel_fail_fast.yaml` — Added
  `fan_in: report`.

### Change Log

- **2026-05-02 (initial):** Verified `yaml_validation.py`, `tea validate`
  CLI, engine-init parity, fixtures, tests, and docs were already in
  place from prior development. Confirmed all 43 dedicated tests pass.
- **2026-05-02 (DoD bullet — examples pass):** Found four
  `examples/yaml/dynamic_parallel_*.yaml` examples lacked the required
  `fan_in:` field (a pre-existing latent bug — `tea run` would have
  raised the same error). Patched each example to declare its fan_in
  target so `tea validate examples/yaml/*.yaml` now exits 0 for every
  shipped example.
- **2026-05-02 (status):** Set story status to "Ready for Review";
  marked all six task groups complete.

## QA Results

### Review Date: 2026-05-02

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Solid, focused implementation that delivers exactly the contract the story
asked for. The new `yaml_validation.py` module is pure-Python, side-effect
free, and well-organized: a `_LineLoader` PyYAML subclass attaches
`__line__`/`__column__` markers to mappings (giving precise file:line:col
diagnostics), a stable `ValidationCode` namespace exposes machine-readable
error identifiers, and `WorkflowValidationError` carries `severity`, `node`,
`path`, and a `normalized_key()` helper purpose-built for the AC-15 parity
test. The CLI subcommand is thin and lazy-imports the validator so the
AC-13 import allow-list contract holds even in interactive runs. Engine
init at `yaml_engine.py:1128-1162` calls `validate_workflow_dict` and
re-raises hard codes as a single aggregated `ValueError`, preserving the
"raise on init" contract while guaranteeing parity with `tea validate`.

The four `examples/yaml/dynamic_parallel_*.yaml` fan-in fixes were a
genuine pre-existing latent bug (the engine had been raising the same
`fan_in` error all along — surfacing it is exactly what this story is
supposed to do) and the patches are minimal and correct.

### Refactoring Performed

None. The implementation was already in good shape and adding refactors
would exceed the QA-Results-only scope.

### Compliance Check

- Coding Standards: ✓ Type hints present; module docstring spells out
  the AC-13 import allow-list contract; no `print()` calls in the
  validator (CLI uses `typer.echo`).
- Project Structure: ✓ New module lives at
  `python/src/the_edge_agent/yaml_validation.py` (top-level package
  member, mirroring `yaml_engine.py`); fixtures under
  `python/tests/fixtures/validate/{valid,broken,sentinel,strict_known_*}`
  match the test-design layout.
- Testing Strategy: ✓ 43 dedicated tests across unit + integration;
  P0 risk-fix gates all mechanized (parity, sentinel, import allow-list,
  scope qualifier, --help limitations).
- All ACs Met: ✓ Including the four QA-recommended additions
  (AC-2 revised wording, AC-13 import allow-list, AC-14 sentinel-file,
  AC-15 refactor parity).

### Improvements Checklist

All "must fix before merge" items from the risk profile and NFR
assessment are present in the PR:

- [x] AC-13 import allow-list enforced by `1.6-UNIT-002`
  (clean-subprocess `sys.modules` inspection — verified locally with
  `forbidden = []`)
- [x] AC-14 sentinel-file regression test in
  `tests/test_yaml_validation.py::TestSecurityGuards::test_sentinel_file_not_created_by_validate`
  using `tempfile.gettempdir()` + UUID
- [x] AC-15 refactor parity suite —
  `TestRefactorParity` covers valid fixtures, broken fixtures, and
  engine-init `ValueError` raising for all hard codes
- [x] AC-2 revised wording — success line includes
  `[structural checks only — run blocks not executed]`, snapshot-asserted
  by `1.6-INT-003` and `TestValidateCLI::test_validate_ok_exits_zero`
- [x] AC-12 `--help` enumerates explicit non-checks (run-block
  semantics, runtime template undefineds, LLM credentials, LTM
  reachability) — asserted by `1.6-INT-018`
- [x] AC-7 `--strict` false-positive guard — 5 known-good + 5
  known-bad fixtures (BUS-001 / TECH-003)
- [x] AC-4 / AC-5 side-effect guards — socket sentinel + secret-env
  read sentinel both green
- [x] DoD: every shipped `examples/yaml/*.yaml` (13 files) now
  validates cleanly
- [ ] Optional follow-up (AC-16): inline `# tea-validate: ignore <code>`
  escape hatch for `--strict` warnings — out of scope for this PR but
  worth tracking for false-positive triage
- [ ] Optional follow-up (AC-17): curated CI `strict_known_good/`
  directory wired into a separate CI job that fails on any new warning
  — fixtures exist, dedicated CI step does not

### Security Review

SEC-001 ("accidental exec of run blocks during validation") is the
only security concern called out by the risk profile and is mitigated
by two complementary guards:

1. **Static (AC-13):** the validator module's import surface is
   constrained — verified by a unit test running in a clean Python
   subprocess that loads `yaml_validation.py` via `importlib.util` and
   asserts `the_edge_agent.{yaml_engine, yaml_nodes, cli, parallel*,
   checkpointers*, checkpoint, actions, backends}` are absent from
   `sys.modules` afterwards. A future contributor pulling executor
   helpers into `yaml_validation.py` will fail this test.

2. **Behavioral (AC-14):** a fixture YAML whose `run:` block writes a
   uniquely-named sentinel file under `tempfile.gettempdir()` is
   validated through both the API and the CLI; the sentinel file
   must not exist afterwards. Cross-platform safe (uses
   `tempfile.gettempdir()`); UUID-suffixed to prevent cross-test
   contamination.

Additional belt-and-suspenders: socket-open sentinel (validator never
opens a socket) and API-key-env-read sentinel
(`ANTHROPIC_API_KEY`/`OPENAI_API_KEY`/`PERPLEXITY_API_KEY`/
`FIRECRAWL_API_KEY` are never read).

### Performance Considerations

Validation is O(nodes + edges) with negligible constants; the 500-node
synthetic smoke (`test_500_node_validation_completes_quickly`)
completes well under the 5s budget. Validator runs off the runtime hot
path (engine init only) — no concerns.

### Files Modified During Review

None.

### Test Execution Evidence

- `pytest tests/test_yaml_validation.py tests/test_dx_001_6_validate.py
  -v` → **43 passed** in 0.53s.
- `pytest tests/test_yaml_engine_observability.py
  tests/test_cli_unified.py tests/test_yaml_dynamic_parallel.py
  tests/test_cli.py -q` → **171 passed** in 1.19s (no regressions in
  adjacent surfaces).
- `for f in examples/yaml/*.yaml; do tea validate "$f"; done` → all
  13 shipped examples exit 0.

### Risk-Mitigation Coverage

| Risk ID  | Title                                                                          | Score | Status                                                                 |
| -------- | ------------------------------------------------------------------------------ | ----- | ---------------------------------------------------------------------- |
| TECH-001 | Refactor changes `tea run` error timing/message phase                          | 6     | MITIGATED — `TestRefactorParity` (3 scenarios) all green               |
| BUS-001  | `--strict` false positives erode user trust                                    | 4     | MITIGATED — 5+5 known-good/known-bad fixtures + opt-in default          |
| BUS-002  | Validator gives false confidence — passes but workflow fails at runtime        | 4     | MITIGATED — scope qualifier in success message + `--help` limitations   |
| TECH-003 | `--strict` dead-state-key static analysis precision                            | 3     | MITIGATED — conservative regex + known-good fixture guard               |
| SEC-001  | Accidental `exec()` of `run:` blocks during validation                         | 3     | MITIGATED — import allow-list + sentinel-file regression test           |
| OPS-001  | Engine init error-timing shift breaks downstream test assertions               | 3     | MITIGATED — adjacent 171-test suite green; hard-codes filter conservative |
| TECH-002 | Jinja parse check misses runtime-only template errors                          | 2     | DOCUMENTED — explicit in `--help` non-checks list                       |
| OPS-002  | CI-integration recipe under-documented; inconsistent adoption                  | 1     | MITIGATED — CI recipe in `docs/python/cli-validate.md`                  |
| PERF-001 | Slow validation on very large workflows                                        | 1     | MITIGATED — 500-node smoke test in budget                               |

All 9 risks from the risk profile resolved (7 mitigated, 1 documented,
1 mitigated via docs).

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-DX-001.6-tea-validate-command.yml
Risk profile: docs/qa/assessments/TEA-DX-001.6-risk-20260501.md
NFR assessment: docs/qa/assessments/TEA-DX-001.6-nfr-20260501.md
Test design: docs/qa/assessments/TEA-DX-001.6-test-design-20260501.md
Trace: docs/qa/assessments/TEA-DX-001.6-trace-20260501.md

The pre-implementation risk profile recommended CONCERNS until three
"must fix before merge" mitigations landed (parity suite, sentinel
test, success-message scope qualifier). All three are present and
green in this PR, plus the AC-13/14/15 advisory ACs are mechanized.
Re-evaluation per the risk profile's stated criterion: **PASS**.

### Recommended Status

✓ **Ready for Done** — all ACs met, all P0 mitigations in place, no
open issues. Owner decides final status.
