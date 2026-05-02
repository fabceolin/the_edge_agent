# Requirements Traceability Matrix

## Story: TEA-DX-001.6 — `tea validate` workflow command

**Date:** 2026-05-01
**Reviewer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Story status:** Draft (implementation not yet started — trace maps ACs to *planned* scenarios from the test design)

**Inputs consulted:**
- Story: `docs/stories/TEA-DX-001.6-tea-validate-command.md`
- Risk profile: `docs/qa/assessments/TEA-DX-001.6-risk-20260501.md`
- NFR assessment: `docs/qa/assessments/TEA-DX-001.6-nfr-20260501.md`
- Test design: `docs/qa/assessments/TEA-DX-001.6-test-design-20260501.md`

**Important context:** The story is in Draft. No `python/src/the_edge_agent/yaml_validation.py` exists; no `validate` Typer subcommand exists in `cli.py`; no `tests/test_validate*.py` exists. Therefore this matrix traces every in-scope AC to the **planned** scenarios documented in the test design (IDs `1.6-UNIT-*` / `1.6-INT-*`). Every AC has ≥1 planned scenario; the gap section instead flags scenarios that have no codified AC and the AC additions the NFR review recommended.

---

## Coverage Summary

| Metric                                   | Value                                                |
| ---------------------------------------- | ---------------------------------------------------- |
| Total in-scope ACs                       | 12 (AC-1 … AC-12)                                    |
| ACs with ≥1 planned scenario             | 12 (100 %)                                           |
| Fully covered (planned)                  | 12                                                   |
| Partially covered                        | 0                                                    |
| Not covered                              | 0                                                    |
| Recommended additions (Security/Reliab.) | 3 ACs (AC-13, AC-14, AC-15) + AC-2 wording revision  |
| Optional follow-up additions             | 2 ACs (AC-16, AC-17)                                 |
| Total planned test scenarios             | 42 (23 unit · 19 integration · 0 e2e)                |
| Scenarios mechanizing P0 risk-fix gates  | 7                                                    |

> Because nothing is implemented yet, "fully covered" here means "every AC is bound to a planned scenario in the test-design document with explicit Given-When-Then expectations." Coverage will need re-validation post-implementation by running `pytest` and confirming each scenario exists and passes.

---

## Requirement Mappings

### AC-1 — `tea validate <workflow.yaml>`; exit 0 on success, 1 on errors

**Coverage: FULL (planned)**

- **Integration test (planned):** `1.6-INT-001` — `tea validate <valid.yaml>` golden path
  - **Given:** A structurally-valid YAML workflow exists on disk
  - **When:** `tea validate <valid.yaml>` is invoked from the CLI
  - **Then:** Process exits with code 0; stdout matches the success snapshot
- **Integration test (planned):** `1.6-INT-002` — `tea validate <broken.yaml>` failure path
  - **Given:** A YAML workflow with at least one structural error exists on disk
  - **When:** `tea validate <broken.yaml>` is invoked
  - **Then:** Process exits with code 1
- **Integration test (planned):** `1.6-INT-009` — exit-code coverage across all AC-6 categories
  - **Given:** One broken fixture per AC-6 error category
  - **When:** `tea validate <fixture>` is invoked for each
  - **Then:** Exit code is 1 in every case; each error block matches its expected snapshot

### AC-2 — Success message: `OK: <workflow.yaml> (N nodes, M edges)`; exit 0

**Coverage: FULL (planned) — wording revision recommended**

- **Integration test (planned):** `1.6-INT-003` — success message snapshot
  - **Given:** Valid fixture with N=3 nodes and M=4 edges
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Stdout matches `OK: <fixture> (3 nodes, 4 edges) [structural checks only — run blocks not executed]` and exit code is 0

> **Recommendation (carried over from NFR review):** AC-2 wording should be revised to include the bracketed scope qualifier (`[structural checks only — run blocks not executed]`). The snapshot in `1.6-INT-003` already encodes the qualified form; the AC text in the story should match.

### AC-3 — Failure output blocks include path, line/col when available, formatted message from TEA-DX-001.5

**Coverage: FULL (planned)**

- **Unit test (planned):** `1.6-UNIT-001` — error-block formatter contract
  - **Given:** A list of `ValidationError` objects with file path, line, column, code, and human-readable message
  - **When:** The error formatter is called
  - **Then:** Each error renders as a separate block with file path, line/col when present, and the TEA-DX-001.5 formatted message
- **Integration test (planned):** `1.6-INT-004` — single-error CLI rendering
  - **Given:** A broken fixture with exactly one structural error
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Stdout contains exactly one error block matching the expected snapshot
- **Integration test (planned):** `1.6-INT-005` — multi-error CLI rendering
  - **Given:** A broken fixture with multiple structural errors
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Stdout contains one block per error in the documented order

### AC-4 — Validator must NOT instantiate LLM clients, fetch secrets, open LTM connections, or execute `run:` blocks

**Coverage: FULL (planned) — strongest evidence is sentinel + import allow-list**

- **Unit test (planned):** `1.6-UNIT-002` — import allow-list
  - **Given:** A clean `sys.modules` baseline (subprocess-isolated)
  - **When:** `import the_edge_agent.yaml_validation` is executed
  - **Then:** `sys.modules` does NOT include `yaml_engine` (executor classes), `actions.*`, `backends.*`, `checkpointers*`, or `parallel*`
- **Integration test (planned):** `1.6-INT-006` — sentinel-file exec guard
  - **Given:** A YAML fixture whose `run:` block writes a uniquely-named sentinel file under `tempfile.gettempdir()`
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Process exits 0 (structurally valid) AND the sentinel file does NOT exist on disk afterward

### AC-5 — Validator must NOT require API keys or external services

**Coverage: FULL (planned)**

- **Integration test (planned):** `1.6-INT-007` — network sentinel
  - **Given:** `socket.socket.__init__` is monkeypatched to raise `AssertionError("network call attempted")`
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Process exits 0; no AssertionError is raised
- **Integration test (planned):** `1.6-INT-008` — secret-env sentinel
  - **Given:** `os.environ.__getitem__` is wrapped to record key reads
  - **When:** `tea validate <fixture>` is invoked
  - **Then:** Recorded key list contains zero entries from the secret allow-list (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, etc.)

### AC-6 — V1 checks (parse, required fields, edge endpoints, dynamic_parallel, duplicate names, fan_in target, jinja parse on conditions)

**Coverage: FULL (planned) — one unit per category + CLI smoke**

| AC-6 sub-check                         | Planned unit (validator return) | Planned integration (CLI surface) |
| -------------------------------------- | ------------------------------- | --------------------------------- |
| YAML parse error                       | `1.6-UNIT-003`                  | `1.6-INT-009` (parse fixture)     |
| Required top-level fields missing      | `1.6-UNIT-004` (`name`)         | `1.6-INT-009`                     |
|                                        | `1.6-UNIT-005` (`nodes`)        |                                   |
|                                        | `1.6-UNIT-006` (`edges`)        |                                   |
| Edge `from` references undeclared node | `1.6-UNIT-007`                  | `1.6-INT-009`                     |
| Edge `to` references undeclared node   | `1.6-UNIT-008`                  | `1.6-INT-009`                     |
| `__start__` / `__end__` accepted       | `1.6-UNIT-009` (negative case)  | covered by `1.6-INT-001`          |
| `dynamic_parallel` mutual exclusion    | `1.6-UNIT-010` (only `action`)  | `1.6-INT-009`                     |
|                                        | `1.6-UNIT-011` (only `steps`)   |                                   |
|                                        | `1.6-UNIT-012` (only `subgraph`)|                                   |
|                                        | `1.6-UNIT-013` (>1 set → err)   |                                   |
|                                        | `1.6-UNIT-014` (none set → err) |                                   |
| `dynamic_parallel` valid `fan_in`      | `1.6-UNIT-015`                  | `1.6-INT-009`                     |
| Duplicate node names                   | `1.6-UNIT-016`                  | `1.6-INT-009`                     |
| `fan_in` references undeclared node    | `1.6-UNIT-017`                  | `1.6-INT-009`                     |
| `condition:` jinja parse error         | `1.6-UNIT-018`                  | `1.6-INT-009`                     |

**Given-When-Then template for each unit:**
- **Given:** A `dict` parsed from a fixture exercising exactly one error category
- **When:** `validate_workflow(path)` is called
- **Then:** Returned `List[ValidationError]` contains exactly the expected error code, references the offending node/edge, and includes line/col when PyYAML supplies them

### AC-7 — `--strict` flag promotes soft warnings (unreferenced nodes, dead state-key references) to errors

**Coverage: FULL (planned) — pair of negative + positive fixture suites**

- **Unit test (planned):** `1.6-UNIT-019` — `--strict` promotes warnings to errors
  - **Given:** A YAML fixture that produces zero errors and ≥1 warning
  - **When:** `validate_workflow(path, strict=True)` vs `strict=False`
  - **Then:** `strict=False` returns `[]`; `strict=True` returns the warnings as errors
- **Unit test (planned):** `1.6-UNIT-020` — unreferenced-node detection
  - **Given:** A node declared in `nodes:` but never referenced by an edge `from` or `to`
  - **When:** Strict validation runs
  - **Then:** Warning of code `unreferenced-node` is emitted, naming the node
- **Unit test (planned):** `1.6-UNIT-021` — dead state-key detection (best-effort)
  - **Given:** A `condition:` referencing `state["never_set"]` with no node assigning that key
  - **When:** Strict validation runs
  - **Then:** Warning of code `dead-state-key` is emitted, naming the key and condition node
- **Unit test (planned):** `1.6-UNIT-022` — false-positive guard for state-key heuristic
  - **Given:** A condition referencing a key set inside a `run:` block via `state["x"] = ...`
  - **When:** Strict validation runs
  - **Then:** No `dead-state-key` warning is emitted (heuristic accepts run-block writes)
- **Integration test (planned):** `1.6-INT-010` — `--strict` true-positive suite
  - **Given:** ≥5 fixtures from `strict_known_bad/` each containing a clear strict-mode violation
  - **When:** `tea validate --strict <fixture>` is invoked for each
  - **Then:** Each exits 1 with at least one warning of the expected code
- **Integration test (planned):** `1.6-INT-011` — `--strict` false-positive guard
  - **Given:** ≥5 fixtures from `strict_known_good/` (tricky-but-valid YAMLs)
  - **When:** `tea validate --strict <fixture>` is invoked for each
  - **Then:** Each exits 0; zero warnings emitted

### AC-8 — Refactor structural validation into a shared module used by both `validate` and the engine

**Coverage: FULL (planned)**

- **Integration test (planned):** `1.6-INT-012` — engine init delegates to validator
  - **Given:** `yaml_validation.validate_workflow` is patched/spied
  - **When:** Engine init runs against a fixture
  - **Then:** Spy records exactly one call to `validate_workflow` during init

### AC-9 — Adding `validate` does not change `run` semantics

**Coverage: FULL (planned)**

- **Integration test (planned):** `1.6-INT-016` — `tea run` golden-path regression
  - **Given:** The existing `tea run` golden-path test suite
  - **When:** All tests are executed post-refactor
  - **Then:** Every test passes with identical stdout, exit code, and side effects

### AC-10 — Tests cover: valid → exit 0; one test per error category; `--strict` adds warnings

**Coverage: FULL (planned) — meta-AC, satisfied by AC-1, AC-6, AC-7 scenarios**

- Valid workflow → exit 0: `1.6-INT-001`
- One test per error category: `1.6-UNIT-003` … `1.6-UNIT-018` plus `1.6-INT-009`
- `--strict` adds warnings: `1.6-UNIT-019`, `1.6-INT-010`, `1.6-INT-011`

### AC-11 — Documented in `docs/python/` CLI reference

**Coverage: FULL (planned)**

- **Unit test (planned):** `1.6-UNIT-023` — docs presence check
  - **Given:** The `docs/python/` directory tree
  - **When:** Test scans for the new `validate` CLI reference section
  - **Then:** A section named `validate` exists with required subsections (synopsis, options, exit codes, scope qualifier, examples)
- **Integration test (planned):** `1.6-INT-017` — shipped-example smoke
  - **Given:** Every YAML under `examples/yaml/`
  - **When:** `tea validate <example>` is invoked for each
  - **Then:** All exit 0 (proves docs claims and examples are validator-clean)

### AC-12 — `tea validate --help` describes checks and limitations

**Coverage: FULL (planned)**

- **Integration test (planned):** `1.6-INT-018` — `--help` snapshot
  - **Given:** `tea validate --help` is invoked
  - **When:** Stdout is captured
  - **Then:** Snapshot contains: list of v1 checks (AC-6 enumeration), the scope qualifier from AC-2, and explicit non-checks (run-block semantics, runtime template-undefined errors)

### Performance smoke (informal NFR — not an AC; covered for risk PERF-001)

- **Integration test (planned):** `1.6-INT-019` — 500-node validation
  - **Given:** A synthetically-generated 500-node workflow
  - **When:** `validate_workflow` is invoked
  - **Then:** Returns within <1s on the CI runner (smoke; not a hard gate)

---

## AC ↔ Test ID Matrix (compact)

| AC                              | Unit IDs                  | Integration IDs                        |
| ------------------------------- | ------------------------- | -------------------------------------- |
| AC-1                            | —                         | INT-001, INT-002, INT-009              |
| AC-2 (revised wording)          | —                         | INT-003                                |
| AC-3                            | UNIT-001                  | INT-004, INT-005                       |
| AC-4 (also AC-13/14)            | UNIT-002                  | INT-006                                |
| AC-5                            | —                         | INT-007, INT-008                       |
| AC-6                            | UNIT-003 … UNIT-018       | INT-009                                |
| AC-7                            | UNIT-019 … UNIT-022       | INT-010, INT-011                       |
| AC-8 (also AC-15)               | —                         | INT-012, INT-013, INT-014, INT-015     |
| AC-9                            | —                         | INT-016                                |
| AC-10                           | covered transitively      | covered transitively                   |
| AC-11                           | UNIT-023                  | INT-017                                |
| AC-12                           | —                         | INT-018                                |
| Performance (PERF-001 smoke)    | —                         | INT-019                                |

---

## Gaps Identified

### Gap 1 — AC text omits the success-message scope qualifier (BUS-002)

- **Requirement:** AC-2 — `Output on success: OK: <workflow.yaml> (N nodes, M edges) and exit 0`
- **Gap:** The AC does not include the bracketed scope qualifier `[structural checks only — run blocks not executed]`. The test scenario `1.6-INT-003` already snapshots the qualified string, so the test would *fail* against a literal AC-2 implementation. The story's NFR review (BUS-002) recommends revising AC-2 to include the qualifier; this trace confirms the test plan already assumes the revision.
- **Severity:** Medium — user-facing message that mitigates the original LLM-spend pain point.
- **Recommendation:** Update AC-2 wording in the story before development starts. Implementer aligns code to the qualified form; `1.6-INT-003` snapshot stays as-is.

### Gap 2 — Import allow-list is not an AC (SEC-001)

- **Requirement:** AC-4 says "Validator does not instantiate LLM clients … or execute any `run:` block."
- **Gap:** AC-4 forbids *runtime side-effects* but does not constrain which package modules `yaml_validation.py` may import. A future contributor importing executor code (e.g., `from .yaml_engine import …`) for a "shared helper" can silently re-introduce executor pathways without breaking AC-4. The sentinel test (`1.6-INT-006`) is then the only line of defense; an import-time module recorded under a fast-fail check is cheaper to maintain.
- **Severity:** Medium-High — future-regression vector flagged in the NFR review (SEC-001).
- **Recommendation:** Add **AC-13 (Security — import allow-list)**: `yaml_validation.py` must not transitively load executor-related modules (`yaml_engine.YAMLEngine`, executor functions in `yaml_nodes`, `cli`, `parallel*`, `checkpointers`, `actions.*`, `backends.*`); allowed dependencies are `yaml_config` dataclasses and the TEA-DX-001.5 error-formatter helper. Test `1.6-UNIT-002` already mechanizes this AC and runs in subprocess isolation.

### Gap 3 — Sentinel-file regression test is not an AC (SEC-001)

- **Requirement:** AC-4 covers *intent* but not the regression mechanism.
- **Gap:** The single highest-confidence guard against silent re-introduction of `exec()` is the sentinel-file fixture. It exists in the test design (`1.6-INT-006`) but is not bound to an explicit AC, leaving it removable as "extra".
- **Severity:** Medium-High.
- **Recommendation:** Add **AC-14 (Security — sentinel-file regression test)** and mark it as a security-critical, not-auto-fixable test.

### Gap 4 — Refactor parity is in DoD but not an AC (TECH-001 / OPS-001)

- **Requirement:** Definition of Done bullet "`tea run` continues to surface the same errors at init time".
- **Gap:** DoD is enforceable by reviewer judgment, not by a test gate. The parity *test asset* (every example + every broken fixture validated through both surfaces, identical normalized error sets) is the mechanizing artifact and is documented in test design (`1.6-INT-013`, `1.6-INT-014`, `1.6-INT-015`). Without an AC tie, the parity suite can be skipped under PR pressure — exactly the kind of silent drift the risk profile flags.
- **Severity:** High (TECH-001 is the only High-priority risk in the profile).
- **Recommendation:** Add **AC-15 (Reliability — refactor parity suite)** binding `1.6-INT-013`, `1.6-INT-014`, and `1.6-INT-015` as required scenarios. Pre-refactor snapshot capture for `1.6-INT-015` should land in the same PR to prevent retroactive baseline drift.

### Gap 5 — `--strict` lacks an inline ignore mechanism (BUS-001 / TECH-003)

- **Requirement:** AC-7 introduces opt-in static analysis warnings.
- **Gap:** No inline escape hatch (e.g., `# tea-validate: ignore unreferenced-node`) and no curated CI fixture set guarding heuristic changes. False positives in `--strict` will push teams to disable it altogether — the failure mode the BUS-001 risk anticipates.
- **Severity:** Medium-Low — acceptable v1, planned for follow-up per the NFR review.
- **Recommendation:** Track AC-16 (inline ignore comments) and AC-17 (curated `strict_known_good/` fixture set) as follow-up stories rather than blocking AC-7. False-positive guard `1.6-INT-011` partially compensates by failing CI on any new warning against tricky-but-valid fixtures.

---

## Recommendations

### Must-add ACs before development starts (closes Gaps 1–4)

| AC ID  | Title                              | Mechanizing scenario                             |
| ------ | ---------------------------------- | ------------------------------------------------ |
| AC-13  | Import allow-list                  | `1.6-UNIT-002`                                   |
| AC-14  | Sentinel-file exec guard           | `1.6-INT-006`                                    |
| AC-15  | Refactor parity suite              | `1.6-INT-013`, `1.6-INT-014`, `1.6-INT-015`      |
| AC-2*  | Revise wording with scope qualifier| `1.6-INT-003` (snapshot already assumes revision)|

### Optional follow-up ACs (Gap 5)

| AC ID  | Title                                        | Note                                       |
| ------ | -------------------------------------------- | ------------------------------------------ |
| AC-16  | `--strict` supports inline ignore comments   | Defer to follow-up story                    |
| AC-17  | Curated `strict_known_good/` fixture in CI   | Defer to follow-up story                    |

### P0 scenarios that must run in-PR (mirrors test design's "Must-Have")

1. `1.6-UNIT-002` — import allow-list
2. `1.6-INT-006` — sentinel-file exec guard
3. `1.6-INT-013` — refactor parity (examples)
4. `1.6-INT-014` — refactor parity (broken fixtures)
5. `1.6-INT-015` — engine error-message snapshot diff
6. `1.6-INT-003` — success message scope qualifier
7. `1.6-INT-018` — `--help` enumerates limitations

### Operational recommendations

- **Capture pre-refactor engine error snapshots first.** `1.6-INT-015` requires baseline captured on the *current* engine; record it before any code in `yaml_validation.py` is written. Without this, the diff has no fixed reference point.
- **Run `1.6-UNIT-002` in a clean subprocess.** `sys.modules` is process-wide; a prior test that imports `yaml_engine` will mask a violation. Pytest's default in-process model is insufficient.
- **Sentinel test must use `tempfile.gettempdir()` with a uuid suffix.** Hard-coded `/tmp/...` paths break on Windows and risk cross-test contamination.
- **Document the `--strict` heuristic's known false-positive shape in `--help`** so users hit by the heuristic understand it is best-effort. Backstops the mitigation flagged for BUS-001 / TECH-003.

---

## Risk Assessment

| Risk Bucket   | Definition                         | Items in this trace                                                |
| ------------- | ---------------------------------- | ------------------------------------------------------------------ |
| **High**      | ACs with no covering scenario      | None                                                               |
| **Medium**    | Missing AC binding for required tests | Gap 1 (AC-2 wording), Gap 2 (AC-13), Gap 3 (AC-14), Gap 4 (AC-15) |
| **Low**       | ACs with full unit + integration coverage | AC-1, AC-3, AC-4, AC-6, AC-7, AC-8, AC-9, AC-11, AC-12, AC-10 |
| **Deferred**  | Optional follow-up                 | Gap 5 (AC-16, AC-17)                                               |

No AC is uncovered, but four medium-severity AC additions are required to harden the contract against regressions identified in the risk profile and NFR review. With those additions accepted, this story moves from CONCERNS to PASS at the gate.

---

## Trace Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.6-trace-20260501.md
```

---

## Gate YAML Block

```yaml
trace:
  totals:
    requirements: 12
    full: 12
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.6-test-design-20260501.md'
  uncovered: []
  recommended_additions:
    - ac: 'AC-2 (revise wording)'
      reason: 'Test 1.6-INT-003 already snapshots the qualified form; AC text must match'
    - ac: 'AC-13 (import allow-list)'
      reason: 'AC-4 forbids side-effects but does not constrain imports; SEC-001 regression vector'
      mechanizing_scenario: '1.6-UNIT-002'
    - ac: 'AC-14 (sentinel-file exec guard)'
      reason: 'Single highest-confidence guard against silent exec() reintroduction'
      mechanizing_scenario: '1.6-INT-006'
    - ac: 'AC-15 (refactor parity suite)'
      reason: 'TECH-001 high-risk parity guarantee currently lives in DoD only'
      mechanizing_scenarios:
        - '1.6-INT-013'
        - '1.6-INT-014'
        - '1.6-INT-015'
  follow_up_additions:
    - ac: 'AC-16 (inline ignore comments for --strict)'
    - ac: 'AC-17 (curated strict_known_good/ fixture set in CI)'
  notes: 'Story in Draft. Trace maps ACs to planned scenarios from test design; re-validate post-implementation.'
```
