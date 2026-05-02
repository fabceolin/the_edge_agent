# Test Design: Story TEA-DX-001.6 — `tea validate` workflow command

**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO (non-interactive)
**Story:** [`docs/stories/TEA-DX-001.6-tea-validate-command.md`](../../stories/TEA-DX-001.6-tea-validate-command.md)
**Risk profile:** [`TEA-DX-001.6-risk-20260501.md`](TEA-DX-001.6-risk-20260501.md)
**NFR assessment:** [`TEA-DX-001.6-nfr-20260501.md`](TEA-DX-001.6-nfr-20260501.md)

---

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 23 (55%)
- **Integration tests:** 19 (45%)
- **E2E tests:** 0 (0%) — `tea validate` is a CLI dev tool with no UI/network dependency; the integration layer (CLI → validator → fixtures on disk) is the realistic top boundary.
- **Priority distribution:** P0: 27, P1: 10, P2: 4, P3: 1
- **AC coverage:** 12 in-scope ACs + 3 must-add ACs (AC-13/14/15 from NFR) + revised AC-2 wording. AC-16/17 (optional follow-ups) deliberately not covered in this PR.

### Strategy intent

This story refactors validation logic out of two existing modules and adds a new CLI surface. Two failure modes dominate:

1. **Silent regression in `tea run`** — engine init used to perform these checks; the refactor must not change error codes, messages, or timing in a way that breaks downstream test assertions or user expectations. Defended by the **refactor parity suite** (`1.6-INT-013/014/015`) — every example and every broken fixture run through both surfaces with identical normalized error sets, plus a pre-/post-refactor message snapshot diff.
2. **Validator silently re-acquires side effects** — a future "shared helper" import from `yaml_engine` could re-introduce `exec()` of `run:` blocks, LTM connections, or secret reads, undoing AC-4/AC-5. Defended by the **import allow-list test** (`1.6-UNIT-002`) and the **sentinel-file test** (`1.6-INT-006`), reinforced by network and env-secret sentinels.

Logic-heavy AC-6 checks are pushed to the unit layer (one test per error category, no fixtures) for fast feedback. The CLI integration layer asserts wiring (exit codes, output format, sentinel side effects) against on-disk fixture YAMLs.

---

## Test Scenarios by Acceptance Criteria

### AC-1 — CLI command exists; exit 0 on success, 1 on validation errors

| ID            | Level       | Priority | Test                                                           | Justification                                                  |
| ------------- | ----------- | -------- | -------------------------------------------------------------- | -------------------------------------------------------------- |
| 1.6-INT-001   | Integration | P0       | `tea validate <valid.yaml>` exits 0                            | CLI wiring; happy-path exit-code contract (smoke + regression) |
| 1.6-INT-002   | Integration | P0       | `tea validate <broken.yaml>` exits 1                           | CLI wiring; failure-path exit-code contract                    |

### AC-2 (revised) — Success output: `OK: <file> (N nodes, M edges) [structural checks only — run blocks not executed]`

| ID            | Level       | Priority | Test                                                                        | Justification                                                              |
| ------------- | ----------- | -------- | --------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| 1.6-INT-003   | Integration | P0       | Success-message snapshot includes node/edge count + scope qualifier bracket | BUS-002 mitigation; user must not misread "validate passed" as "will run"  |

### AC-3 — Failure output: each error block with file path, line/col, formatted message; exit 1

| ID            | Level       | Priority | Test                                                                       | Justification                                          |
| ------------- | ----------- | -------- | -------------------------------------------------------------------------- | ------------------------------------------------------ |
| 1.6-UNIT-001  | Unit        | P1       | `format_error()` renders `{file, line, col, code, message}` block          | Pure formatting logic; isolated from CLI               |
| 1.6-INT-004   | Integration | P1       | Single-error YAML emits one block with file path + line/col where present  | End-to-end wiring of formatter into CLI output         |
| 1.6-INT-005   | Integration | P2       | Multi-error YAML emits one block per error (no concatenation, no clipping) | Output contract for CI consumers                       |

### AC-4 / AC-13 / AC-14 — No side effects; no LLM clients, secrets, LTM, run-block exec

| ID            | Level       | Priority | Test                                                                                                                                    | Justification                                                                              |
| ------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| 1.6-UNIT-002  | Unit        | P0       | After `import the_edge_agent.yaml_validation`, `sys.modules` does **not** contain `yaml_engine`, executor classes in `yaml_nodes`, `actions.*`, `backends.*`, `checkpointers*`, `parallel*` | AC-13 import allow-list — last automated guard against silent re-acquisition of side effects |
| 1.6-INT-006   | Integration | P0       | Sentinel-file fixture: `run:` block writes `/tmp/tea-validate-sentinel-<uuid>` → file MUST NOT exist after `tea validate`              | AC-14; SEC-001 mitigation; regression guard for accidental `exec()`                        |
| 1.6-INT-007   | Integration | P1       | Network sentinel: monkeypatch `socket.socket.__init__` to raise; `tea validate` exit 0 against valid fixture                            | Confirms no DNS/HTTP during validation                                                     |
| 1.6-INT-008   | Integration | P1       | Env-secret sentinel: wrap `os.environ.__getitem__` to track reads of `ANTHROPIC_API_KEY` / `OPENAI_API_KEY`; assert zero reads          | Confirms no secret backend init                                                            |

### AC-5 — No API keys or external services required

Covered by AC-4 sentinels (`1.6-INT-007`, `1.6-INT-008`) — no additional scenarios needed.

### AC-6 — v1 checks (one unit per error category; one CLI integration covers wiring)

| ID            | Level       | Priority | Test                                                                                | Justification                                                |
| ------------- | ----------- | -------- | ----------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| 1.6-UNIT-003  | Unit        | P0       | YAML parse error → `ValidationError` carries PyYAML's message + line/col            | Pure parsing wrapper                                         |
| 1.6-UNIT-004  | Unit        | P0       | Missing top-level `name` → error                                                    | Required-field check                                         |
| 1.6-UNIT-005  | Unit        | P0       | Missing top-level `nodes` → error                                                   | Required-field check                                         |
| 1.6-UNIT-006  | Unit        | P0       | Missing top-level `edges` → error                                                   | Required-field check                                         |
| 1.6-UNIT-007  | Unit        | P0       | Edge `from` referencing undefined node → error                                      | Edge endpoint check                                          |
| 1.6-UNIT-008  | Unit        | P0       | Edge `to` referencing undefined node → error                                        | Edge endpoint check                                          |
| 1.6-UNIT-009  | Unit        | P0       | Edge using `__start__` / `__end__` accepted (not flagged)                           | Negative case for edge endpoint check (false-positive guard) |
| 1.6-UNIT-010  | Unit        | P0       | `dynamic_parallel` with both `action` AND `steps` → mutual-exclusion error          | dynamic_parallel rule                                        |
| 1.6-UNIT-011  | Unit        | P0       | `dynamic_parallel` with neither `action`/`steps`/`subgraph` → error                 | dynamic_parallel rule                                        |
| 1.6-UNIT-012  | Unit        | P0       | `dynamic_parallel` with `action` only → valid                                       | Negative case (false-positive guard)                         |
| 1.6-UNIT-013  | Unit        | P0       | `dynamic_parallel` with `subgraph` only → valid                                     | Negative case (false-positive guard)                         |
| 1.6-UNIT-014  | Unit        | P0       | `dynamic_parallel` missing `fan_in` → error                                         | dynamic_parallel rule                                        |
| 1.6-UNIT-015  | Unit        | P0       | `dynamic_parallel` `fan_in` references undefined node → error                       | fan_in target check                                          |
| 1.6-UNIT-016  | Unit        | P0       | Duplicate node names → error (cites both line numbers)                              | Node-name uniqueness                                         |
| 1.6-UNIT-017  | Unit        | P0       | Invalid Jinja in `condition:` → error preserving Jinja's message                    | Jinja parse check (TEA-DX-001.5 helper)                      |
| 1.6-UNIT-018  | Unit        | P0       | Valid Jinja `condition:` → no error                                                 | Negative case (false-positive guard)                         |
| 1.6-INT-009   | Integration | P0       | End-to-end CLI: one fixture per AC-6 category exits 1 with formatted block          | AC-10 explicit requirement; CLI wiring per category          |

### AC-7 — `--strict` flag

| ID            | Level       | Priority | Test                                                                                                | Justification                                              |
| ------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| 1.6-UNIT-019  | Unit        | P1       | `--strict` promotes warnings to errors (exit 1 instead of 0)                                        | Strict-flag mechanic                                       |
| 1.6-UNIT-020  | Unit        | P1       | Without `--strict`, warnings emit but do not change exit code                                       | Default-mode contract                                      |
| 1.6-UNIT-021  | Unit        | P1       | Unreferenced node → emits unreferenced-node warning                                                 | One soft-warning rule                                      |
| 1.6-UNIT-022  | Unit        | P2       | Condition references state key never set by any node → emits dead-state-key warning (best-effort)   | TECH-003 — known-imprecise heuristic; documented limit     |
| 1.6-INT-010   | Integration | P1       | `--strict` true-positive suite: ≥5 broken YAMLs MUST warn → exit 1                                  | BUS-001 mitigation; TECH-003 — guards heuristic strength   |
| 1.6-INT-011   | Integration | P1       | `--strict` false-positive suite: ≥5 tricky-but-valid YAMLs MUST NOT warn → exit 0                   | BUS-001 mitigation; TECH-003 — guards heuristic precision  |

### AC-8 / AC-15 — Engine and validator share the same module; refactor parity

| ID            | Level       | Priority | Test                                                                                                                                                 | Justification                                                      |
| ------------- | ----------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------ |
| 1.6-INT-012   | Integration | P0       | Engine init invokes `yaml_validation.validate_workflow` (mock + assert called) for a representative fixture                                          | AC-8 wiring; mechanizes "shared module" contract                   |
| 1.6-INT-013   | Integration | P0       | **Refactor parity (examples):** every `examples/yaml/*.yaml` validated through both `tea validate` and engine init → identical normalized error set | AC-15; TECH-001 mitigation (the keystone test for the refactor)    |
| 1.6-INT-014   | Integration | P0       | **Refactor parity (broken fixtures):** every broken fixture (one per AC-6 + each `--strict` rule) validated through both surfaces → identical errors | AC-15; TECH-001 mitigation; ensures error codes/line/col agree     |
| 1.6-INT-015   | Integration | P0       | Engine-init error-message **snapshot diff** before vs after refactor for all existing engine tests; zero unintended diffs                            | TECH-001 / OPS-001 mitigation                                      |

### AC-9 — `run` semantics unchanged

| ID            | Level       | Priority | Test                                                                                          | Justification                                          |
| ------------- | ----------- | -------- | --------------------------------------------------------------------------------------------- | ------------------------------------------------------ |
| 1.6-INT-016   | Integration | P0       | Existing `tea run` golden-path tests pass unchanged (output, exit code, side effects)         | Regression guard against `run` semantics drift         |

### AC-10 — Tests cover valid + each error category + `--strict`

Covered by AC-1, AC-6, AC-7 scenarios above. No additional scenarios.

### AC-11 — Documentation

| ID            | Level       | Priority | Test                                                                              | Justification                                              |
| ------------- | ----------- | -------- | --------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| 1.6-UNIT-023  | Unit        | P2       | `docs/python/cli-reference.md` contains a `tea validate` section with one-line CI recipe | DoD requirement; lightweight content presence check        |
| 1.6-INT-017   | Integration | P2       | `tea validate examples/yaml/*.yaml` exits 0 for every shipped example             | DoD smoke; ensures shipped examples remain self-validating |

### AC-12 — `tea validate --help` describes checks and limitations

| ID            | Level       | Priority | Test                                                                                                                                                                       | Justification                                                              |
| ------------- | ----------- | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| 1.6-INT-018   | Integration | P1       | `tea validate --help` snapshot includes: list of checks performed; `[structural checks only — run blocks not executed]`; explicit non-checks (run-block semantics, runtime template errors) | BUS-002 + TECH-002 mitigation; AC-12 + revised AC-2 user-facing reinforcement |

### Performance (PERF-001 spot-check)

| ID            | Level       | Priority | Test                                                              | Justification                              |
| ------------- | ----------- | -------- | ----------------------------------------------------------------- | ------------------------------------------ |
| 1.6-INT-019   | Integration | P3       | 500-node synthetic workflow validates in <1s on CI runner (smoke) | PERF-001 risk is Minimal; smoke only       |

---

## Risk Coverage Map

| Risk ID  | Title                                                            | Priority | Covered by                                                            |
| -------- | ---------------------------------------------------------------- | -------- | --------------------------------------------------------------------- |
| TECH-001 | Refactor changes `tea run` error timing/message phase            | High     | 1.6-INT-013, 1.6-INT-014, 1.6-INT-015, 1.6-INT-016                    |
| BUS-001  | `--strict` false positives erode user trust                      | Medium   | 1.6-INT-010, 1.6-INT-011                                              |
| BUS-002  | Validator gives false confidence                                 | Medium   | 1.6-INT-003, 1.6-INT-018                                              |
| TECH-003 | `--strict` dead-state-key precision                              | Low      | 1.6-UNIT-022, 1.6-INT-010, 1.6-INT-011                                |
| SEC-001  | Accidental `exec()` of `run:` blocks                             | Low      | 1.6-UNIT-002 (allow-list), 1.6-INT-006 (sentinel)                     |
| OPS-001  | Engine error-timing shift breaks downstream test assertions      | Low      | 1.6-INT-015, 1.6-INT-016                                              |
| TECH-002 | Jinja parse misses runtime-only template errors                  | Low      | 1.6-INT-018 (documented limitation in `--help`)                       |
| PERF-001 | Slow validation on very large workflows                          | Minimal  | 1.6-INT-019                                                           |
| OPS-002  | CI-integration recipe under-documented                           | Minimal  | 1.6-UNIT-023                                                          |

All identified risks have at least one covering scenario. TECH-001 (the only High) has four.

---

## Test Data and Environment Requirements

### Fixture sets (under `python/tests/fixtures/`)

1. **`validate/valid/`** — minimum one fixture per "happy" assertion:
   - `minimal.yaml` (one node, one edge, no condition)
   - `dynamic_parallel_action.yaml`, `dynamic_parallel_subgraph.yaml`
   - `with_conditions.yaml` (valid Jinja conditions)
   - `start_end_edges.yaml` (uses `__start__` / `__end__`)
2. **`validate/broken/`** — one fixture per AC-6 error category:
   - `parse_error.yaml`, `missing_name.yaml`, `missing_nodes.yaml`, `missing_edges.yaml`
   - `edge_from_undefined.yaml`, `edge_to_undefined.yaml`
   - `dp_action_and_steps.yaml`, `dp_no_branch.yaml`, `dp_missing_fan_in.yaml`, `dp_fan_in_undefined.yaml`
   - `duplicate_nodes.yaml`
   - `invalid_jinja_condition.yaml`
3. **`validate/sentinel/`** — security-critical fixtures:
   - `run_block_writes_sentinel.yaml` (run: writes `/tmp/tea-validate-sentinel-<uuid>`; uuid generated per test invocation)
4. **`validate/strict_known_good/`** — ≥5 tricky-but-valid YAMLs that MUST NOT warn under `--strict` (false-positive suite for BUS-001):
   - `cond_using_only_initial_state.yaml`, `subgraph_locally_set_state.yaml`, `fan_in_only_referenced_in_dp.yaml`, `parallel_branch_state_keys.yaml`, `interrupt_resume_state_keys.yaml`
5. **`validate/strict_known_bad/`** — ≥5 broken YAMLs that MUST warn under `--strict` (true-positive suite for BUS-001):
   - `unreferenced_node.yaml`, `cond_uses_unset_key.yaml`, `dead_branch.yaml`, `unreferenced_subgraph.yaml`, `cond_uses_typo_key.yaml`
6. **`validate/perf/`** — generated in test setup:
   - `synthetic_500_nodes.yaml` (programmatically built; not committed)

### Snapshot files (under `python/tests/snapshots/`)

- `validate_help.txt` — `tea validate --help` output
- `validate_success.txt` — success-message format
- `validate_error_blocks/<category>.txt` — one per AC-6 category
- `engine_init_errors_pre_refactor.json` — captured before refactor; `engine_init_errors_post_refactor.json` diffed against it (1.6-INT-015)

### Environment

- **Python:** 3.10+ (project minimum)
- **Pytest plugins:** `pytest-snapshot` (or equivalent text-snapshot helper) for `--help` and error-block snapshots
- **No external services required** — that is the point of AC-4/AC-5; sentinel tests will fail loudly if anything tries.
- **CI:** standard `python/tests/` runner; perf scenario (1.6-INT-019) marked `@pytest.mark.slow` and runs in the same job (target <1s; not a hard gate).
- **OS:** sentinel test (1.6-INT-006) writes to `/tmp` — guard with `tempfile.gettempdir()` so it works on Windows runners; uuid-suffix prevents cross-test contamination.

### Mocks / monkeypatches

- 1.6-UNIT-002 — fresh-`sys.modules` baseline + assertion (use `importlib.reload` in isolated subinterpreter or subprocess to avoid pollution from prior tests).
- 1.6-INT-007 — monkeypatch `socket.socket.__init__` to `raise AssertionError`.
- 1.6-INT-008 — wrap `os.environ.__getitem__` (via `unittest.mock.patch.object`) to record keys read; assert intersection with secret-name allow-list is empty.
- 1.6-INT-012 — patch `yaml_validation.validate_workflow` and assert called once during engine init for the test fixture.

---

## Recommended Execution Order

1. **P0 unit tests** (`1.6-UNIT-002` through `1.6-UNIT-018`) — fail fast on any AC-6 logic regression or import-allow-list breach.
2. **P0 integration tests** — start with sentinel + parity:
   - `1.6-INT-006` (sentinel)
   - `1.6-INT-013`, `1.6-INT-014`, `1.6-INT-015` (refactor parity — the keystone TECH-001 defenses)
   - `1.6-INT-001`, `1.6-INT-002`, `1.6-INT-003`, `1.6-INT-009`, `1.6-INT-012`, `1.6-INT-016`
3. **P1 tests** — `--strict` suites and `--help` snapshot:
   - `1.6-UNIT-001`, `1.6-UNIT-019`, `1.6-UNIT-020`, `1.6-UNIT-021`
   - `1.6-INT-004`, `1.6-INT-007`, `1.6-INT-008`, `1.6-INT-010`, `1.6-INT-011`, `1.6-INT-018`
4. **P2 tests** — docs and shipped-example smoke:
   - `1.6-INT-005`, `1.6-INT-017`, `1.6-UNIT-022`, `1.6-UNIT-023`
5. **P3 tests** — perf spot-check:
   - `1.6-INT-019`

---

## Coverage Validation

- [x] Every in-scope AC has at least one covering scenario (AC-1…AC-15).
- [x] AC-16 / AC-17 (optional follow-ups) deliberately out of scope.
- [x] No duplicate coverage across levels — AC-6 logic tested at unit; CLI wiring at integration; only one overlap (`1.6-INT-009`) which exists deliberately to prove the unit-tested logic is reachable through the CLI.
- [x] Critical paths have multi-level defense — AC-4 covered by unit (allow-list), integration (sentinel-file, network, env-secret).
- [x] All risks (1 High, 2 Medium, 4 Low, 2 Minimal) have at least one covering scenario.
- [x] Test IDs follow `1.6-{LEVEL}-{SEQ}` convention.
- [x] Scenarios are atomic and independent.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 23
    integration: 19
    e2e: 0
  by_priority:
    p0: 27
    p1: 10
    p2: 4
    p3: 1
  coverage_gaps: []
  must_have_before_merge:
    - 1.6-UNIT-002   # AC-13 import allow-list
    - 1.6-INT-006    # AC-14 sentinel-file
    - 1.6-INT-013    # AC-15 refactor parity (examples)
    - 1.6-INT-014    # AC-15 refactor parity (broken fixtures)
    - 1.6-INT-015    # TECH-001 engine error-message snapshot diff
    - 1.6-INT-003    # BUS-002 success-message scope qualifier
    - 1.6-INT-018    # AC-12 + BUS-002 + TECH-002 --help limitations
  risk_coverage:
    TECH-001: [1.6-INT-013, 1.6-INT-014, 1.6-INT-015, 1.6-INT-016]
    BUS-001:  [1.6-INT-010, 1.6-INT-011]
    BUS-002:  [1.6-INT-003, 1.6-INT-018]
    TECH-003: [1.6-UNIT-022, 1.6-INT-010, 1.6-INT-011]
    SEC-001:  [1.6-UNIT-002, 1.6-INT-006]
    OPS-001:  [1.6-INT-015, 1.6-INT-016]
    TECH-002: [1.6-INT-018]
    PERF-001: [1.6-INT-019]
    OPS-002:  [1.6-UNIT-023]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.6-test-design-20260501.md
P0 tests identified: 27
Must-have-before-merge: 7 (1.6-UNIT-002, 1.6-INT-006, 1.6-INT-013, 1.6-INT-014, 1.6-INT-015, 1.6-INT-003, 1.6-INT-018)
```
