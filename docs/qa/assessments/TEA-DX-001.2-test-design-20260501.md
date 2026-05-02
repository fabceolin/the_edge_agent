# Test Design: Story TEA-DX-001.2 — CLI `--trace-file` flag

Date: 2026-05-01
Designer: Quinn (Test Architect) — `*test-design` (YOLO mode)
Story: [docs/stories/TEA-DX-001.2-cli-trace-file-flag.md](../../stories/TEA-DX-001.2-cli-trace-file-flag.md)
Inputs consulted: story ACs, in-story Risk Profile, in-story NFR Assessment.

## Test Strategy Overview

- **Total test scenarios:** 17
- **Unit:** 2 (12%)
- **Integration (CLI):** 14 (82%)
- **E2E:** 0 (0%) — story is a single-process CLI flag; no cross-system journey
- **Doc verification:** 1 (6%)
- **Priority distribution:** P0: 4, P1: 8, P2: 4, P3: 1
- **Test home:** `python/tests/test_cli_trace_file.py` (new file alongside `test_cli_fail_on_state.py`); engine-wiring unit added to `python/tests/test_yaml_engine.py` (existing). Follows `--output`/`--quiet`/`--fail-on-state` test pattern.

Rationale for level mix: the change is a thin Typer option wired to an existing `YAMLEngine(trace_file=...)` constructor kwarg (`yaml_engine.py:106`). Logic surface is tiny; the meaningful behavior is the **CLI-to-engine boundary** (option parsing → constructor → `_configure_from_settings` precedence → `FileExporter` write). That boundary is best covered by Typer `CliRunner` integration tests against `tea run`. Two unit tests carve out the pure-logic pieces (env-var expansion parity, constructor-vs-settings precedence) so we get fast feedback on the highest-risk items (R1, R4) without spinning up the CLI.

## Test Scenarios by Acceptance Criteria

### AC-1: `--trace-file` overrides `settings.trace_file`

| ID                    | Level | Priority | Test                                                                                                                          | Justification                                          | Mitigates |
| --------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------ | --------- |
| TEA-DX-001.2-INT-001  | Int   | **P0**   | YAML has `settings.trace_file: yaml.jsonl`; invoke `tea run agent.yaml --trace-file cli.jsonl`; assert `cli.jsonl` exists with JSONL events and `yaml.jsonl` was never created. | Locks the headline behavior end-to-end through Typer + engine + FileExporter. | R1        |
| TEA-DX-001.2-UNIT-002 | Unit  | **P0**   | `YAMLEngine(trace_file="cli.jsonl")` constructed against a parsed YAML dict whose `settings.trace_file` is `"yaml.jsonl"` — assert installed exporter targets `cli.jsonl` after `_configure_from_settings` (`yaml_engine.py:980-1004`) runs. | Pins R1: confirms `_configure_from_settings` does not clobber a constructor-supplied path; cheap to run, fails first if precedence regresses. | R1        |

### AC-2: Implicit exporter promotion (`unset`/`console` → `file`) when `--trace-file` set

| ID                   | Level | Priority | Test                                                                                                            | Justification                                              | Mitigates |
| -------------------- | ----- | -------- | --------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-002 | Int   | **P1**   | YAML omits `settings.trace_exporter`; `--trace-file out.jsonl`; assert `out.jsonl` populated with JSONL trace events. | Covers "exporter unset" cell of R6 matrix.                | R2, R6    |
| TEA-DX-001.2-INT-003 | Int   | **P1**   | YAML sets `settings.trace_exporter: console`; `--trace-file out.jsonl`; assert exporter is now `file` and `out.jsonl` is populated. (Pin documented behavior re: console: either suppressed or dual-output — assert whichever is implemented and document.) | Covers "console → file" promotion cell.                    | R2, R6    |

### AC-3: Exporter preserved when already `file`; only path is overridden

| ID                   | Level | Priority | Test                                                                                                                                       | Justification                              | Mitigates |
| -------------------- | ----- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------ | --------- |
| TEA-DX-001.2-INT-004 | Int   | **P1**   | YAML sets `settings.trace_exporter: file` and `settings.trace_file: yaml.jsonl`; run with `--trace-file cli.jsonl`; assert exporter type is unchanged and only path moves to `cli.jsonl`. | Negative-control for INT-002/003 promotion logic. | R6        |

### AC-4: Implicit `auto_trace=true` when `--trace-file` is supplied and YAML opts out

| ID                   | Level | Priority | Test                                                                                                                | Justification                                                          | Mitigates |
| -------------------- | ----- | -------- | ------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-005 | Int   | **P0**   | YAML sets `settings.auto_trace: false`; `--trace-file out.jsonl`; assert `out.jsonl` is created and contains JSONL events. | Implicit-enable is the single most surprising behavior of this story; must be exercised end-to-end. | R2, R6    |

### AC-5: `${ENV_VAR}` expansion via `expand_env_vars`

| ID                    | Level | Priority | Test                                                                                                                              | Justification                                                                              | Mitigates |
| --------------------- | ----- | -------- | --------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | --------- |
| TEA-DX-001.2-UNIT-001 | Unit  | **P1**   | Direct call to whatever helper the CLI uses (or unit-level helper introduced for this story) on `${TRACE_DIR}/run.jsonl` with `TRACE_DIR=/tmp/foo` resolves to `/tmp/foo/run.jsonl`; also assert `${MISSING:-default}` semantics match TEA-DX-001.1. | Unit-level pin of expansion logic; fast, decoupled from Typer. | R4        |
| TEA-DX-001.2-INT-006  | Int   | **P1**   | Parity assertion: same string `${TRACE_DIR}/run.jsonl` produces an identical resolved path whether supplied via YAML `settings.trace_file` (TEA-DX-001.1 path) or CLI `--trace-file`. Run both and compare resolved trace file location. | End-to-end parity guard; protects against drift between two expansion call sites. | R4        |

### AC-6: `--help` shows `--trace-file` with one-line description

| ID                   | Level | Priority | Test                                                                                                                          | Justification                                              | Mitigates |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-007 | Int   | **P1**   | `CliRunner().invoke(app, ["run", "--help"])` exits 0 and stdout contains both the literal string `--trace-file` and a non-empty description fragment. | Mechanical AC-6 enforcement (NFR T4); cheap CI guard.       | R7        |

### AC-7: Behavior unchanged when `--trace-file` not provided

| ID                   | Level | Priority | Test                                                                                                                              | Justification                                                                                  | Mitigates |
| -------------------- | ----- | -------- | --------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-008 | Int   | **P0**   | Run the same fixture YAML without `--trace-file`; assert exit code, stdout (with and without `--quiet`), and trace-file behavior match the pre-change baseline (snapshot or comparison against an explicit golden). | Regression guard for the entire backward-compatibility surface. P0 because external runners depend on no-flag stability. | R1        |

### AC-8: Works with `--quiet`, `--stream`, `--show-graph`

| ID                   | Level | Priority | Test                                                                                                                  | Justification                              | Mitigates |
| -------------------- | ----- | -------- | --------------------------------------------------------------------------------------------------------------------- | ------------------------------------------ | --------- |
| TEA-DX-001.2-INT-009 | Int   | **P2**   | `--trace-file out.jsonl --quiet` → trace file populated, stdout suppressed, exit 0.                                   | Mode interaction with quiet (most common). | —         |
| TEA-DX-001.2-INT-010 | Int   | **P2**   | `--trace-file out.jsonl --stream` → trace file populated, streaming output unaffected.                                | Mode interaction with stream.              | —         |
| TEA-DX-001.2-INT-011 | Int   | **P2**   | `--trace-file out.jsonl --show-graph` → trace file populated, graph rendering unaffected.                             | Mode interaction with graph view.          | —         |

### AC-9: No interaction with `--output`

| ID                   | Level | Priority | Test                                                                                                                      | Justification                                                       | Mitigates |
| -------------------- | ----- | -------- | ------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-012 | Int   | **P2**   | Run with `--trace-file traces.jsonl --output state.json`; assert both files are produced, contents are distinct (JSONL vs final state JSON), neither contains the other's payload. | Pins independence of two file-emitting flags.                       | —         |

### AC-10: Test coverage on flag override / implicit-enable / env-var / missing-flag

Implicitly satisfied by TEA-DX-001.2-INT-001, INT-002, INT-005, UNIT-001, INT-008. No additional dedicated scenarios needed.

### AC-11: CLI reference documentation

| ID                   | Level | Priority | Test                                                                                                                                                                            | Justification                                                                              | Mitigates |
| -------------------- | ----- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | --------- |
| TEA-DX-001.2-DOC-001 | Doc   | **P2**   | Manual verification (or `grep`-based CI assertion) that `docs/python/` CLI reference includes `--trace-file` with the precedence note: "CLI `--trace-file` overrides `settings.trace_file`. When set, implicitly enables `auto_trace=true` and switches `trace_exporter` to `file` if unset or `console`." | Pins R7 (docs precedence ambiguity). Lightweight grep is enough; no need for a full snapshot. | R7        |

### AC-12 (recommended addition from NFR): Clean error on bad path

| ID                   | Level | Priority | Test                                                                                                                                                  | Justification                                                                                                              | Mitigates |
| -------------------- | ----- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | --------- |
| TEA-DX-001.2-INT-013 | Int   | **P1**   | `--trace-file /nonexistent/dir/foo.jsonl` exits non-zero with stderr containing the literal `--trace-file` and **not** containing `Traceback (most recent call last)`. | NFR T1 promoted from P2 → P1 because external runners parse stderr. Closes Reliability CONCERNS in NFR assessment.         | R5        |

### Determinism (recommended P3 from risk profile)

| ID                   | Level | Priority | Test                                                                                                                            | Justification                              | Mitigates |
| -------------------- | ----- | -------- | ------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------ | --------- |
| TEA-DX-001.2-INT-014 | Int   | **P3**   | Two consecutive runs with same `--trace-file out.jsonl` — assert documented FileExporter semantics (overwrite vs append); pin whichever is current. | Locks current behavior to prevent silent drift. | —         |

## Risk Coverage

Mapping of identified risks (from in-story Risk Profile) to test scenarios:

| Risk | Description                                          | Severity | Covered by                                                |
| ---- | ---------------------------------------------------- | -------- | --------------------------------------------------------- |
| R1   | CLI override does not deterministically beat YAML    | Medium   | INT-001 (P0), UNIT-002 (P0), INT-008 (P0)                 |
| R2   | Implicit-enable surprise                             | Medium   | INT-002 (P1), INT-003 (P1), INT-005 (P0), INT-007 (P1, help text), DOC-001 (P2) |
| R3   | Path traversal / write to sensitive locations        | Low      | INT-013 (P1) — bounded; OS perms are sufficient mitigation |
| R4   | Env-var expansion drift vs TEA-DX-001.1              | Low      | UNIT-001 (P1), INT-006 (P1)                               |
| R5   | Opaque traceback on bad path                         | Low      | INT-013 (P1)                                              |
| R6   | Combinatorial implicit-enable matrix under-tested    | Medium   | INT-002 + INT-003 + INT-004 + INT-005 (covers exporter × auto_trace cells) |
| R7   | Docs precedence ambiguity                            | Low      | INT-007 (P1, help text), DOC-001 (P2, full ref)           |

All Medium risks have at least one P0 or two P1 scenarios. No risks left without a test.

## NFR Coverage

Mapping to in-story NFR Assessment recommendations:

| NFR Test (NFR doc)                                               | Mapped scenario              | Priority resolution            |
| ---------------------------------------------------------------- | ---------------------------- | ------------------------------ |
| T1: Bad path → typer.BadParameter                                | INT-013                      | **Promoted P2 → P1** (per NFR) |
| T2: Unwritable parent dir → clean error                          | INT-013 (extend with chmod-based variant) or follow-on P2 | P2                  |
| T3: Env-var parity                                               | INT-006                      | P1                             |
| T4: `--help` snapshot                                            | INT-007                      | P1                             |
| T5: No-flag baseline byte-identical with `--quiet`/`--stream`    | INT-008 + INT-009 + INT-010  | P0                             |
| T6: Invocation-overhead delta <5%                                | Skipped (per NFR doc)        | P3 / out of scope              |

## Recommended Execution Order

1. **P0 unit:** UNIT-002 (engine wiring precedence)
2. **P0 integration:** INT-001 (override), INT-005 (implicit-enable from off), INT-008 (no-flag baseline)
3. **P1 integration:** INT-002, INT-003, INT-004, INT-006, INT-007, INT-013
4. **P1 unit:** UNIT-001 (env-var helper)
5. **P2 integration:** INT-009, INT-010, INT-011, INT-012, DOC-001
6. **P3 integration:** INT-014

P0 unit → P0 integration → P1 → P2 → P3, fail-fast preferred. Total wall-clock budget for the suite is small (all CLI tests; no external services).

## Test Data & Environment Requirements

- **Fixture YAMLs** under `python/tests/fixtures/dx_001_2/`:
  - `simple.yaml` — minimal one-node graph, no `settings:` block.
  - `auto_trace_off.yaml` — `settings.auto_trace: false`, no `trace_file`.
  - `console_exporter.yaml` — `settings.trace_exporter: console`.
  - `file_exporter_with_path.yaml` — `settings.trace_exporter: file` + `settings.trace_file: yaml.jsonl`.
  - `env_var_path.yaml` — `settings.trace_file: ${TRACE_DIR}/yaml.jsonl` for parity test.
- **Tooling:** `typer.testing.CliRunner`, `pytest`'s `tmp_path` for trace destinations, `monkeypatch.setenv` for `TRACE_DIR` / `MISSING` env vars, `caplog` for stderr assertions on INT-013, `chmod 0o500` (or `pyfakefs`) for INT-013's unwritable variant.
- **Environment isolation:** every test uses `tmp_path` for `--trace-file` and `--output` destinations; no shared filesystem state. Tests must NOT touch `/etc/`, `/tmp/foo` directly, or any path outside `tmp_path` except the documented "nonexistent" path in INT-013 which never gets created.
- **Determinism:** mock or freeze trace timestamps if asserting exact JSONL content; otherwise assert `len(events) > 0` and well-formed JSONL (one JSON object per line).
- **No external services:** zero LLM/DB/network usage. Suite must remain hermetic.

## Coverage Validation

- [x] Every AC (AC-1 through AC-11) has at least one test scenario.
- [x] Recommended AC-12 has a P1 scenario (INT-013).
- [x] No duplicate coverage across levels — UNIT-001/UNIT-002 carve out logic that is **not** re-asserted by integration tests (UNIT-002 isolates `_configure_from_settings`; INT-001 covers the same precedence end-to-end, which is intentional defense-in-depth on R1).
- [x] All risks (R1–R7) mapped to a test.
- [x] All NFR T1–T5 recommendations mapped.
- [x] All test IDs follow `TEA-DX-001.2-{LEVEL}-{NNN}` convention.
- [x] Scenarios are atomic (one assertion focus each) and independent (each owns its `tmp_path`).

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 17
  by_level:
    unit: 2
    integration: 14
    doc: 1
    e2e: 0
  by_priority:
    p0: 4
    p1: 8
    p2: 4
    p3: 1
  coverage_gaps: []  # AC-1..AC-11 + recommended AC-12 all covered
  risks_covered: [R1, R2, R3, R4, R5, R6, R7]
  nfr_tests_covered: [T1, T3, T4, T5]   # T2 deferred P2; T6 explicitly skipped
```

## Trace References

- Test design matrix: `docs/qa/assessments/TEA-DX-001.2-test-design-20260501.md`
- P0 tests identified: 4 (UNIT-002, INT-001, INT-005, INT-008)
- Source paths referenced: `python/src/the_edge_agent/cli.py` (run command), `python/src/the_edge_agent/yaml_engine.py:106` (constructor), `python/src/the_edge_agent/yaml_engine.py:980-1004` (`_configure_from_settings`).

## Sign-off

Test design covers all 11 declared ACs plus the NFR-recommended AC-12, with explicit risk and NFR traceability. P0 set is small (4 tests) and fast — safe gate criterion is "P0 + P1 green, P2 best-effort." No blocking gaps.
