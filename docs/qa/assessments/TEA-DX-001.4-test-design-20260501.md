# Test Design: Story TEA-DX-001.4

**Story:** `variables` accessible in Python `run:` blocks
**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO

---

## Test Strategy Overview

- **Total test scenarios:** 13
- **Unit tests:** 9 (69%)
- **Integration tests:** 4 (31%)
- **E2E tests:** 0 (0%) — change is too narrow to warrant a full workflow E2E
- **Priority distribution:** P0: 4, P1: 5, P2: 3, P3: 1

The change under test is a single additive line at `python/src/the_edge_agent/yaml_nodes.py:715` injecting `engine.variables` into `exec_globals`. Test strategy is biased toward unit-level tests against `run_inline` semantics, with integration-level tests covering parallel-flow interactions where multiple engine subsystems collide (the only place where unit tests cannot give confidence).

Test design directly addresses the residual Reliability CONCERNS and TECH-006 risk surfaced in prior NFR/risk assessments by codifying parallel-thread non-determinism and parallel-process write invisibility as executable specifications.

---

## Test Scenarios by Acceptance Criteria

### AC-1 / AC-2: `variables` resolves in `run:`; both `[]` and `.get()` access work

| ID                  | Level | Priority | Test                                                                                                  | Justification                                  | Mitigates Risks |
|---------------------|-------|----------|-------------------------------------------------------------------------------------------------------|------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-001 | Unit  | P0       | Top-level `variables: {x: 5}` + `run:` returns `{"y": variables["x"] + 1}` → state contains `y: 6`.   | Pure scope-injection logic in `run_inline`.    | —               |
| TEA-DX-001.4-UNIT-002 | Unit  | P0       | Same as -001 but `run:` uses `variables.get("x", 0)` and `variables.get("missing", 99)`.              | Confirms dict-like API surface, default path. | —               |

### AC-3: Writes inside `run:` mutate `engine.variables`

| ID                    | Level       | Priority | Test                                                                                                                       | Justification                                       | Mitigates Risks |
|-----------------------|-------------|----------|----------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-003 | Unit        | P0       | `run:` does `variables["new"] = "v"`; assert `engine.variables["new"] == "v"` after node executes.                         | Direct mutation observable on engine instance.      | —               |
| TEA-DX-001.4-INT-001  | Integration | P0       | Two-node graph: node A writes `variables["new"] = "v"`; node B's Jinja `{{ variables.new }}` resolves to `"v"`.             | Cross-node visibility — engine + jinja + nodes.    | TECH-002        |

### AC-4: Existing Jinja `{{ variables.x }}` syntax continues to work unchanged

| ID                    | Level | Priority | Test                                                                                                            | Justification                                      | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------|----------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-004 | Unit  | P1       | Jinja-only node: `run:` body uses `result = "{{ variables.x }}"`; resolves to the literal value of `variables.x`. | Closes NFR gap: explicit Jinja non-regression.    | —               |

### AC-5: Kwarg-supplied `variables` overrides engine attribute

| ID                    | Level | Priority | Test                                                                                                                                                | Justification                                                  | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-005 | Unit  | P0       | Node with `with: {variables: {x: 99}}` while `engine.variables = {x: 5}`; `run:` returns `variables["x"]` → `99`.                                   | Validates kwarg precedence; placement of `**kwargs` in dict.   | TECH-001        |
| TEA-DX-001.4-UNIT-006 | Unit  | P2       | Node with `with: {variables: "not-a-dict"}`; `run:` accesses `variables` → resolves to the string. (Documents footgun, not a guard.)                | Edge case; confirms no implicit type coercion.                 | TECH-001        |

### AC-6: Existing example workflows continue to run

| ID                   | Level       | Priority | Test                                                                                                       | Justification                                | Mitigates Risks |
|----------------------|-------------|----------|------------------------------------------------------------------------------------------------------------|----------------------------------------------|-----------------|
| TEA-DX-001.4-INT-002 | Integration | P1       | Smoke-load every YAML in `examples/yaml/`; assert each compiles and (where deterministic) runs to `__end__`. | Catches name-collision regressions in real configs. | OPS-001 |

### AC-7: Lua/Prolog runtimes do not get `variables` injected

| ID                    | Level | Priority | Test                                                                                                                                                            | Justification                                                       | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-007 | Unit  | P3       | Lua `run:` referencing `variables` raises a Lua nil/undefined-name error (i.e., it is *not* injected into the Lua sandbox). Skip if `lupa` unavailable.        | Defensive: confirms scope of change is Python-only.                | —               |

### AC-8: Read test (already prescribed in story)

Covered by **TEA-DX-001.4-UNIT-001**.

### AC-9: Write test — mutation visible to subsequent node Jinja (already prescribed)

Covered by **TEA-DX-001.4-INT-001**.

### AC-10: Documentation updated

| ID                    | Level | Priority | Test                                                                                                                                                          | Justification                                            | Mitigates Risks |
|-----------------------|-------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|-----------------|
| TEA-DX-001.4-DOC-001  | Doc   | P2       | Reviewer verifies `docs/python/actions-reference.md` lists `variables` alongside `state`, `actions`, and includes a "Parallel safety" callout.              | Manual gate on PR; not automatable as pytest.            | OPS-001         |

### AC-11 (proposed): Parallel-safety documentation callout

Covered by **TEA-DX-001.4-DOC-001**.

### AC-12 (proposed): Parallel-thread race regression test

| ID                   | Level       | Priority | Test                                                                                                                                                                                  | Justification                                                                   | Mitigates Risks |
|----------------------|-------------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.4-INT-003 | Integration | P1       | Under `settings.parallel.strategy: thread`, two parallel branches each set `variables["x"] = <branch_id>`; fan-in reads `variables["x"]`; assert it is *one of* the two branch ids.  | Multi-component (parallel runner + engine state); codifies non-determinism.    | TECH-006        |

### AC-13 (proposed): Parallel-process invisibility regression test

| ID                   | Level       | Priority | Test                                                                                                                                                                                                       | Justification                                                                | Mitigates Risks |
|----------------------|-------------|----------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.4-INT-004 | Integration | P1       | Under `settings.parallel.strategy: process`, branch sets `variables["x"] = "from-branch"`; after fan-in, assert `engine.variables` does **not** contain `"x"` (or retains its original value).             | Codifies cross-process discard semantics; multi-process collaboration.       | TECH-006        |

### Defensive / cross-cutting

| ID                    | Level | Priority | Test                                                                                                                                                                            | Justification                                                       | Mitigates Risks |
|-----------------------|-------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|-----------------|
| TEA-DX-001.4-UNIT-008 | Unit  | P1       | YAML with no top-level `variables:` block; `run:` references `variables` → resolves to empty dict (`{}`), not `NameError`.                                                       | Confirms default-empty engine state still injects a usable scope.   | —               |
| TEA-DX-001.4-UNIT-009 | Unit  | P2       | `engine.variables` contains nested dict (`{"a": {"b": 1}}`); `run:` returns `variables["a"]["b"]` → `1`. No deep-copy boundary.                                                 | Confirms references (not copies) are passed; matches Jinja semantics. | SEC-001       |

---

## Coverage Matrix

| AC    | Test IDs                                    | Levels Covered      | Priorities |
|-------|---------------------------------------------|---------------------|------------|
| AC-1  | UNIT-001                                    | Unit                | P0         |
| AC-2  | UNIT-001, UNIT-002                          | Unit                | P0         |
| AC-3  | UNIT-003, INT-001                           | Unit + Integration  | P0         |
| AC-4  | UNIT-004                                    | Unit                | P1         |
| AC-5  | UNIT-005, UNIT-006                          | Unit                | P0, P2     |
| AC-6  | INT-002                                     | Integration         | P1         |
| AC-7  | UNIT-007                                    | Unit                | P3         |
| AC-8  | UNIT-001 (alias)                            | Unit                | P0         |
| AC-9  | INT-001 (alias)                             | Integration         | P0         |
| AC-10 | DOC-001                                     | Doc review          | P2         |
| AC-11 | DOC-001                                     | Doc review          | P2         |
| AC-12 | INT-003                                     | Integration         | P1         |
| AC-13 | INT-004                                     | Integration         | P1         |

**Coverage gaps:** None. Every AC (including proposed AC-11/12/13) maps to at least one scenario.

---

## Risk Coverage

| Risk ID  | Description                                                            | Mitigating Tests                            |
|----------|------------------------------------------------------------------------|---------------------------------------------|
| TECH-006 | Parallel flows racing on shared `engine.variables` writes              | INT-003 (thread race), INT-004 (process discard) |
| TECH-001 | Local/kwarg `variables` shadowing engine attribute confuses authors    | UNIT-005, UNIT-006                          |
| TECH-002 | In-block mutation visible across nodes (by design)                     | INT-001                                     |
| SEC-001  | Mutable engine state exposed in `exec()` sandbox                       | UNIT-009 (reference semantics)              |
| OPS-001  | Discoverability lost if docs not updated                               | DOC-001, INT-002                            |

---

## Test Data & Environment Requirements

### Fixtures
- **Minimal YAML factory:** helper that produces a `YAMLEngine` from an inline YAML string with configurable `variables:` block, a single `run:` node, and entry/finish wired to it. Most UNIT scenarios reuse this.
- **Two-node YAML factory:** entry → A → B → end, where A's `run:` and B's body are parameterized. Used by INT-001.
- **Parallel-fan-out factory:** entry → fan-out (N branches) → fan-in → end, with `settings.parallel.strategy` parameterized. Used by INT-003 and INT-004.

### Environment
- **Standard pytest path:** `cd python && pytest`. All UNIT and INT-001..002 run in default config.
- **INT-003 (thread race):** runs in default Python interpreter; assertion uses `assert result["x"] in {"branch-a", "branch-b"}` to codify non-determinism without flakiness. May warrant `@pytest.mark.flaky` reruns or repeated invocation (e.g., loop 20 times to confirm both outcomes are reachable) — preferred form is to assert membership only, not which branch wins.
- **INT-004 (process discard):** requires `multiprocessing` start-method `spawn` (default on macOS/Windows); test should `pytest.importorskip` if `process` strategy is unavailable in the test environment.
- **UNIT-007 (Lua):** `pytest.importorskip("lupa")`; skip cleanly when Lua runtime is absent.
- **DOC-001:** manual reviewer step in PR review; not a pytest run. Tracked in DoD checklist.

### Test data
- **Primitive variables:** `{x: 5}`, `{flag: true}`, `{name: "alice"}` for UNIT-001/002/008.
- **Nested:** `{a: {b: 1}}` for UNIT-009.
- **Type-edge:** `variables` overridden via `with:` to a non-dict string for UNIT-006.
- **Parallel marker values:** distinct ids per branch (`"branch-a"`, `"branch-b"`) for INT-003/004 to make discard/race observable.

### Suite hygiene
- Tests must reset `engine.variables` between runs (per-test fixture creates a fresh engine).
- No shared module-level state; parallel-test scenarios must not leak into other tests.

---

## Recommended Execution Order

1. **P0 unit:** UNIT-001, UNIT-002, UNIT-003, UNIT-005 (smallest blast radius, fastest feedback).
2. **P0 integration:** INT-001 (cross-node write visibility — the most likely real-world failure if AC-3 is implemented incorrectly).
3. **P1 unit:** UNIT-004, UNIT-008.
4. **P1 integration:** INT-002 (examples smoke), INT-003 (thread race), INT-004 (process discard).
5. **P2:** UNIT-006, UNIT-009, DOC-001.
6. **P3:** UNIT-007 (Lua) — informational.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 13
  by_level:
    unit: 9
    integration: 4
    e2e: 0
  by_priority:
    p0: 4
    p1: 5
    p2: 3
    p3: 1
  coverage_gaps: []
  risk_mitigations:
    TECH-006: [TEA-DX-001.4-INT-003, TEA-DX-001.4-INT-004]
    TECH-001: [TEA-DX-001.4-UNIT-005, TEA-DX-001.4-UNIT-006]
    TECH-002: [TEA-DX-001.4-INT-001]
    SEC-001:  [TEA-DX-001.4-UNIT-009]
    OPS-001:  [TEA-DX-001.4-DOC-001, TEA-DX-001.4-INT-002]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.4-test-design-20260501.md
P0 tests identified: 4
```

---

## Quality Checklist

- [x] Every AC has test coverage (incl. proposed AC-11/12/13)
- [x] Test levels appropriate (no over-testing — no E2E for a single-line scope change)
- [x] No duplicate coverage across levels (AC-8 = UNIT-001 alias; AC-9 = INT-001 alias)
- [x] Priorities align with risk profile (TECH-006 → P1 integration tests)
- [x] Test IDs follow `TEA-DX-001.4-{LEVEL}-{SEQ}` convention
- [x] Scenarios atomic and independent (no inter-test ordering dependencies)
