# TEA-DX-001.4: `variables` accessible in Python `run:` blocks

## Status
Done

## Parent Epic
[TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md)

## Priority
Medium

---

## Story

**As a** YAML workflow author writing Python `run:` blocks,
**I want** to read engine `variables` directly as a Python dict (not via Jinja string interpolation),
**so that** I can write `variables.get("foo", N)` instead of `{{ variables.foo | default(N) }}`-interpolated into Python source.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/yaml_nodes.py:710-768` (`run_inline` function — builds `exec_globals` for inline Python `run:` execution)
- Engine attribute: `YAMLEngine.variables` (declared `yaml_engine.py:488`, populated `yaml_engine.py:892` from `config.get("variables", {})`)
- Already passed to: Jinja template context (`yaml_engine.py:533` — `variables=self.variables`)
- Technology: Python `exec()`, Jinja2

**Problem Statement:**

Today, `variables:` defined at the top of a YAML are accessible only via Jinja:

```yaml
variables:
  max_retries: 3

nodes:
  - name: do_thing
    run: |
      # Forced to interpolate via Jinja into Python source — ugly
      retries = {{ variables.max_retries | default(3) }}
      for _ in range(retries):
          ...
```

The Jinja-into-Python pattern is fragile (string escaping, syntax highlighting in editors, type loss on complex values). The engine already has `self.variables` populated; it just isn't in the `exec_globals` dict at `yaml_nodes.py:715`.

## Acceptance Criteria

**Functional Requirements:**

1. **AC-1:** Inside a Python `run:` block, the name `variables` resolves to the engine's `variables` dict (`engine.variables`).
2. **AC-2:** Reading: `retries = variables["max_retries"]` and `retries = variables.get("max_retries", 3)` both work.
3. **AC-3:** Writing: `variables["foo"] = "bar"` mutates the engine's variables dict (matches existing Jinja-context semantics where variables are read-write per the `extraction_prompt` precedent at `yaml_engine.py:1263`).
4. **AC-4:** Existing Jinja `{{ variables.x }}` syntax continues to work unchanged.

**Integration Requirements:**

5. **AC-5:** No name collision: if a node `with:` mapping passes a kwarg literally named `variables`, the kwarg wins (preserves existing kwarg precedence). Document this edge case.
6. **AC-6:** Existing example workflows in `examples/yaml/` continue to run unchanged.
7. **AC-7:** No change to `run:` blocks executed via Lua/Prolog runtimes (Python-only).

**Quality Requirements:**

8. **AC-8:** Unit test: YAML with top-level `variables: {x: 5}` and a `run:` block that returns `{"y": variables["x"] + 1}` produces state `{"y": 6}`.
9. **AC-9:** Unit test: write inside `run:` (`variables["new"] = "v"`) is observable in subsequent nodes' Jinja templating.
10. **AC-10:** Documentation updated in `docs/python/actions-reference.md` (or equivalent) to list `variables` alongside `state`, `actions`, etc.

## Technical Notes

- **Integration Approach:** Add one line to `yaml_nodes.py:715`:
  ```python
  exec_globals = {
      "state": state,
      "variables": engine.variables,   # <-- new
      **kwargs,
      "json": json,
      ...
  }
  ```
  Note that `**kwargs` comes after, so a `with:`-supplied `variables` kwarg overrides the engine attribute (preserves AC-5 semantics).
- **Existing Pattern Reference:** `yaml_engine.py:533` — `variables=self.variables` already passed to Jinja.
- **Key Constraint:** Inserting `variables` *before* `**kwargs` matters for kwarg precedence. Keep that order.

## Tasks / Subtasks

- [x] **Task 1: Add `variables` to `exec_globals`** (AC: 1, 2, 3)
  - [x] Modify `yaml_nodes.py:715` to inject `engine.variables`
  - [x] Confirm placement before `**kwargs` for AC-5 semantics
- [x] **Task 2: Tests** (AC: 4, 5, 7, 8, 9, 12, 13)
  - [x] Read test (UNIT-001/002 — AC-1, AC-2, AC-8)
  - [x] Write test (UNIT-003 — AC-3, AC-9 / INT-001 alias)
  - [x] Jinja non-regression test (UNIT-004 — AC-4)
  - [x] Kwarg-override test (UNIT-005 — AC-5)
  - [x] Non-dict kwarg pass-through (UNIT-006 — AC-5 footgun)
  - [x] Lua isolation test (UNIT-007 — AC-7; skips when `lupa` not installed)
  - [x] Defensive: missing `variables:` block → empty dict (UNIT-008)
  - [x] Defensive: nested dict access (UNIT-009 — SEC-001)
  - [x] Parallel thread race (INT-003 — AC-12 / TECH-006)
  - [x] Parallel process discard (INT-004 — AC-13 / cross-process invisibility)
- [x] **Task 3: Docs** (AC: 10, 11)
  - [x] Document the new `run:`-scope name in `docs/python/actions-reference.md`
  - [x] Add parallel-safety callout (thread vs process/remote)

## Definition of Done

- [x] All ACs met (10 original + AC-11/12/13 from NFR review)
- [x] TEA-DX-001.4 test files green (`tests/test_dx_001_4_variables_in_run.py`, `tests/test_yaml_run_variables_parallel.py`)
- [x] Targeted regression run green: `tests/test_yaml_engine_*.py`, `tests/test_yaml_dynamic_parallel.py`, `tests/test_parallel_integration.py` (200 passed, 1 skipped)
- [x] No regression in `examples/yaml/` workflow loading (12/13 OK; the single `stream_broadcast.yaml` failure is pre-existing on `main` and unrelated to this change — verified via `git stash` smoke test)
- [x] Docs updated (`docs/python/actions-reference.md` § Run Block Globals)

## Risk and Compatibility

- **Primary Risk:** A workflow currently relying on the `NameError` for `variables` inside a `run:` block (e.g., a try/except probing whether it's defined) would break. Extremely unlikely; not a documented pattern.
- **Rollback:** Revert the single line.
- **Compatibility:** Fully additive — adds a new name to the exec scope.

---

## QA Notes - Risk Profile

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full assessment:** [`docs/qa/assessments/TEA-DX-001.4-risk-20260501.md`](../qa/assessments/TEA-DX-001.4-risk-20260501.md)

### Risk Level
**Overall: 94/100 — Very Low Risk · Recommended Gate: PASS**

5 risks identified, none Critical/High/Medium. The change is a single additive line at `yaml_nodes.py:715` mirroring an existing Jinja-context pattern (`yaml_engine.py:533`).

### Identified Risks

| ID       | Category | Risk                                                                | Score | Priority |
|----------|----------|---------------------------------------------------------------------|-------|----------|
| TECH-006 | Tech     | Parallel flows racing on shared `engine.variables` writes           | 2     | Low      |
| TECH-001 | Tech     | Local/kwarg `variables` shadowing engine attribute confuses authors | 1     | Minimal  |
| TECH-002 | Tech     | In-block mutation visible across nodes (matches AC-3 by design)     | 1     | Minimal  |
| SEC-001  | Security | Mutable engine state exposed in `exec()` sandbox                    | 1     | Minimal  |
| OPS-001  | Ops      | Discoverability lost if `actions-reference.md` not updated (AC-10)  | 1     | Minimal  |

### Mitigations

- **TECH-006 (only non-minimal risk):** Documentation must call out that `variables` is shared by reference; parallel-flow writes race; use `state` for per-flow data. Add a regression test demonstrating the non-determinism.
- **TECH-001:** AC-5 + the kwarg-override unit test in Task 2 already cover this; reinforce in docs.
- **TECH-002:** Accepted by design (AC-3); AC-9's write-visibility test serves as executable documentation.
- **SEC-001:** No new attack surface — `run:` already executes arbitrary Python; trust boundary is the YAML source itself.
- **OPS-001:** DoD already gates on docs update; reviewer to verify in PR.

### Testing Priorities

**P1 — Must have**
1. Read test (AC-8): `variables["x"]` accessible in `run:`.
2. Write test (AC-9): mutation visible to next node's Jinja.
3. Kwarg-override test (AC-5): `with: {variables: ...}` shadows engine attribute.
4. **New (TECH-006):** Parallel branches both write `variables["x"]`; fan-in reads it; assert value is *one of* the writes (not which).

**P2 — Regression**
- Full `pytest python/tests/` suite green.
- Smoke-run `examples/yaml/*.yaml` (AC-6).

**P3 — Defensive**
- Lua/Prolog runtime test asserting `variables` is *not* injected (AC-7).

---

## QA Notes - NFR Assessment

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO (core four)
**Full assessment:** [`docs/qa/assessments/TEA-DX-001.4-nfr-20260501.md`](../qa/assessments/TEA-DX-001.4-nfr-20260501.md)

### NFR Coverage

| NFR | Status | Quality Score Impact |
|-----|--------|----------------------|
| Security | **PASS** | 0 |
| Performance | **PASS** | 0 |
| Reliability | **CONCERNS** | −10 |
| Maintainability | **PASS** | 0 |

**Quality Score: 90/100**

### Gate YAML Block

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'No new attack surface — run: already executes arbitrary Python; trust boundary is the YAML source.'
  performance:
    status: PASS
    notes: 'O(1) dict insertion per node invocation; no I/O or copy.'
  reliability:
    status: CONCERNS
    notes: 'engine.variables shared by reference across parallel flows (race on writes). Cross-process parallel strategy: mutations invisible. Needs doc warning + regression tests.'
  maintainability:
    status: PASS
    notes: 'Single-line change mirroring yaml_engine.py:533. ACs require read/write tests, kwarg-override test, docs update.'
```

### Missing Considerations

The risk profile flagged TECH-006 (parallel race), but the NFR review surfaces a second, harder failure mode that is not yet captured in any AC:

1. **Cross-process invisibility.** `settings.parallel.strategy` supports `"thread"`, `"process"`, and `"remote"` (`yaml_engine.py:1325`). Under `process`/`remote`, the worker has its own copy of `engine.variables`; in-block writes are **silently discarded** at fan-in. Authors who use the new write-through capability (AC-3) inside a process-mode parallel branch will get incorrect behavior with no error.
2. **AC-10 doc scope.** Currently asks only for "list `variables` alongside `state`, `actions`". Should be extended to document the shared-reference semantics and the cross-process discard.
3. **AC-4 (Jinja non-regression).** Currently covered only implicitly via AC-6 (examples regression). Recommend an explicit unit test that `{{ variables.x }}` continues to resolve once the `exec_globals` change is in place.

### Test Recommendations

**P0 — Must have (already in story)**
- Read test (AC-8): `variables["x"]` accessible in `run:`.
- Write-visibility test (AC-9): mutation visible to next node's Jinja.
- Kwarg-override test (AC-5): `with: {variables: ...}` shadows engine attribute.

**P1 — Close the Reliability CONCERNS**
- **Parallel-thread race test (TECH-006):** two branches both write `variables["x"]`; fan-in reads it; assert value is *one of* the writes. Codifies non-determinism.
- **Parallel-process invisibility test (new):** under `settings.parallel.strategy: process`, write `variables["x"]` in a branch; assert at fan-in that `engine.variables["x"]` is **not** updated. Codifies the discard.

**P2 — Regression**
- Full `pytest python/tests/` suite green.
- Smoke-run `examples/yaml/*.yaml` (AC-6).
- Explicit Jinja `{{ variables.x }}` non-regression unit test (firms up AC-4).

**P3 — Defensive**
- Lua/Prolog runtime test asserting `variables` is *not* injected (AC-7).

### Acceptance Criteria Recommendations

The existing 10 ACs adequately cover the functional and integration scope. To close the Reliability CONCERNS, the story should add:

- **AC-11 (proposed):** Documentation in `actions-reference.md` includes a "Parallel safety" callout describing (a) shared-reference semantics in thread mode and (b) cross-process invisibility under `process`/`remote` parallel strategies. Recommend `state` for per-flow data.
- **AC-12 (proposed):** Regression test demonstrating parallel-thread writes to `variables` produce a non-deterministic but bounded fan-in value.
- **AC-13 (proposed):** Regression test demonstrating that under `settings.parallel.strategy: process`, in-branch writes to `variables` are not visible at fan-in. Either document this as expected, or escalate to a runtime warning.

Net effort to lift Reliability → PASS: ~2.5 hours (one doc paragraph + two pytest cases).

### Recommended Gate Decision

**CONCERNS** — gate may pass with waiver if the team accepts the parallel-safety documentation gap as known and tracked. **Preferred path:** add AC-11 + AC-12 + AC-13 (or equivalent) and re-gate to PASS. The change itself remains very low risk (94/100 from the risk profile); the NFR concern is a documentation/test completeness issue, not an implementation defect.

---

## QA Notes - Test Design

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full assessment:** [`docs/qa/assessments/TEA-DX-001.4-test-design-20260501.md`](../qa/assessments/TEA-DX-001.4-test-design-20260501.md)

### Strategy Summary

- **13 scenarios** total — 9 Unit (69%), 4 Integration (31%), 0 E2E (single-line scope change does not warrant a full workflow E2E).
- **Priority distribution:** P0: 4, P1: 5, P2: 3, P3: 1.
- Strategy is biased toward unit tests against `run_inline`, with integration tests reserved for cross-node and parallel-flow interactions where unit-level confidence is insufficient.
- Tests close the Reliability CONCERNS from the NFR assessment: INT-003 (thread race) and INT-004 (process discard) codify TECH-006.

### Test Coverage Matrix

| AC    | Test IDs                              | Levels             | Priorities | Notes                                          |
|-------|---------------------------------------|--------------------|------------|------------------------------------------------|
| AC-1  | UNIT-001                              | Unit               | P0         | `variables` resolves in `run:`                 |
| AC-2  | UNIT-001, UNIT-002                    | Unit               | P0         | `[]` and `.get()` access                       |
| AC-3  | UNIT-003, INT-001                     | Unit + Integration | P0         | Mutation observable on engine and next node    |
| AC-4  | UNIT-004                              | Unit               | P1         | Explicit Jinja non-regression                  |
| AC-5  | UNIT-005, UNIT-006                    | Unit               | P0, P2     | Kwarg precedence + non-dict footgun            |
| AC-6  | INT-002                               | Integration        | P1         | Examples smoke                                 |
| AC-7  | UNIT-007                              | Unit               | P3         | Lua runtime not affected (skip if no `lupa`)   |
| AC-8  | UNIT-001 (alias)                      | Unit               | P0         | Read test                                      |
| AC-9  | INT-001 (alias)                       | Integration        | P0         | Write visible to next node's Jinja             |
| AC-10 | DOC-001                               | Doc review         | P2         | `actions-reference.md` lists `variables`       |
| AC-11 | DOC-001                               | Doc review         | P2         | Parallel-safety callout in docs                |
| AC-12 | INT-003                               | Integration        | P1         | Thread race — non-determinism codified         |
| AC-13 | INT-004                               | Integration        | P1         | Process discard — invisibility codified        |

**Coverage gaps:** None.

### Scenarios with Expected Results

#### AC-1 / AC-2 — Read access (P0)

| ID         | Setup                                                               | Action                                                  | Expected                                              |
|------------|---------------------------------------------------------------------|---------------------------------------------------------|-------------------------------------------------------|
| UNIT-001   | YAML with `variables: {x: 5}`, single `run:` node                   | `return {"y": variables["x"] + 1}`                       | State after node = `{..., "y": 6}`                    |
| UNIT-002   | Same YAML                                                            | `return {"y": variables.get("x", 0), "z": variables.get("missing", 99)}` | State = `{..., "y": 5, "z": 99}` |

#### AC-3 — Write semantics (P0)

| ID       | Setup                                            | Action                                            | Expected                                                              |
|----------|--------------------------------------------------|---------------------------------------------------|-----------------------------------------------------------------------|
| UNIT-003 | YAML with `variables: {x: 5}`, single `run:`     | `variables["new"] = "v"; return {}`               | After execution, `engine.variables["new"] == "v"`                     |
| INT-001  | Two-node YAML; A is Python, B's body has Jinja   | A: `variables["new"] = "v"`; B: `result = "{{ variables.new }}"` | State after B contains `result == "v"`                |

#### AC-4 — Jinja non-regression (P1)

| ID       | Setup                                       | Action                                          | Expected                                              |
|----------|---------------------------------------------|-------------------------------------------------|-------------------------------------------------------|
| UNIT-004 | YAML with `variables: {x: "hello"}`         | `result = "{{ variables.x }}"; return {"r": result}` | State `r == "hello"` (Jinja still resolves)      |

#### AC-5 — Kwarg precedence (P0 / P2)

| ID       | Setup                                                                 | Action                          | Expected                                       |
|----------|-----------------------------------------------------------------------|---------------------------------|------------------------------------------------|
| UNIT-005 | `engine.variables = {x: 5}`; node `with: {variables: {x: 99}}`        | `return {"v": variables["x"]}` | State `v == 99` (kwarg wins)                   |
| UNIT-006 | Node `with: {variables: "not-a-dict"}`                                | `return {"t": type(variables).__name__}` | State `t == "str"` (no coercion)     |

#### AC-6 — Examples regression (P1)

| ID       | Setup                                  | Action                                 | Expected                                              |
|----------|----------------------------------------|----------------------------------------|-------------------------------------------------------|
| INT-002  | Iterate every YAML in `examples/yaml/` | Compile + (where deterministic) invoke | All compile; deterministic ones run to `__end__` clean |

#### AC-7 — Lua isolation (P3)

| ID       | Setup                                  | Action                                       | Expected                                              |
|----------|----------------------------------------|----------------------------------------------|-------------------------------------------------------|
| UNIT-007 | YAML with Lua `run:` referencing `variables` | Execute (skip if `lupa` unavailable)    | Raises Lua nil/undefined-name (not injected)          |

#### AC-12 / AC-13 — Parallel safety (P1)

| ID       | Setup                                                                           | Action                                                    | Expected                                                                              |
|----------|---------------------------------------------------------------------------------|-----------------------------------------------------------|---------------------------------------------------------------------------------------|
| INT-003  | Fan-out → 2 branches → fan-in; `settings.parallel.strategy: thread`            | Each branch: `variables["x"] = "branch-{a,b}"`            | Fan-in reads `variables["x"]`; assert `in {"branch-a", "branch-b"}` (race codified)   |
| INT-004  | Same shape; `settings.parallel.strategy: process`                              | Branch: `variables["x"] = "from-branch"`                  | After fan-in, `engine.variables` lacks `"x"` (or retains pre-existing value)          |

#### Defensive (P1 / P2)

| ID       | Setup                                                | Action                                               | Expected                                            |
|----------|------------------------------------------------------|------------------------------------------------------|-----------------------------------------------------|
| UNIT-008 | YAML with no top-level `variables:` block            | `return {"v": variables}` (or `len(variables)`)      | `variables` is empty dict, no `NameError`           |
| UNIT-009 | YAML with `variables: {a: {b: 1}}`                   | `return {"v": variables["a"]["b"]}`                  | `v == 1` (nested access works; no deep copy)        |

### Test Data & Environment Requirements

**Fixtures**
- **`yaml_engine_factory(yaml_str)`** — produces a fresh `YAMLEngine` from inline YAML; resets between tests.
- **`single_node_yaml(variables, run_body, with_kwargs=None)`** — composes minimal YAML for UNIT-001..009.
- **`two_node_yaml(node_a_run, node_b_body)`** — composes the entry → A → B → end shape for INT-001.
- **`parallel_fan_yaml(branches, strategy)`** — composes fan-out/fan-in with parameterized `settings.parallel.strategy`.

**Environment**
- Default pytest invocation: `cd python && pytest`.
- **INT-003 (thread race):** Assert *membership* in the set of valid outcomes (`assert result["x"] in {"branch-a", "branch-b"}`), not which branch wins. Avoids flakiness while still catching regressions.
- **INT-004 (process discard):** Requires `multiprocessing` `spawn` start method; gate with `pytest.importorskip` or `pytest.skip` if process-mode unavailable.
- **UNIT-007 (Lua):** `pytest.importorskip("lupa")`.
- **DOC-001:** Manual PR-review step against `docs/python/actions-reference.md`; not automated.

**Test data**
- Primitives: `{x: 5}`, `{flag: true}`, `{name: "alice"}` (UNIT-001/002/008).
- Nested: `{a: {b: 1}}` (UNIT-009).
- Distinct branch ids `"branch-a"` / `"branch-b"` for INT-003/004 (makes race/discard observable).

**Suite hygiene**
- Per-test fresh `YAMLEngine` instance; never reuse `engine.variables` across tests.
- Parallel scenarios must not leak module-level state into other tests.

### Recommended Execution Order

1. P0 unit: UNIT-001, UNIT-002, UNIT-003, UNIT-005
2. P0 integration: INT-001
3. P1 unit: UNIT-004, UNIT-008
4. P1 integration: INT-002, INT-003, INT-004
5. P2: UNIT-006, UNIT-009, DOC-001
6. P3: UNIT-007

### Gate YAML Block

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

## QA Notes - Requirements Trace

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Full assessment:** [`docs/qa/assessments/TEA-DX-001.4-trace-20260501.md`](../qa/assessments/TEA-DX-001.4-trace-20260501.md)

### Requirements Coverage

| Metric | Count | % |
|--------|-------|---|
| Total ACs | 13 | 100% |
| Planned FULL coverage | 13 | 100% |
| Planned PARTIAL | 0 | 0% |
| Planned NONE | 0 | 0% |
| **Tests authored against current `main`** | **0** | **0%** |

The test-design document already prescribes 13 scenarios (9 unit + 4 integration + 1 doc review) that, once authored, give every AC — including the three NFR-proposed ones (AC-11/12/13) — full traceable coverage. At trace time none of these are implemented; the existing `engine.variables` tests (`test_yaml_engine_core.py:149` for loading, `test_yaml_engine_jinja2.py:169` for Jinja read-path) cover only upstream invariants and do not exercise the new `run:`-scope name.

### Traceability Matrix

| AC    | What it asserts                                        | Planned Test IDs      | Level(s)            | Coverage |
|-------|--------------------------------------------------------|-----------------------|---------------------|----------|
| AC-1  | `variables` resolves in `run:`                         | UNIT-001              | Unit                | FULL     |
| AC-2  | `[]` and `.get()` access patterns                      | UNIT-001, UNIT-002    | Unit                | FULL     |
| AC-3  | Writes mutate `engine.variables`                       | UNIT-003, INT-001     | Unit + Integration  | FULL     |
| AC-4  | Jinja `{{ variables.x }}` non-regression                | UNIT-004              | Unit                | FULL     |
| AC-5  | `with:` kwarg overrides engine attribute               | UNIT-005, UNIT-006    | Unit                | FULL     |
| AC-6  | `examples/yaml/` workflows still run                   | INT-002               | Integration         | FULL     |
| AC-7  | Lua/Prolog runtimes unaffected                         | UNIT-007              | Unit                | FULL     |
| AC-8  | Read test `{x:5}` → `y == 6`                           | UNIT-001 (alias)      | Unit                | FULL     |
| AC-9  | Write visible to next node's Jinja                     | INT-001 (alias)       | Integration         | FULL     |
| AC-10 | `actions-reference.md` lists `variables`               | DOC-001               | Doc review          | FULL     |
| AC-11 | Parallel-safety doc callout                            | DOC-001               | Doc review          | FULL     |
| AC-12 | Thread race codified (non-determinism)                 | INT-003               | Integration         | FULL     |
| AC-13 | Process discard codified (invisibility)                | INT-004               | Integration         | FULL     |

**Defensive extras** (not tied to a single AC, recommended to retain): `UNIT-008` (no `variables:` block → `{}`, no `NameError`) and `UNIT-009` (nested dict access, no implicit deep copy).

**Risk-to-test mapping** (closes risk-profile findings):
- TECH-006 (parallel race) → INT-003, INT-004
- TECH-001 (kwarg shadowing) → UNIT-005, UNIT-006
- TECH-002 (cross-node mutation visibility) → INT-001
- SEC-001 (mutable engine state in `exec()`) → UNIT-009
- OPS-001 (doc discoverability) → DOC-001, INT-002

### Gaps Identified

1. **Gap 1 — All 13 planned scenarios are unimplemented.** Expected (story is Draft); listed so the dev agent can use the matrix as a checklist when authoring tests.
2. **Gap 2 — AC-13 needs platform-aware skip.** Process-mode discard test should use `multiprocessing.set_start_method("spawn", force=True)` and an explicit skip-with-log path so CI does not mistake silent skips for green.
3. **Gap 3 — AC-11 is a manual doc review.** Add an explicit checkbox in the PR DoD: "`actions-reference.md`: `variables` listed *and* parallel-safety callout present."
4. **Gap 4 — No performance ceiling.** Not needed — single dict insertion per `run_inline` call is O(1); NFR Performance was already PASS. Listed only for completeness.

### Recommendations

- **Authoring order (matches test-design):** P0 unit (UNIT-001/002/003/005) → P0 integration (INT-001) → P1 unit (UNIT-004/008) → P1 integration (INT-002/003/004) → P2 (UNIT-006/009, DOC-001) → P3 (UNIT-007).
- **File placement:** UNIT-001..006/008/009 belong next to `test_unit_006_load_from_dict_extracts_variables` in `python/tests/test_yaml_engine_core.py`; INT-003/INT-004 are best isolated in a new `python/tests/test_yaml_run_variables_parallel.py` so parallel-strategy gating doesn't pollute the core suite.
- **CI placement:** Move INT-002 (examples smoke) to a nightly or pre-merge job; running every example YAML on every PR will slow the suite for marginal incremental safety.
- **Sentinel test:** Keep UNIT-007 (Lua-not-injected) even when it looks redundant during refactors — it is the only assertion that the change did not leak across runtimes (AC-7).
- **Risk posture:** All ACs land at Low Risk by design. The risk-profile score (94/100) is consistent with this trace; no AC requires re-assessment.

### Gate Contribution

```yaml
trace:
  totals: { requirements: 13, full: 13, partial: 0, none: 0 }
  uncovered: []
  implementation_status: { tests_authored: 0, tests_planned: 13 }
  notes: 'See docs/qa/assessments/TEA-DX-001.4-trace-20260501.md'
```

Trace matrix: docs/qa/assessments/TEA-DX-001.4-trace-20260501.md

---

## SM Validation

**Reviewer:** Bob (Scrum Master) · **Date:** 2026-05-01 · **Mode:** YOLO

### Definition of Ready Checklist

| # | Criterion | Status | Notes |
|---|-----------|--------|-------|
| 1 | Story has clear title and description | ✅ PASS | Title is precise; "As a/I want/So that" framing present; problem statement is concrete with before/after example. |
| 2 | Acceptance criteria are defined and testable | ✅ PASS | 10 numbered ACs in story body (AC-1..AC-10) covering Functional, Integration, Quality. Each is observable/binary. NFR proposes AC-11/12/13 (parallel-safety) — test design and trace already cover them. |
| 3 | Dependencies are identified | ✅ PASS | Parent epic [TEA-DX-001](TEA-DX-001-yaml-runner-developer-experience-epic.md) linked. Integration points cited with file:line refs (`yaml_nodes.py:710-768`, `yaml_engine.py:488/533/892/1263`, `yaml_engine.py:1325` for parallel strategy). |
| 4 | Technical approach is documented | ✅ PASS | Single-line change site identified at `yaml_nodes.py:715` with concrete code snippet. Existing-pattern reference (`yaml_engine.py:533`) and key constraint (kwarg ordering) called out. |
| 5 | Story is properly sized | ✅ PASS | Single-line implementation + 3 unit tests + 2 parallel-safety integration tests + doc update. Tasks broken into 3 with explicit AC mapping. Estimated <1 day including the NFR-recommended additions. |
| 6 | QA notes sections present | ✅ PASS | All four sections present and link to assessments under `docs/qa/assessments/` (verified on disk): Risk Profile (94/100, gate PASS), NFR (90/100, gate CONCERNS — waivable), Test Design (13 scenarios, 0 gaps), Requirements Trace (13/13 FULL coverage planned). |
| 7 | No blocking issues or unknowns | ✅ PASS (with note) | Risk profile = PASS. NFR = CONCERNS but explicitly waivable; test-design and trace already author the parallel-safety scenarios that close the concern. No unresolved unknowns block dev start. |

### Outcome
**All 7 DoR criteria pass.** Story is **Ready for Development**.

### Notes for the Dev Agent
- **AC list reconciliation:** QA test-design and trace reference AC-11/12/13 (parallel-safety doc + thread race + process discard). These are listed in the NFR section as "proposed" but already mapped to tests INT-003/INT-004/DOC-001. **Implement against all 13** (the original 10 plus the three NFR-proposed) to lift the NFR gate from CONCERNS to PASS. If the team prefers to defer AC-11/12/13, file a follow-up and waive the NFR concern explicitly.
- **Authoring order** (per trace recommendation): P0 unit (UNIT-001/002/003/005) → P0 integration (INT-001) → P1 unit (UNIT-004/008) → P1 integration (INT-002/003/004) → P2 (UNIT-006/009, DOC-001) → P3 (UNIT-007).
- **File placement:** UNIT-001..006/008/009 next to `test_unit_006_load_from_dict_extracts_variables` in `python/tests/test_yaml_engine_core.py`; INT-003/INT-004 isolated in a new `python/tests/test_yaml_run_variables_parallel.py` to avoid leaking parallel-strategy state into the core suite.
- **Process-mode test (INT-004):** must use `multiprocessing.set_start_method("spawn", force=True)` with explicit skip-with-log so silent skips don't read green in CI.
- **Kwarg ordering** in `exec_globals` is load-bearing — `variables` must be inserted **before** `**kwargs` to preserve AC-5 semantics. Do not reorder.

SM_CHECKLIST_COMPLETED

---

## Dev Agent Record

### Agent Model Used
James (BMad Dev) on `claude-opus-4-7[1m]`.

### Debug Log References
- Targeted suite: `pytest tests/test_dx_001_4_variables_in_run.py tests/test_yaml_run_variables_parallel.py tests/test_yaml_engine_core.py tests/test_yaml_engine_jinja2.py tests/test_yaml_engine_nodes.py tests/test_yaml_engine_actions.py tests/test_yaml_engine_observability.py tests/test_yaml_dynamic_parallel.py tests/test_parallel_integration.py` → **200 passed, 1 skipped**.
- Examples smoke load: 12/13 YAMLs loaded clean. `examples/yaml/stream_broadcast.yaml` raises `TypeError: unhashable type: 'list'`; reproduced on a clean `git stash` of the working tree (i.e. pre-existing on `main`, unrelated to TEA-DX-001.4).
- Lua isolation test (UNIT-007) skipped: `lupa` is not installed in the local env. The test is gated with `pytest.importorskip("lupa")` and will execute in CI environments that have it.

### Completion Notes List
- **Task 1 — Implementation.** Single-line addition in `yaml_nodes.py:710-768` injects `engine.variables` into `exec_globals` for inline Python `run:` blocks. Placement is **before** `**kwargs`, preserving AC-5 kwarg-precedence semantics. `hasattr(engine, "variables")` guard is defensive (always true in current engine, but keeps the closure tolerant of future refactors that might lazily attach the attribute).
- **Task 2 — Tests.** All 13 planned scenarios are covered:
  - **`tests/test_dx_001_4_variables_in_run.py`** — UNIT-001..009 (UNIT-007 skipped without `lupa`). Includes the AC-9 cross-node Jinja-visibility test (also serves as INT-001 alias).
  - **`tests/test_yaml_run_variables_parallel.py`** — INT-003 (thread race, asserts membership of the observed value rather than which branch wins) and INT-004 (process discard). INT-004 forces the `spawn` start method and gracefully skips when the process executor is unavailable in the env.
  - **INT-002 (examples smoke)** is exercised manually via the smoke loader above; not added to pytest because that's the per-trace recommendation (nightly/pre-merge job).
  - **DOC-001 (manual review)** is satisfied by the `docs/python/actions-reference.md` § Run Block Globals section.
- **Task 3 — Docs.** Added a "Run Block Globals" section to `docs/python/actions-reference.md` that:
  - Lists `state`, `variables`, `actions`, `json`, `requests`, `datetime`, `OpenAI` with type/lifetime/description (AC-10).
  - Documents the kwarg-precedence rule and "no auto-coercion" footgun (covers UNIT-005/006 in prose).
  - Calls out the parallel-safety semantics: thread = shared/race, process/remote = discarded (AC-11). Closes the NFR Reliability CONCERNS.
- **NFR gate path.** AC-11/12/13 from the NFR review are now implemented (doc + INT-003 + INT-004). The Reliability CONCERNS condition is closed by the documentation callout plus the two integration regression tests; reviewer can lift the gate to PASS.

### File List
**Modified:**
- `python/src/the_edge_agent/yaml_nodes.py` — added `variables` to `exec_globals` in `run_inline` (`yaml_nodes.py:715`-area). NB: this file already carried unrelated TEA-DX-001.5 dynamic_parallel error-message changes from a sibling story; only the `variables` injection is part of TEA-DX-001.4.
- `python/tests/test_dx_001_4_variables_in_run.py` — extended from 5 to 10 tests (UNIT-001..009 + Lua AC-7 isolation).
- `docs/python/actions-reference.md` — added "Run Block Globals" section + ToC entry, including parallel-safety callout.

**Added:**
- `python/tests/test_yaml_run_variables_parallel.py` — new file holding INT-003 (thread race) and INT-004 (process discard) parallel-safety regression tests.

**Deleted:**
- _none_

### Change Log
| Date       | Author | Summary                                                                                                |
|------------|--------|--------------------------------------------------------------------------------------------------------|
| 2026-05-02 | James  | Implemented TEA-DX-001.4: injected `engine.variables` into Python `run:` exec_globals; authored UNIT-001..009 + INT-003/004 tests; documented Run Block Globals + parallel safety in `actions-reference.md`. Set status to Ready for Review. |

DEV_STORY_COMPLETED

---

## QA Results

### Final Review

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-02 · **Mode:** YOLO comprehensive review
**Gate Decision:** **PASS** · **Quality Score: 100/100**
**Gate file:** [`docs/qa/gates/TEA-DX-001.4-variables-in-run-exec-globals.yml`](../qa/gates/TEA-DX-001.4-variables-in-run-exec-globals.yml)

#### Implementation Verification

Reviewed `python/src/the_edge_agent/yaml_nodes.py:710-736`:

- ✅ Single-line additive change as designed; `engine.variables` injected into `exec_globals` for inline Python `run:` blocks.
- ✅ Placed **before** `**kwargs` (line 722-725 vs 725) — preserves AC-5 kwarg precedence.
- ✅ Defensive `hasattr(engine, "variables")` guard tolerates future engine refactors that might lazily attach the attribute.
- ✅ Lua runtime branch (line 700-708) and Prolog runtime branch (line 683-691) return *before* the `run_inline` factory — `variables` is structurally inaccessible from those runtimes (AC-7 enforced by routing, not just by test).
- ✅ Inline TEA-DX-001.4 code comment documents intent and the kwarg-ordering invariant for future maintainers.

#### Requirements Traceability — 13/13 FULL

| AC    | Test File                                             | Status |
|-------|-------------------------------------------------------|--------|
| AC-1  | `test_dx_001_4_variables_in_run.py::test_unit_001`    | ✅ PASS |
| AC-2  | `test_dx_001_4_variables_in_run.py::test_unit_001/002`| ✅ PASS |
| AC-3  | `test_dx_001_4_variables_in_run.py::test_unit_003`    | ✅ PASS |
| AC-4  | `test_dx_001_4_variables_in_run.py::test_unit_004`    | ✅ PASS |
| AC-5  | `test_dx_001_4_variables_in_run.py::test_unit_005/006`| ✅ PASS |
| AC-6  | Manual examples smoke (12/13 OK; 1 pre-existing fail) | ✅ PASS |
| AC-7  | `test_dx_001_4_variables_in_run.py::test_unit_007`    | ⏭ SKIP (lupa not in env; gated correctly) |
| AC-8  | `test_dx_001_4_variables_in_run.py::test_unit_001`    | ✅ PASS |
| AC-9  | `test_dx_001_4_variables_in_run.py::test_unit_003`    | ✅ PASS |
| AC-10 | `docs/python/actions-reference.md` § Run Block Globals| ✅ PASS |
| AC-11 | `docs/python/actions-reference.md` § Parallel safety  | ✅ PASS |
| AC-12 | `test_yaml_run_variables_parallel.py::thread_race`    | ✅ PASS |
| AC-13 | `test_yaml_run_variables_parallel.py::process_discard`| ✅ PASS |

Defensive extras UNIT-008 (no `variables:` block) and UNIT-009 (nested access) also green.

#### Test Execution

- **TEA-DX-001.4 targeted suite:** 10 passed, 1 skipped (lupa unavailable; correctly gated by `pytest.importorskip`) in 0.51s.
- **Regression suite** (`test_yaml_engine_core`, `test_yaml_engine_jinja2`, `test_yaml_engine_nodes`, `test_yaml_dynamic_parallel`, `test_parallel_integration`): **118 passed, 0 failed** in 0.89s.
- **Examples smoke:** Dev Agent Record reports 12/13 OK; the single failure (`stream_broadcast.yaml`) was reproduced on a clean `git stash` of `main` and is unrelated to this change.

#### Code Quality Review

- Code: minimal, idiomatic, mirrors the established Jinja-context pattern at `yaml_engine.py:533`.
- Tests: well-organized, named after AC + UNIT/INT IDs from test design, with docstrings explaining the assertion. Race test (INT-003) correctly asserts membership in the valid-outcomes set rather than which branch wins — stable but still proves the binding is shared. Process-discard test (INT-004) gracefully skips when the process executor or `spawn` start method is unavailable.
- Docs: new "Run Block Globals" section is self-contained, includes a kwarg-precedence note, an explicit footgun callout (no auto-coercion), and a parallel-safety table covering thread/process/remote semantics. ToC entry added.

#### NFR Validation

| NFR             | Status   | Notes                                                                                              |
|-----------------|----------|----------------------------------------------------------------------------------------------------|
| Security        | **PASS** | No new attack surface — `run:` already executes arbitrary Python; trust boundary is the YAML source. |
| Performance     | **PASS** | O(1) dict insertion per node invocation; off the hot path; no I/O or copy.                          |
| Reliability     | **PASS** (was CONCERNS) | Closed by AC-11 (doc callout) + AC-12 (thread race test) + AC-13 (process discard test). Both failure modes are now codified and documented. |
| Maintainability | **PASS** | Single-line change with intent-explaining comment; ACs require all relevant test/doc updates; mirrors an existing pattern. |

The original NFR Reliability CONCERNS gate condition — "needs doc warning + regression tests for parallel-strategy semantics" — has been **fully addressed**. Quality score lifts from 90 → 100.

#### Standards Compliance

- ✅ File placement matches SM Validation guidance: TEA-DX-001.4-specific tests in `test_dx_001_4_variables_in_run.py`; parallel-strategy tests isolated in `test_yaml_run_variables_parallel.py`.
- ✅ Kwarg ordering invariant in `exec_globals` preserved.
- ✅ Lua isolation test gated with `pytest.importorskip`.
- ✅ Process-mode test forces `spawn` start method and skips with clear log on unavailability — no silent skips reading green.
- ✅ Story permissions respected: only QA Results section modified.

#### Gate Status

**PASS** — All 13 ACs met, all targeted and regression tests green, NFR Reliability CONCERNS closed. The change is a single additive line at `yaml_nodes.py:715` mirroring an existing engine-level pattern; risk profile remains 94/100 Very Low.

#### Recommendations

- **Immediate:** None.
- **Future (non-blocking):**
  - Consider promoting the examples-smoke check (INT-002) into a nightly CI job so AC-6 is exercised continuously rather than at PR time.
  - When the `lupa` dev dep is added to the default test env, UNIT-007 will start asserting AC-7 actively rather than skipping; no code change required.
  - The `stream_broadcast.yaml` pre-existing load failure is unrelated to this story but should be tracked in its own ticket.

QA_REVIEW_COMPLETED
