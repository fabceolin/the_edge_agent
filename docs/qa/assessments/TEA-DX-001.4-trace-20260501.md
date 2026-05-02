# Requirements Traceability Matrix

## Story: TEA-DX-001.4 — `variables` accessible in Python `run:` blocks

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Story status at trace:** Draft (implementation not yet started; tests not yet authored)
**Planning reference:** [`TEA-DX-001.4-test-design-20260501.md`](TEA-DX-001.4-test-design-20260501.md)

---

### Coverage Summary

| Metric | Count | % |
|--------|-------|---|
| Total Requirements (ACs) | 13 | 100% |
| Planned Coverage — FULL | 13 | 100% |
| Planned Coverage — PARTIAL | 0 | 0% |
| Planned Coverage — NONE | 0 | 0% |
| **Implemented today (against current `main`)** | **0** | **0%** |

**Interpretation.** The test-design document specifies 13 scenarios (9 unit + 4 integration + 1 doc review) that, *once authored*, will provide full coverage of every AC including the three proposed in the NFR review (AC-11/12/13). At trace time none of those scenarios have been implemented in `python/tests/` — this is the dominant gap. Existing `engine.variables` tests (e.g., `test_yaml_001_int_004_variables_accessible` at `python/tests/test_yaml_engine_jinja2.py:169`, `test_unit_006_load_from_dict_extracts_variables` at `python/tests/test_yaml_engine_core.py:149`) cover *upstream* invariants (variables get loaded from YAML and Jinja can read them) but none probe the new `run:`-scope name proposed by this story.

---

### Requirement Mappings

#### AC-1: `variables` resolves to `engine.variables` inside a `run:` block

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-001` — `test_run_block_reads_variables_dict`
  - **Given:** YAML with top-level `variables: {x: 5}` and a single Python `run:` node.
  - **When:** The node body executes `return {"y": variables["x"] + 1}`.
  - **Then:** The post-node state contains `y == 6`, proving the name `variables` resolved (not `NameError`) and points at the engine dict.

#### AC-2: Both `variables["k"]` and `variables.get("k", default)` work

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-001` (subscript form, see AC-1).
- **Unit Test (planned):** `UNIT-002` — `test_run_block_variables_get_with_default`
  - **Given:** YAML with `variables: {x: 5}`.
  - **When:** Body executes `return {"y": variables.get("x", 0), "z": variables.get("missing", 99)}`.
  - **Then:** State contains `y == 5` and `z == 99`, confirming dict-protocol behavior including default fallback for absent keys.

#### AC-3: Writes inside a `run:` block mutate `engine.variables`

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-003` — `test_run_block_writes_to_variables`
  - **Given:** YAML with `variables: {x: 5}`, single `run:` node.
  - **When:** Body executes `variables["new"] = "v"; return {}`.
  - **Then:** After invoke, `engine.variables["new"] == "v"` (mutation observed on the engine attribute).
- **Integration Test (planned):** `INT-001` — `test_run_block_write_visible_to_next_node_jinja` (see AC-9 for the cross-node body).

#### AC-4: Existing Jinja `{{ variables.x }}` continues to work

**Planned coverage: FULL · Actual coverage today: PARTIAL (covered upstream by `test_yaml_001_int_004_variables_accessible`, but not in the post-change context)**

- **Unit Test (planned):** `UNIT-004` — `test_jinja_variables_template_still_resolves_after_change`
  - **Given:** YAML with `variables: {x: "hello"}` and a node body containing `result = "{{ variables.x }}"`.
  - **When:** The node executes after the new `exec_globals["variables"] = engine.variables` line is in place.
  - **Then:** State `r == "hello"` — Jinja path is untouched by the new exec-scope name.
- **Existing baseline (already green on `main`):** `python/tests/test_yaml_engine_jinja2.py:169` (`test_yaml_001_int_004_variables_accessible`) confirms the Jinja-side invariant; needs to remain green post-merge.

#### AC-5: `with:` kwarg named `variables` overrides the engine attribute

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-005` — `test_with_kwarg_variables_overrides_engine`
  - **Given:** `engine.variables = {x: 5}`; node has `with: {variables: {x: 99}}`.
  - **When:** Body executes `return {"v": variables["x"]}`.
  - **Then:** State `v == 99` — confirms the `**kwargs` spread *follows* the engine injection at `yaml_nodes.py:715` (kwarg precedence preserved).
- **Unit Test (planned):** `UNIT-006` — `test_non_dict_variables_kwarg_passes_through_uncoerced`
  - **Given:** Node has `with: {variables: "not-a-dict"}`.
  - **When:** Body executes `return {"t": type(variables).__name__}`.
  - **Then:** State `t == "str"` — codifies the footgun (no implicit coercion).

#### AC-6: Existing `examples/yaml/` workflows continue to run unchanged

**Planned coverage: FULL · Actual coverage today: NONE (no smoke harness for this story yet)**

- **Integration Test (planned):** `INT-002` — `test_examples_yaml_smoke`
  - **Given:** Every YAML file under `examples/yaml/`.
  - **When:** Each is loaded via `YAMLEngine.load_from_file(...).compile()` (and invoked when deterministic — i.e., no LLM/network/HITL).
  - **Then:** All compile without error; deterministic runs reach `__end__` cleanly with no new `NameError` or shadowing.

#### AC-7: Lua/Prolog `run:` runtimes are not affected (Python-only scope)

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-007` — `test_lua_run_block_does_not_inject_variables`
  - **Given:** YAML with a Lua `run:` body referencing `variables`.
  - **When:** Engine executes the node (skipped if `lupa` is unavailable via `pytest.importorskip`).
  - **Then:** Lua raises a nil/undefined-name error — the Python branch in `_create_node_run_function` (around `yaml_nodes.py:700`) is the only one mutated; `run_lua` at `yaml_nodes.py:702` remains untouched.

#### AC-8: Read test — `{x: 5}` + `return {"y": variables["x"] + 1}` → `{"y": 6}`

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** Alias of `UNIT-001` (literal restatement of the AC). See AC-1.

#### AC-9: Write test — mutation observable in subsequent node's Jinja

**Planned coverage: FULL · Actual coverage today: NONE**

- **Integration Test (planned):** `INT-001` — `test_run_block_write_visible_to_next_node_jinja`
  - **Given:** Two-node YAML; node A is Python (`variables["new"] = "v"`); node B's body contains `result = "{{ variables.new }}"`.
  - **When:** Engine invokes A → B → `__end__`.
  - **Then:** State after B contains `result == "v"`. Closes the loop between AC-3 (engine-level mutation) and AC-4 (Jinja read path).

#### AC-10: `docs/python/actions-reference.md` lists `variables` alongside `state`, `actions`

**Planned coverage: FULL · Actual coverage today: NONE**

- **Doc Review (planned):** `DOC-001` — manual PR-review checklist item
  - **Given:** PR diff touches `actions-reference.md`.
  - **When:** Reviewer scans the run-block scope section.
  - **Then:** `variables` is listed as an exec-scope name with a one-line description and a code example (parity with the `state` / `actions` entries).

#### AC-11 (NFR-proposed): Docs include "Parallel safety" callout

**Planned coverage: FULL · Actual coverage today: NONE**

- **Doc Review (planned):** `DOC-001` extended scope
  - **Given:** PR adds a "Parallel safety" callout to the new `variables` entry.
  - **When:** Reviewer reads the callout.
  - **Then:** The callout (a) flags shared-reference semantics under `parallel.strategy: thread`, (b) flags cross-process invisibility under `process`/`remote`, and (c) recommends `state` for per-flow data. Closes the Reliability CONCERNS in the NFR assessment.

#### AC-12 (NFR-proposed): Parallel-thread race regression test

**Planned coverage: FULL · Actual coverage today: NONE**

- **Integration Test (planned):** `INT-003` — `test_parallel_thread_race_on_variables_write`
  - **Given:** Fan-out → 2 branches → fan-in YAML with `settings.parallel.strategy: thread`.
  - **When:** Each branch writes `variables["x"] = "branch-a"` / `variables["x"] = "branch-b"` and the fan-in node reads the value.
  - **Then:** `engine.variables["x"] in {"branch-a", "branch-b"}` — assert *membership* (codifies non-determinism without flaking on which branch wins). Maps to risk TECH-006.

#### AC-13 (NFR-proposed): Parallel-process discard regression test

**Planned coverage: FULL · Actual coverage today: NONE**

- **Integration Test (planned):** `INT-004` — `test_parallel_process_write_discarded_at_fanin`
  - **Given:** Same fan-out/fan-in shape; `settings.parallel.strategy: process`; pre-set `engine.variables = {x: "pre-existing"}`.
  - **When:** A worker branch writes `variables["x"] = "from-branch"`; control returns to the parent process at fan-in.
  - **Then:** `engine.variables["x"] == "pre-existing"` (or no `"x"` key, depending on initial state) — codifies the silent-discard failure mode flagged in the NFR assessment. Skip-gated on `multiprocessing.get_start_method() == "spawn"` availability.

---

### Defensive / Out-of-Story-Scope Mappings

These two `UNIT-008` and `UNIT-009` scenarios from the test-design document are not tied to a specific AC but harden the implementation:

| Test ID | Scenario | What it protects |
|---------|----------|-------------------|
| UNIT-008 | YAML with no top-level `variables:` block; `run:` reads `variables` | `engine.variables` defaults to `{}`, never `None` — no `NameError` regression |
| UNIT-009 | YAML with nested `variables: {a: {b: 1}}`; `run:` reads `variables["a"]["b"]` | No accidental deep copy / type loss; nested dicts traverse natively |

Recommend keeping these in the suite — they are cheap and protect the long tail.

---

### Critical Gaps

#### Gap 1 — All planned scenarios are unimplemented
- **Severity:** EXPECTED (story is Draft; this is the next dev's job).
- **Risk:** None at trace time; tracked here so the dev agent and reviewer can use the table above as a checklist when authoring tests.
- **Action:** When implementing, follow the recommended execution order from the test-design (P0 unit → P0 integration → P1 unit → P1 integration → P2 → P3).

#### Gap 2 — AC-13 (process-mode discard) requires platform-aware skip
- **Severity:** Low.
- **Risk:** Test could be silently skipped on macOS where `fork` is the default and process-mode behavior differs.
- **Action:** Use `pytest.importorskip("multiprocessing")` and explicit `multiprocessing.set_start_method("spawn", force=True)` inside the test fixture; assert the skip path is logged so CI does not mistake silent skips for green.

#### Gap 3 — AC-11 doc review is manual
- **Severity:** Low.
- **Risk:** Easy to miss in a follow-up PR if the reviewer doesn't open the file diff.
- **Action:** Add a one-line entry in the PR template / DoD checklist: "`actions-reference.md`: `variables` listed *and* parallel-safety callout present." Already partially captured in story DoD.

#### Gap 4 — No explicit performance ceiling
- **Severity:** Trivial.
- **Risk:** Single dict-key insertion per `run_inline` call has O(1) cost; no ceiling needed. Mentioned only for completeness given the NFR Performance assessment was PASS.
- **Action:** None.

---

### Test Design Recommendations

The test-design document (already at FULL coverage) is the source of truth for scenarios. Based on this trace, the only additional recommendations are operational:

1. **Author tests in the test-design's recommended execution order** — get P0 unit tests (UNIT-001/002/003/005) green first; they protect the four functional ACs that matter most (read, get, write, kwarg precedence).
2. **Add the new tests to the same file as related coverage** — recommend `python/tests/test_yaml_engine_core.py` for UNIT-001..006/008/009 (alongside `test_unit_006_load_from_dict_extracts_variables`); a new `python/tests/test_yaml_run_variables_parallel.py` for INT-003/INT-004 to keep parallel-strategy gating isolated.
3. **Wire INT-002 (examples smoke) into a CI job, not a per-PR test** — running every example YAML on every test invocation will slow the suite; a nightly or pre-merge job suffices for AC-6.
4. **Treat AC-7 as a sentinel** — UNIT-007 is the only test in the suite asserting that the change *did not* leak into Lua/Prolog. Keep it even when it would be tempting to delete during refactors.

---

### Risk Assessment (per-AC)

| Risk Tier | Requirements |
|-----------|--------------|
| **High Risk (no planned coverage)** | None |
| **Medium Risk (partial planned coverage)** | None |
| **Low Risk (full unit + integration planned)** | AC-1, AC-2, AC-3, AC-5, AC-9 |
| **Low Risk (full unit-only planned)** | AC-4, AC-7, AC-8 |
| **Low Risk (integration-only planned)** | AC-6, AC-12, AC-13 |
| **Low Risk (doc-review only)** | AC-10, AC-11 |

All ACs are at Low Risk *by design*: this is a single-line additive change. The risk-profile assessment (94/100, very low) is consistent with this trace.

---

### Gate YAML Block

```yaml
trace:
  totals:
    requirements: 13
    full: 13
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.4-test-design-20260501.md'
  uncovered: []
  implementation_status:
    tests_authored: 0
    tests_planned: 13
    note: 'Story is Draft; full planned coverage from test-design, zero authored against current main.'
  ac_to_test_map:
    AC-1: [UNIT-001]
    AC-2: [UNIT-001, UNIT-002]
    AC-3: [UNIT-003, INT-001]
    AC-4: [UNIT-004]
    AC-5: [UNIT-005, UNIT-006]
    AC-6: [INT-002]
    AC-7: [UNIT-007]
    AC-8: [UNIT-001]
    AC-9: [INT-001]
    AC-10: [DOC-001]
    AC-11: [DOC-001]
    AC-12: [INT-003]
    AC-13: [INT-004]
  defensive_extras: [UNIT-008, UNIT-009]
  risk_mitigations:
    TECH-006: [INT-003, INT-004]
    TECH-001: [UNIT-005, UNIT-006]
    TECH-002: [INT-001]
    SEC-001:  [UNIT-009]
    OPS-001:  [DOC-001, INT-002]
  notes: 'See docs/qa/assessments/TEA-DX-001.4-trace-20260501.md'
```

---

### Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.4-trace-20260501.md
```
