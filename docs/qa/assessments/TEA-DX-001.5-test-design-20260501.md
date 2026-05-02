# Test Design: Story TEA-DX-001.5

**Story:** Better `dynamic_parallel` error messages
**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO

---

## Test Strategy Overview

- **Total test scenarios:** 21
- **Unit tests:** 17 (81%)
- **Integration tests:** 3 (14%)
- **E2E tests:** 0 (0%) — story is a parse-time error-formatting refactor; full
  workflow E2E adds no signal beyond integration
- **Doc tests:** 1 (5%)
- **Priority distribution:** P0: 6, P1: 9, P2: 5, P3: 1

The change under test is concentrated at four `raise ValueError(...)` sites in
`python/src/the_edge_agent/yaml_nodes.py:1141–1172` plus one new check (fan_in
target existence). Test design is biased toward unit-level message-content
assertions because the change is essentially structured-string formatting,
with integration coverage where the new fan_in-existence check intersects the
node registry (the only place unit tests cannot give confidence) and where
existing valid workflows must continue to execute (AC-6 regression).

The design directly addresses Reliability (TECH-002 fan_in registry timing)
and Maintainability (TECH-004 template-YAML-validity, OPS-001 doc anchor,
DATA-001 grep-friendly first line) concerns surfaced in prior risk and NFR
assessments by codifying them as executable tests.

---

## Test Scenarios by Acceptance Criteria

### AC-1: Each `ValueError` includes node name, key path, example, doc anchor

| ID                    | Level | Priority | Test                                                                                                                                                                  | Justification                                                  | Mitigates Risks      |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|----------------------|
| TEA-DX-001.5-UNIT-001 | Unit  | P0       | Parametrized over all 5 error codes: each `ValueError` message contains the offending node name as a substring.                                                       | Pure formatting logic; ensures the established invariant.      | DATA-001             |
| TEA-DX-001.5-UNIT-002 | Unit  | P0       | Parametrized over all 5 error codes: each message contains the doc anchor substring `docs/shared/YAML_REFERENCE.md#dynamic-parallel`.                                 | Closes AC-1's docs-anchor requirement; one assertion per code. | OPS-001              |
| TEA-DX-001.5-UNIT-003 | Unit  | P0       | Parametrized over all 5 error codes: each message contains a YAML-fragment example (regex: a line starting with two-space indent and a YAML key).                     | Ensures every code carries actionable example syntax.          | BUS-001              |
| TEA-DX-001.5-UNIT-004 | Unit  | P1       | Parametrized over all 5 error codes: each example fragment, when extracted from the message, parses cleanly via `yaml.safe_load`.                                     | Codifies AC-11 (proposed); guards template drift.              | TECH-004             |
| TEA-DX-001.5-UNIT-005 | Unit  | P1       | Parametrized over all 5 error codes: every line of the message is ≤ 80 chars and is plain ASCII (no high-codepoint chars).                                            | AC-8 (line wrap + ASCII).                                      | DATA-001             |

### AC-2: Conflicting `action` / `steps` / `subgraph` keys named explicitly

| ID                    | Level | Priority | Test                                                                                                                                                                                                                                       | Justification                                            | Mitigates Risks |
|-----------------------|-------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-006 | Unit  | P0       | YAML defines both `action:` and `steps:` on a `dynamic_parallel` node; `ValueError` message names exactly the two conflicting keys (`action`, `steps`) and suggests removing one.                                                          | Most common form of the AC-2 confusion.                  | BUS-001         |
| TEA-DX-001.5-UNIT-007 | Unit  | P1       | YAML defines `action:` and `subgraph:`; message names `action` and `subgraph`.                                                                                                                                                             | Other 2-of-3 combinations behave consistently.           | —               |
| TEA-DX-001.5-UNIT-008 | Unit  | P1       | YAML defines `steps:` and `subgraph:`; message names `steps` and `subgraph`.                                                                                                                                                              | Symmetry across all 2-of-3 combinations.                 | —               |
| TEA-DX-001.5-UNIT-009 | Unit  | P1       | YAML defines all three (`action:`, `steps:`, `subgraph:`); message lists all three and suggests removing two.                                                                                                                              | 3-of-3 case must not silently degrade to 2-of-3 message. | —               |
| TEA-DX-001.5-UNIT-010 | Unit  | P0       | YAML defines zero of (`action`, `steps`, `subgraph`); message names all three options as alternatives (the `mode_count == 0` branch).                                                                                                      | Distinct from 2-of-3; AC-1 + AC-9.                       | —               |

### AC-3: Missing `fan_in:` includes guidance on where it goes

| ID                    | Level | Priority | Test                                                                                                                                                                                | Justification                                       | Mitigates Risks |
|-----------------------|-------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-011 | Unit  | P0       | YAML omits `fan_in:` entirely; message contains the substring `sibling of items` (case-insensitive) AND a short example fragment with `fan_in: <node_name>` at the same indent as `items:`. | Codifies AC-3 placement guidance.                   | BUS-001         |

### AC-4: Invalid `fan_in:` target → clearer error with candidate list

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                                                                | Justification                                                          | Mitigates Risks |
|-----------------------|-------------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------|-----------------|
| TEA-DX-001.5-INT-001  | Integration | P0       | Two-node YAML graph: `dynamic_parallel` with `fan_in: missing_node` and a separate `aggregate:` node; loading raises `ValueError` containing `fan_in target 'missing_node' is not a defined node` AND the list of declared nodes (e.g., `[aggregate, the_dynamic_parallel_node]`).                  | Crosses the registry boundary; pure unit can't see other nodes.        | TECH-002        |
| TEA-DX-001.5-INT-002  | Integration | P1       | YAML where `fan_in:` target is **declared after** the `dynamic_parallel` node in the YAML file; loads successfully (no error). Asserts validation timing accommodates forward references.                                                                                                          | Confirms TECH-002 mitigation works for late-bound declarations.        | TECH-002        |
| TEA-DX-001.5-INT-003  | Integration | P1       | YAML where `fan_in:` target is **declared before** the `dynamic_parallel` node; loads successfully (no error). Sanity check that early-bound declarations also work.                                                                                                                               | Symmetry with INT-002; protects against accidental order-coupling.     | TECH-002        |
| TEA-DX-001.5-UNIT-012 | Unit        | P2       | YAML where `fan_in:` target is misspelled as a near-neighbor (`agregate` vs `aggregate`); error message includes a "did you mean" candidate list or at minimum the full declared-node list.                                                                                                        | Validates message helpfulness (BUS-001) without hard-requiring fuzzy match. | BUS-001         |
| TEA-DX-001.5-UNIT-013 | Unit        | P2       | YAML where `fan_in:` is a reserved name (`__end__` or `__start__`); behavior is *defined* and tested — either accept (treat as terminal) or reject with a specific message; either way the assertion locks the chosen contract.                                                                    | Closes a corner case that risk profile flagged.                        | TECH-002        |

### AC-5: Missing `items:` includes the example `items: "{{ state.batches }}"`

| ID                    | Level | Priority | Test                                                                                                                                                                                       | Justification                                           | Mitigates Risks |
|-----------------------|-------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-014 | Unit  | P0       | YAML omits `items:`; `ValueError` message contains the literal substring `items: "{{ state.batches }}"`.                                                                                  | Direct AC-5 assertion; one of the highest-frequency errors. | BUS-001     |

### AC-6: Existing valid `dynamic_parallel` workflows continue to work

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                  | Justification                                                  | Mitigates Risks |
|-----------------------|-------------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------|
| TEA-DX-001.5-INT-004  | Integration | P0       | Smoke-load every YAML under `examples/yaml/` that uses `dynamic_parallel`; assert each compiles and (where deterministic) executes to `__end__` without raising.                                                                      | Catches accidental regression in valid configurations.        | TECH-001        |

### AC-7: Error type remains `ValueError`

| ID                    | Level | Priority | Test                                                                                                                                                                  | Justification                                            | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-015 | Unit  | P1       | Parametrized over all 5 error codes: assert `type(exc) is ValueError` (strict, not `isinstance`) — catches accidental introduction of a `ValueError` subclass.        | Preserves the public contract per AC-7.                  | —               |

### AC-8: 80-char wrap, plain ASCII

Covered by **TEA-DX-001.5-UNIT-005**.

### AC-9: Unit test for each error case

Coverage table below; satisfied by the parametrized UNIT-001..005 plus the
per-code scenarios (UNIT-006/010/011/014, INT-001).

### AC-10: No new dependencies

| ID                    | Level | Priority | Test                                                                                                                                                                                            | Justification                                                       | Mitigates Risks |
|-----------------------|-------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-016 | Unit  | P3       | Diff `python/setup.py` and `python/pyproject.toml` against `main`: assert no new entries in `install_requires` / `[project] dependencies`. (Implemented as a CI-time diff or manual DoD check.) | Cheap guard; rarely the failure mode but easy to verify.            | —               |

### AC-11 (proposed): Example fragments are valid YAML

Covered by **TEA-DX-001.5-UNIT-004**.

### AC-12 (proposed): `#dynamic-parallel` anchor exists in `YAML_REFERENCE.md`

| ID                    | Level | Priority | Test                                                                                                                                                                                                                                                                | Justification                                                  | Mitigates Risks |
|-----------------------|-------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------|
| TEA-DX-001.5-DOC-001  | Doc   | P1       | Read `docs/shared/YAML_REFERENCE.md` and assert there exists a heading whose GitHub-style slug equals `dynamic-parallel` (regex: `^#+\s+Dynamic\s+Parallel\s*$`, case-insensitive). Auto-runnable as a pytest, not just a manual review.                            | Locks the doc-anchor contract that error messages depend on.   | OPS-001         |

### AC-13 (proposed): Grep-friendly first line

| ID                    | Level | Priority | Test                                                                                                                                                                                                                                                                  | Justification                                              | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-017 | Unit  | P1       | Parametrized over all 5 error codes: the **first line** of the message contains both the node name and a short error category (e.g., `missing 'items'`, `invalid fan_in target`). Assertion is two substring checks against `message.splitlines()[0]`.                | Codifies DATA-001 mitigation as a test, not just a guideline. | DATA-001    |

### `max_concurrency` validation (existing 4th raise site)

| ID                    | Level | Priority | Test                                                                                                                                                                                                | Justification                                              | Mitigates Risks |
|-----------------------|-------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-018 | Unit  | P2       | `max_concurrency: 0` and `max_concurrency: -1` raise `ValueError`; message includes the valid range (e.g., `must be a positive integer`) and an example fragment (`max_concurrency: 4`).         | Refactor target #4 — must keep behaviour and gain richer message. | TECH-001 |
| TEA-DX-001.5-UNIT-019 | Unit  | P2       | `max_concurrency: "{{ state.workers }}"` (Jinja string) does **not** raise at parse time. Locks the existing skip-validation-for-Jinja behaviour at `yaml_nodes.py:1167`.                          | Regression: refactor must not eagerly evaluate templates.  | TECH-001        |

### Defensive / cross-cutting

| ID                    | Level | Priority | Test                                                                                                                                                                                                          | Justification                                                  | Mitigates Risks |
|-----------------------|-------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|-----------------|
| TEA-DX-001.5-UNIT-020 | Unit  | P2       | Helper `_format_dynamic_parallel_error` is importable directly (module-level function, stable signature `(node_name: str, code: str, **context) -> str`).                                                     | Locks API surface so TEA-DX-001.6 can reuse without churn.     | TECH-003        |
| TEA-DX-001.5-UNIT-021 | Unit  | P1       | A `dynamic_parallel` node config containing a sibling key whose value resembles a credential (e.g., `api_key: "secret-token"`) does not appear in any error message produced by the helper.                  | Defensive: confirms helper interpolates only explicit context. | (NFR advisory)  |

---

## Coverage Matrix

| AC    | Test IDs                                                                            | Levels Covered      | Priorities       |
|-------|-------------------------------------------------------------------------------------|---------------------|------------------|
| AC-1  | UNIT-001, UNIT-002, UNIT-003                                                        | Unit                | P0               |
| AC-2  | UNIT-006, UNIT-007, UNIT-008, UNIT-009                                              | Unit                | P0, P1           |
| AC-3  | UNIT-011                                                                            | Unit                | P0               |
| AC-4  | INT-001, INT-002, INT-003, UNIT-012, UNIT-013                                       | Unit + Integration  | P0, P1, P2       |
| AC-5  | UNIT-014                                                                            | Unit                | P0               |
| AC-6  | INT-004                                                                             | Integration         | P0               |
| AC-7  | UNIT-015                                                                            | Unit                | P1               |
| AC-8  | UNIT-005                                                                            | Unit                | P1               |
| AC-9  | UNIT-006, UNIT-010, UNIT-011, UNIT-014, INT-001 (one per code)                      | Unit + Integration  | P0               |
| AC-10 | UNIT-016                                                                            | Unit / DoD          | P3               |
| AC-11 | UNIT-004                                                                            | Unit                | P1               |
| AC-12 | DOC-001                                                                             | Doc                 | P1               |
| AC-13 | UNIT-017                                                                            | Unit                | P1               |
| (max_concurrency) | UNIT-018, UNIT-019                                                      | Unit                | P2               |

**Coverage gaps:** None. Every AC (including proposed AC-11/12/13) maps to at
least one scenario. The two `max_concurrency` regression scenarios cover the
fourth refactor site that the original ACs treat implicitly via AC-1/AC-9.

---

## Risk Coverage

| Risk ID  | Description                                                            | Mitigating Tests                                          |
|----------|------------------------------------------------------------------------|-----------------------------------------------------------|
| TECH-001 | Existing tests assert old exact error strings — break on rewrite       | INT-004 (regression), UNIT-018, UNIT-019                  |
| TECH-002 | `fan_in` existence check needs declared-node registry access           | INT-001, INT-002, INT-003, UNIT-013                       |
| TECH-003 | Helper-function placement diverges from TEA-DX-001.6                   | UNIT-020                                                  |
| TECH-004 | Message templates drift from real syntax as YAML schema evolves        | UNIT-004                                                  |
| OPS-001  | Doc anchor URL points at section that doesn't exist yet                | UNIT-002, DOC-001                                         |
| BUS-001  | New messages still fail to help if examples are too generic            | UNIT-003, UNIT-006, UNIT-011, UNIT-012, UNIT-014          |
| DATA-001 | Multi-line / wrapped messages break log-parsing pipelines              | UNIT-001, UNIT-005, UNIT-017                              |

---

## Test Data & Environment Requirements

### Fixtures
- **Minimal `dynamic_parallel` YAML factory:** helper that produces a YAML
  string with a single `dynamic_parallel` node + a fan-in `aggregate` node,
  with each of `items`, `action`, `steps`, `subgraph`, `fan_in`,
  `max_concurrency` selectively present/absent or overridden. Most UNIT-006
  through UNIT-014 reuse this.
- **Two-node graph factory (registry-aware):** for INT-001..003; entry → DP →
  aggregate → end, with the `fan_in:` target name injected as a parameter and
  the declaration order of DP vs. aggregate flipped between INT-002 and
  INT-003.
- **Examples sweep helper:** glob `examples/yaml/**/*.yaml` filtered by
  `grep -l 'dynamic_parallel'`; load each via `YAMLEngine`. Used by INT-004.
- **Error-template introspection helper:** read the constants table from the
  refactored module and yield `(code, template, example, doc_anchor)` tuples
  for parametrized tests UNIT-001..005, UNIT-015, UNIT-017.

### Environment
- **Standard pytest path:** `cd python && pytest python/tests/test_yaml_dynamic_parallel.py`.
- **No new dependencies** required (AC-10) — uses `pytest`, `pyyaml` (already
  in deps), and the in-tree YAML engine.
- **DOC-001:** read-only filesystem access to `docs/shared/YAML_REFERENCE.md`;
  no extra tooling.
- **UNIT-016:** if implemented as a CI diff, requires `git` access to compare
  against `origin/main`. Otherwise, it is a DoD checkbox.

### Test data
- **Valid baseline config:** known-good `dynamic_parallel` block used by
  INT-002/003/004 and as the negative control for UNIT scenarios.
- **Misspelling neighbours:** `aggregate` vs `agregate` for UNIT-012.
- **Reserved names:** `__end__`, `__start__` for UNIT-013.
- **Boundary integers:** `0`, `-1`, `1`, `100` for `max_concurrency` in
  UNIT-018; `"{{ state.workers }}"` Jinja string for UNIT-019.
- **Credential-shaped sibling key:** `api_key: "secret-token"` injected into
  the node config for UNIT-021.

### Suite hygiene
- All UNIT scenarios construct an isolated `YAMLEngine` per test (no shared
  global state).
- Existing assertions on old error strings (currently at
  `python/tests/test_yaml_dynamic_parallel.py:50, 73, 98, 119, 143`) **must be
  updated in the same PR** (TECH-001 mitigation). The grep already exists —
  use it as the authoritative list of fixtures to migrate.
- Migration pattern: prefer substring or structural assertions (node name
  present + doc anchor present + at least one fragment line) over full-string
  matches, so future template edits don't cascade.

---

## Recommended Execution Order

1. **P0 unit:** UNIT-001, UNIT-002, UNIT-003, UNIT-006, UNIT-010, UNIT-011,
   UNIT-014 (smallest blast radius, fastest feedback on the 5-code matrix).
2. **P0 integration:** INT-001 (fan_in invalid target), INT-004 (regression
   sweep) — these are the two places where unit signal is insufficient.
3. **P1 unit:** UNIT-004, UNIT-005, UNIT-007, UNIT-008, UNIT-009, UNIT-015,
   UNIT-017, UNIT-021.
4. **P1 integration / docs:** INT-002, INT-003, DOC-001.
5. **P2:** UNIT-012, UNIT-013, UNIT-018, UNIT-019, UNIT-020.
6. **P3:** UNIT-016 (deps diff — DoD-style guard).

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 21
  by_level:
    unit: 17
    integration: 3
    e2e: 0
    doc: 1
  by_priority:
    p0: 6
    p1: 9
    p2: 5
    p3: 1
  coverage_gaps: []
  risk_mitigations:
    TECH-001: [TEA-DX-001.5-INT-004, TEA-DX-001.5-UNIT-018, TEA-DX-001.5-UNIT-019]
    TECH-002: [TEA-DX-001.5-INT-001, TEA-DX-001.5-INT-002, TEA-DX-001.5-INT-003, TEA-DX-001.5-UNIT-013]
    TECH-003: [TEA-DX-001.5-UNIT-020]
    TECH-004: [TEA-DX-001.5-UNIT-004]
    OPS-001:  [TEA-DX-001.5-UNIT-002, TEA-DX-001.5-DOC-001]
    BUS-001:  [TEA-DX-001.5-UNIT-003, TEA-DX-001.5-UNIT-006, TEA-DX-001.5-UNIT-011, TEA-DX-001.5-UNIT-012, TEA-DX-001.5-UNIT-014]
    DATA-001: [TEA-DX-001.5-UNIT-001, TEA-DX-001.5-UNIT-005, TEA-DX-001.5-UNIT-017]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.5-test-design-20260501.md
P0 tests identified: 6
```

---

## Quality Checklist

- [x] Every AC has test coverage (incl. proposed AC-11/12/13)
- [x] Test levels appropriate (no E2E for a parse-time formatting refactor)
- [x] No duplicate coverage across levels (AC-9 satisfied via reuse, not new tests)
- [x] Priorities align with risk profile (TECH-002 → P0/P1 integration tests)
- [x] Test IDs follow `TEA-DX-001.5-{LEVEL}-{SEQ}` convention
- [x] Scenarios atomic and independent (no inter-test ordering dependencies)
- [x] Migration of existing fixture assertions called out explicitly (TECH-001)
