# Requirements Traceability Matrix

## Story: TEA-DX-001.5 — Better `dynamic_parallel` error messages

**Reviewer:** Quinn (Test Architect) · **Date:** 2026-05-01 · **Mode:** YOLO
**Story status at trace:** Draft (implementation not yet started; new tests
not yet authored — only the legacy assertions that must be migrated exist)
**Planning reference:** [`TEA-DX-001.5-test-design-20260501.md`](TEA-DX-001.5-test-design-20260501.md)
**Risk profile:** [`TEA-DX-001.5-risk-20260501.md`](TEA-DX-001.5-risk-20260501.md)
**NFR assessment:** [`TEA-DX-001.5-nfr-20260501.md`](TEA-DX-001.5-nfr-20260501.md)

---

### Coverage Summary

| Metric | Count | % |
|--------|-------|---|
| Total Requirements (ACs, including 3 proposed) | 13 | 100% |
| Planned Coverage — FULL | 13 | 100% |
| Planned Coverage — PARTIAL | 0 | 0% |
| Planned Coverage — NONE | 0 | 0% |
| **Implemented today (against current `main`)** | **0** | **0%** |
| Legacy assertions (must be migrated, not removed) | 5 | — |

**Interpretation.** The test-design document specifies 21 scenarios (17 unit +
3 integration + 1 doc) that, *once authored*, fully cover every AC. At trace
time, none of those scenarios have been implemented. Five legacy assertions
in `python/tests/test_yaml_dynamic_parallel.py` (lines 50, 73, 98, 119, 143)
match on the **old** exact error strings; the same PR that ships this story
must rewrite those to substring/structural assertions or they will break the
suite the moment the helper lands. This is the dominant pre-merge risk
(TECH-001) and is reflected in the risk profile.

---

### Requirement Mappings

#### AC-1: Each `ValueError` includes node name, key path, example, doc anchor

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-001` — node-name substring assertion
  - **Given:** A `dynamic_parallel` YAML node misnamed as `mynode_xyz` is
    rendered to trigger each of the 5 error codes.
  - **When:** `YAMLEngine.load_from_dict(...)` is invoked and raises.
  - **Then:** `"mynode_xyz"` appears as a substring in every message.
- **Unit Test (planned):** `UNIT-002` — doc-anchor substring assertion
  - **Given:** Each of the 5 error codes triggered.
  - **When:** The exception message is read.
  - **Then:** `docs/shared/YAML_REFERENCE.md#dynamic-parallel` is present in
    every message.
- **Unit Test (planned):** `UNIT-003` — example-fragment presence
  - **Given:** Each of the 5 error codes triggered.
  - **When:** Message lines are scanned.
  - **Then:** At least one line begins with two-space indent + a YAML key,
    proving an actionable example fragment is present.

#### AC-2: Conflicting `action` / `steps` / `subgraph` keys named explicitly

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-006` — `action:` + `steps:`
  - **Given:** YAML defines both `action:` and `steps:` siblings.
  - **When:** Engine parses and raises.
  - **Then:** Message names exactly the two conflicting keys (`action`,
    `steps`) and suggests removing one.
- **Unit Test (planned):** `UNIT-007` — `action:` + `subgraph:` analog.
- **Unit Test (planned):** `UNIT-008` — `steps:` + `subgraph:` analog.
- **Unit Test (planned):** `UNIT-009` — all three (`action`, `steps`,
  `subgraph`) present
  - **Given:** YAML defines all three execution modes.
  - **When:** Engine raises.
  - **Then:** Message lists all three and suggests removing two — must not
    silently degrade to a 2-of-3 message.

#### AC-3: Missing `fan_in:` includes guidance on where it goes

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-011` — sibling-of-items hint + example
  - **Given:** YAML with `items:` set but no `fan_in:` key.
  - **When:** Engine raises.
  - **Then:** Message contains the substring `sibling of items`
    (case-insensitive) AND a YAML example fragment with `fan_in:` at the
    same indent level as `items:`.

#### AC-4: Invalid `fan_in:` target → clearer error with candidate list

**Planned coverage: FULL · Actual coverage today: NONE**

- **Integration Test (planned):** `INT-001` — `fan_in: missing_node`
  - **Given:** A two-node graph with `dynamic_parallel.fan_in: missing_node`
    and an unrelated `aggregate:` node declared.
  - **When:** Engine loads the graph.
  - **Then:** `ValueError` includes `fan_in target 'missing_node' is not a
    defined node` AND lists the declared nodes
    (e.g., `[aggregate, the_dynamic_parallel_node]`).
- **Integration Test (planned):** `INT-002` — `fan_in:` declared **after**
  - **Given:** YAML where the `fan_in:` target node appears later in the
    file than the `dynamic_parallel` node.
  - **When:** Engine loads.
  - **Then:** No error — forward reference resolution works (TECH-002).
- **Integration Test (planned):** `INT-003` — `fan_in:` declared **before**
  - **Given:** YAML where the target is declared earlier.
  - **When:** Engine loads.
  - **Then:** No error — early-bound symmetry holds.
- **Unit Test (planned):** `UNIT-012` — misspelling + candidate list
  - **Given:** `fan_in: agregate` against a declared `aggregate` node.
  - **When:** Engine raises.
  - **Then:** Message includes the full declared-node list (and ideally a
    "did you mean" suggestion). Message helpfulness — not full fuzzy match.
- **Unit Test (planned):** `UNIT-013` — reserved-name target
  - **Given:** `fan_in: __end__` (or `__start__`).
  - **When:** Engine processes.
  - **Then:** Behavior is *defined* by the test — accept (terminal) or
    reject with a specific message; the test locks the chosen contract.

#### AC-5: Missing `items:` includes example `items: "{{ state.batches }}"`

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-014` — literal example assertion
  - **Given:** YAML with no `items:` key.
  - **When:** Engine raises.
  - **Then:** Message contains the **literal** substring
    `items: "{{ state.batches }}"` — exact text per AC-5.

#### AC-6: Existing valid `dynamic_parallel` workflows continue to work

**Planned coverage: FULL · Actual coverage today: PARTIAL**

- **Integration Test (planned):** `INT-004` — examples sweep regression
  - **Given:** Every YAML under `examples/yaml/**` that contains
    `dynamic_parallel`.
  - **When:** Each is loaded via `YAMLEngine` and (where deterministic)
    invoked to `__end__`.
  - **Then:** No new errors; behavior matches pre-change baseline.
- **Existing baseline (already green on `main`):** `TestDynamicParallelItemsEvaluation`,
  `TestDynamicParallelActionMode`, `TestDynamicParallelStepsMode`,
  `TestDynamicParallelSubgraphMode`, `TestDynamicParallelFanInCollection`
  in `python/tests/test_yaml_dynamic_parallel.py` exercise valid workflows
  end-to-end and must remain green post-merge. They cover most of AC-6
  positively; INT-004 closes the gap on rarely-loaded `examples/yaml/` files.

#### AC-7: Error type remains `ValueError` (not a subclass)

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-015` — strict type check
  - **Given:** Each of the 5 error codes triggered.
  - **When:** The exception is caught.
  - **Then:** `type(exc) is ValueError` — strict identity check (NOT
    `isinstance`); catches accidental subclassing that would break callers
    relying on exact-type `except` blocks.

#### AC-8: 80-char wrap, plain ASCII

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-005` — line length + ASCII purity
  - **Given:** Each of the 5 error codes triggered.
  - **When:** `message.splitlines()` is iterated.
  - **Then:** Each line is ≤ 80 chars AND contains no codepoints > 127.

#### AC-9: Unit test for each error code

**Planned coverage: FULL · Actual coverage today: NONE**

Satisfied by the per-code coverage:

- Missing `items:` → `UNIT-014`
- Zero of {action, steps, subgraph} → `UNIT-010`
- Two/three of {action, steps, subgraph} → `UNIT-006/007/008/009`
- Missing `fan_in:` → `UNIT-011`
- Invalid `fan_in:` target → `INT-001`
- Invalid `max_concurrency` → `UNIT-018`

  - **Given:** Each error code is reachable from a minimal YAML fixture.
  - **When:** Engine parses and raises.
  - **Then:** A dedicated test asserts the new message contents (node name,
    key path, example, doc anchor) — one assertion per code per AC-9.

#### AC-10: No new dependencies

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit/DoD Test (planned):** `UNIT-016` — dependency diff
  - **Given:** `python/setup.py` and `python/pyproject.toml` on the PR branch.
  - **When:** Diffed against `origin/main`.
  - **Then:** No new entries in `install_requires` or `[project] dependencies`.
    Implementable as a CI diff or a manual DoD checkbox.

#### AC-11 (NFR-proposed): Example fragments are valid YAML

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-004` — example-parse safeguard
  - **Given:** The error-template constants table is introspected to yield
    `(code, template, example, doc_anchor)` tuples.
  - **When:** `yaml.safe_load(example)` is invoked on each fragment.
  - **Then:** No exception — the example fragment is, itself, valid YAML.
    Guards against template drift (TECH-004).

#### AC-12 (NFR-proposed): `#dynamic-parallel` anchor exists in `YAML_REFERENCE.md`

**Planned coverage: FULL · Actual coverage today: NONE**

- **Doc Test (planned):** `DOC-001` — anchor existence assertion
  - **Given:** `docs/shared/YAML_REFERENCE.md` is read at test time.
  - **When:** Headings are scanned.
  - **Then:** A heading whose GitHub-style slug equals `dynamic-parallel`
    exists (regex `^#+\s+Dynamic\s+Parallel\s*$`, case-insensitive).
    Auto-runnable as a pytest, not just a manual review (OPS-001).

#### AC-13 (NFR-proposed): Grep-friendly first line

**Planned coverage: FULL · Actual coverage today: NONE**

- **Unit Test (planned):** `UNIT-017` — first-line summary
  - **Given:** Each of the 5 error codes triggered.
  - **When:** `message.splitlines()[0]` is inspected.
  - **Then:** The first line contains both the node name AND a short error
    category substring (e.g., `missing 'items'`, `invalid fan_in target`).
    Codifies DATA-001 mitigation as a test.

---

### Out-of-Story-AC Mappings (covered by test design)

These scenarios are not bound to a specific original AC but harden the
implementation. The test-design document treats them as essential coverage
for the fourth refactor site (`max_concurrency`) plus defensive cross-cuts.

| Test ID | Scenario | What it protects | AC link |
|---------|----------|-------------------|---------|
| UNIT-018 | `max_concurrency: 0` and `-1` raise with valid range + example | Refactor target #4 keeps behavior and gains a richer message | AC-1, AC-9 |
| UNIT-019 | `max_concurrency: "{{ state.workers }}"` does not raise at parse time | Locks the existing skip-validation-for-Jinja behavior at `yaml_nodes.py:1167` | AC-6 (regression) |
| UNIT-020 | `_format_dynamic_parallel_error` is module-level importable with stable signature | TEA-DX-001.6 reuse path (TECH-003) | (cross-story) |
| UNIT-021 | Node config with sibling `api_key: "secret-token"` does not appear in any error message | Defensive — confirms helper interpolates only explicit context (NFR security advisory) | (NFR) |

Recommend keeping all four in the suite — they are cheap and protect the
long tail.

---

### Legacy Test Migration (TECH-001 — Must Fix Before Merge)

The same PR that ships this story must rewrite the five existing
`pytest.raises(..., match=...)` assertions in
`python/tests/test_yaml_dynamic_parallel.py` from full-string regex matches
to substring/structural assertions. Otherwise they will break the moment
the helper lands.

| File:Line | Current match string (legacy) | Migration target |
|-----------|-------------------------------|-------------------|
| `test_yaml_dynamic_parallel.py:50` | `"requires 'items' expression"` | Substring check on node name + doc anchor + presence of `items: "{{ state.batches }}"` example |
| `test_yaml_dynamic_parallel.py:73` | `"requires exactly one of: action, steps, subgraph"` | Substring check on node name + doc anchor + listing of all three keys (zero-of-3 path) |
| `test_yaml_dynamic_parallel.py:98` | `"requires exactly one of: action, steps, subgraph"` | Substring check on node name + doc anchor + naming of the two specific conflicting keys (e.g., `action` and `steps`) |
| `test_yaml_dynamic_parallel.py:119` | `"requires 'fan_in' target node"` | Substring check on node name + doc anchor + `sibling of items` placement hint |
| `test_yaml_dynamic_parallel.py:143` | `"max_concurrency must be positive integer"` | Substring check on node name + doc anchor + `max_concurrency: 4` example fragment |

These migrations are *replacements*, not additions; the new
UNIT-006/UNIT-010/UNIT-011/UNIT-014/UNIT-018 scenarios cover the same
ground with richer assertions and the legacy tests should be deleted or
folded into them rather than kept as duplicates.

---

### Critical Gaps

#### Gap 1 — All planned scenarios are unimplemented
- **Severity:** EXPECTED (story is Draft; this is the implementing dev's job).
- **Risk at trace time:** None — tracked as a checklist for the dev agent.
- **Action:** When implementing, follow the test-design's recommended
  execution order: P0 unit (UNIT-001/002/003/006/010/011/014) → P0
  integration (INT-001, INT-004) → P1 unit + integration + docs → P2 → P3.

#### Gap 2 — Doc anchor depends on TEA-DX-001.8 OR an in-PR placeholder
- **Severity:** Medium (blocks AC-1 + AC-12 + UNIT-002 + DOC-001).
- **Risk:** If `#dynamic-parallel` does not resolve in `YAML_REFERENCE.md`,
  every error message points at a 404 anchor, defeating the user-help
  intent of the story. The risk profile flagged this as OPS-001 with a
  must-fix mitigation: land a placeholder section in the same PR.
- **Action:** Add a `## Dynamic Parallel` heading in
  `docs/shared/YAML_REFERENCE.md` in the same PR (TEA-DX-001.8 fleshes it
  out later). DOC-001 enforces this automatically.

#### Gap 3 — `fan_in` registry-access timing is unresolved
- **Severity:** Medium (TECH-002 from risk profile).
- **Risk:** AC-4 requires that when `fan_in:` references an undefined node,
  a separate error fires with the candidate list. This needs access to the
  declared-nodes registry at validation time. If the implementer chooses
  pre-registration validation, INT-002 (forward references) breaks; if
  they choose post-registration, the error site moves from
  `_create_dynamic_parallel_function` to a graph-build step.
- **Action:** Decide explicitly during implementation; INT-001/002/003 +
  UNIT-013 codify all four corners of the decision matrix. The risk
  profile recommends a post-registration pass or a snapshot/closure of
  known node names threaded through.

#### Gap 4 — UNIT-013 contract is not pre-decided
- **Severity:** Low.
- **Risk:** The reserved-name case (`fan_in: __end__` / `__start__`) has no
  predetermined behavior; UNIT-013 will codify whatever the implementer
  picks. If an undocumented choice is made, future readers won't know it
  was intentional.
- **Action:** Capture the decision in the test docstring AND the
  YAML_REFERENCE doc anchor section.

#### Gap 5 — UNIT-016 (dependency diff) may not run in CI
- **Severity:** Low.
- **Risk:** AC-10 ("no new dependencies") is enforced as a P3 DoD checkbox
  if not wired into CI. Easy to overlook in review.
- **Action:** Either add a CI step diffing `setup.py` + `pyproject.toml`
  against `origin/main`, or ensure the DoD checklist explicitly calls it
  out before the gate flips to PASS.

---

### Test Design Recommendations

The test-design document is the source of truth for scenarios. Based on
this trace, the only additional recommendations are operational:

1. **Migrate legacy assertions in the same PR.** The five lines listed in
   the migration table above are the highest-priority pre-merge work; a
   broken suite is the most likely failure mode for this story.
2. **Author tests in execution order.** P0 unit (UNIT-001/002/003/006/010
   /011/014) gives fastest signal on the matrix; INT-001 + INT-004 then
   close the registry-boundary and regression gaps before P1 work begins.
3. **Land the doc anchor placeholder atomically with the helper.**
   Otherwise UNIT-002 and DOC-001 cannot pass and the user-facing benefit
   of the story collapses.
4. **Use parametrized fixtures.** UNIT-001/002/003/004/005/015/017 are all
   "do this assertion against each of the 5 error codes" — pytest's
   `parametrize` over `(code, template, example, doc_anchor)` tuples will
   keep the test count low and the maintenance cost flat as new error
   codes are added.
5. **Treat UNIT-021 (secret-leak guard) as non-optional.** It is a P1
   defensive test; the helper's interpolation logic is the single point
   where a misuse could leak sibling-key values into log output.

---

### Risk Coverage (per-AC)

| Risk Tier | Requirements |
|-----------|--------------|
| **High Risk (no planned coverage)** | None |
| **Medium Risk (partial planned coverage)** | None |
| **Low Risk (full unit + integration planned)** | AC-4, AC-6 |
| **Low Risk (full unit-only planned)** | AC-1, AC-2, AC-3, AC-5, AC-7, AC-8, AC-9, AC-11, AC-13 |
| **Low Risk (DoD checkbox-only)** | AC-10 |
| **Low Risk (doc-test only)** | AC-12 |

All ACs are at Low Risk *by design*: the change is a parse-time error-message
formatting refactor with one new validity check. The risk-profile assessment
(90/100, LOW) is consistent with this trace. The single Medium-tier risk
(TECH-002 fan_in registry timing) is mitigated by INT-001/002/003 + UNIT-013
covering all four corners of the registry-access decision.

---

### Risk-to-Test Mapping

| Risk ID  | Description                                                           | Mitigating Tests                                          |
|----------|------------------------------------------------------------------------|-----------------------------------------------------------|
| TECH-001 | Existing tests assert old exact error strings — break on rewrite       | Legacy migration (above) + INT-004 + UNIT-018 + UNIT-019  |
| TECH-002 | `fan_in` existence check needs declared-node registry access           | INT-001, INT-002, INT-003, UNIT-013                       |
| TECH-003 | Helper-function placement diverges from TEA-DX-001.6                   | UNIT-020                                                  |
| TECH-004 | Message templates drift from real syntax as YAML schema evolves        | UNIT-004                                                  |
| OPS-001  | Doc anchor URL points at section that doesn't exist yet                | UNIT-002, DOC-001                                         |
| BUS-001  | New messages still fail to help if examples are too generic            | UNIT-003, UNIT-006, UNIT-011, UNIT-012, UNIT-014          |
| DATA-001 | Multi-line / wrapped messages break log-parsing pipelines              | UNIT-001, UNIT-005, UNIT-017                              |

Every risk in the risk profile has at least one mitigating test mapped to
an AC. No risk is uncovered.

---

### Gate YAML Block

```yaml
trace:
  totals:
    requirements: 13
    full: 13
    partial: 0
    none: 0
  planning_ref: 'docs/qa/assessments/TEA-DX-001.5-test-design-20260501.md'
  uncovered: []
  implementation_status:
    tests_authored: 0
    tests_planned: 21
    legacy_assertions_to_migrate: 5
    note: 'Story is Draft; full planned coverage from test-design, zero authored against current main. Five legacy `pytest.raises(..., match=...)` assertions in test_yaml_dynamic_parallel.py must be migrated in the same PR (TECH-001).'
  ac_to_test_map:
    AC-1:  [UNIT-001, UNIT-002, UNIT-003]
    AC-2:  [UNIT-006, UNIT-007, UNIT-008, UNIT-009]
    AC-3:  [UNIT-011]
    AC-4:  [INT-001, INT-002, INT-003, UNIT-012, UNIT-013]
    AC-5:  [UNIT-014]
    AC-6:  [INT-004]
    AC-7:  [UNIT-015]
    AC-8:  [UNIT-005]
    AC-9:  [UNIT-006, UNIT-010, UNIT-011, UNIT-014, INT-001, UNIT-018]
    AC-10: [UNIT-016]
    AC-11: [UNIT-004]
    AC-12: [DOC-001]
    AC-13: [UNIT-017]
  defensive_extras: [UNIT-018, UNIT-019, UNIT-020, UNIT-021]
  risk_mitigations:
    TECH-001: [legacy_migration, INT-004, UNIT-018, UNIT-019]
    TECH-002: [INT-001, INT-002, INT-003, UNIT-013]
    TECH-003: [UNIT-020]
    TECH-004: [UNIT-004]
    OPS-001:  [UNIT-002, DOC-001]
    BUS-001:  [UNIT-003, UNIT-006, UNIT-011, UNIT-012, UNIT-014]
    DATA-001: [UNIT-001, UNIT-005, UNIT-017]
  notes: 'See docs/qa/assessments/TEA-DX-001.5-trace-20260501.md'
```

---

### Story Hook Line

```text
Trace matrix: docs/qa/assessments/TEA-DX-001.5-trace-20260501.md
```
