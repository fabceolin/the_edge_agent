# Test Design: Story TEA-DX-001.7 — Quiet-mode heartbeat

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO

## Test Strategy Overview

- **Total test scenarios:** 22
- **Unit tests:** 7 (32%)
- **Integration tests:** 13 (59%)
- **E2E tests:** 2 (9%)
- **Priority distribution:** P0: 9, P1: 8, P2: 4, P3: 1

Strategy notes:
- Unit layer focuses on pure logic (duration formatting, node-name prefix derivation, guard predicate). These are cheap, fast, and exhaustively cover formatter boundaries called out in TECH-1/AC-13.
- Integration layer drives the Typer `run` command via `CliRunner(mix_stderr=False)` to keep stdout/stderr observable independently. This is where most ACs land because the heartbeat is fundamentally an integration of CLI flag → event loop → stderr writer.
- E2E layer (subprocess) runs only where `CliRunner` is insufficient: confirming real OS-level stdout/stderr separation and CI-tooling smoke.
- Tests must use `CliRunner(mix_stderr=False)` (TEST-1 mitigation). Parallel-branch tests must NOT assert ordering across branches (TECH-2 mitigation).

## Test Scenarios by Acceptance Criteria

### AC-1 — New CLI flag `--heartbeat` (default off)

| ID                  | Level       | Priority | Test                                                              | Justification                                          |
| ------------------- | ----------- | -------- | ----------------------------------------------------------------- | ------------------------------------------------------ |
| TEA-DX-001.7-INT-001 | Integration | P0       | `tea run --help` lists `--heartbeat` with default=False           | CLI surface contract; cheapest to verify via Typer     |
| TEA-DX-001.7-INT-002 | Integration | P0       | Invoking `run` without `--heartbeat` produces no heartbeat lines  | Default-off contract (also covers AC-5/AC-14)          |

### AC-2 — Heartbeat line format `[<node_name> done in <duration>]` on stderr

| ID                  | Level       | Priority | Test                                                                                  | Justification                                                |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| TEA-DX-001.7-UNIT-001 | Unit      | P0       | `format_duration(0.05)` returns sub-second form (e.g. `50ms` or `0.1s` per impl)       | Pure logic; <100ms boundary (mitigates TECH-3/AC-13)         |
| TEA-DX-001.7-UNIT-002 | Unit      | P0       | `format_duration(1.2)` returns `1.2s`                                                  | Pure logic; common case named in AC-2                        |
| TEA-DX-001.7-UNIT-003 | Unit      | P0       | `format_duration(45.3)` returns `45.3s`                                                | Pure logic; named in AC-2                                    |
| TEA-DX-001.7-UNIT-004 | Unit      | P0       | `format_duration(138)` returns `2m 18s`                                                | Pure logic; minute-rollover boundary named in AC-2           |
| TEA-DX-001.7-UNIT-005 | Unit      | P1       | `format_duration(3700)` returns hour form (e.g. `1h 1m 40s`)                           | >1h boundary called out in AC-13                             |
| TEA-DX-001.7-INT-003 | Integration | P0       | 3-node sequential run with `--heartbeat`: stderr matches `^\[<node> done in .+\]$` per node, in order | Core behavior; covers AC-2, AC-10                            |

### AC-3 — `--heartbeat` + `--quiet` emits ONLY heartbeat lines

| ID                  | Level       | Priority | Test                                                                                       | Justification                                                  |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------ | -------------------------------------------------------------- |
| TEA-DX-001.7-INT-004 | Integration | P0       | `--quiet --heartbeat` 3-node run: stderr contains exactly N heartbeat lines, no chatter    | Compositional contract; closes COMPAT-2 silence guarantee gap  |

### AC-4 / AC-11 — Heartbeat goes to stderr, stdout unaffected

| ID                  | Level       | Priority | Test                                                                                          | Justification                                          |
| ------------------- | ----------- | -------- | --------------------------------------------------------------------------------------------- | ------------------------------------------------------ |
| TEA-DX-001.7-INT-005 | Integration | P0       | Invoke with `CliRunner(mix_stderr=False)` and `--heartbeat`; assert stdout has zero heartbeat lines | Stream isolation contract                              |
| TEA-DX-001.7-E2E-001 | E2E         | P1       | `subprocess.run(...)` with `--heartbeat`: stdout byte-identical to baseline run; stderr non-empty | Real OS pipe separation; defends against CliRunner artefacts |

### AC-5 / AC-14 — `--heartbeat` off → behavior unchanged (no heartbeat lines)

| ID                  | Level       | Priority | Test                                                                                       | Justification                              |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------ | ------------------------------------------ |
| TEA-DX-001.7-INT-006 | Integration | P0       | `--quiet` alone: stderr contains zero heartbeat-pattern lines on a successful 3-node run   | Regression guard for COMPAT-2              |
| TEA-DX-001.7-INT-007 | Integration | P1       | Default invocation (no flags): no heartbeat lines on stderr or stdout                      | Implicit-default regression guard          |

### AC-6 / AC-15 — Failed-node heartbeat `[<node> FAILED in <duration>]`

| ID                  | Level       | Priority | Test                                                                                                       | Justification                                                       |
| ------------------- | ----------- | -------- | ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| TEA-DX-001.7-INT-008 | Integration | P0       | Workflow whose middle node raises: stderr contains `[<node> FAILED in <duration>]`; exit code preserved    | Failure path behavior; covers AC-6                                   |
| TEA-DX-001.7-INT-009 | Integration | P1       | Same as INT-008 plus: parsed duration is > 0 and ≤ wall-clock bound captured in test                       | Defends TECH-3; closes AC-15                                         |

### AC-7 — Compatibility with `--quiet`, `--stream`, `--show-graph`

| ID                  | Level       | Priority | Test                                                                                              | Justification                                       |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| TEA-DX-001.7-INT-010 | Integration | P2       | `--heartbeat --stream` runs without crash; both heartbeat lines AND stream events present         | Documents redundant-but-not-harmful interaction     |
| TEA-DX-001.7-INT-011 | Integration | P2       | `--heartbeat --show-graph` runs without crash; graph rendering unaffected                         | Compatibility smoke                                 |

### AC-8 — Compatibility with `--debug-state` (TEA-DX-001.3)

| ID                  | Level       | Priority | Test                                                                                                  | Justification                                       |
| ------------------- | ----------- | -------- | ----------------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| TEA-DX-001.7-INT-012 | Integration | P2       | `--heartbeat --debug-state <dir>` runs without crash; heartbeat lines AND state dumps both produced   | Sibling-feature compatibility                       |

### AC-9 / AC-16 — Parallel branches each emit a prefixed heartbeat

| ID                  | Level       | Priority | Test                                                                                                          | Justification                                                       |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| TEA-DX-001.7-UNIT-006 | Unit      | P1       | Branch-prefix helper produces `parallel:<branch>` (or agreed format) given a parallel branch label             | Pure formatting function                                            |
| TEA-DX-001.7-INT-013 | Integration | P1       | Fan-out with K=3 parallel branches + `--heartbeat`: stderr contains exactly 3 prefixed lines (set-equal, NOT order-equal) | Per-branch attribution; explicitly avoids ordering brittleness (TECH-2) |

### AC-10 — 3-node workflow heartbeat assertion

Covered by **TEA-DX-001.7-INT-003** above.

### AC-11 — Heartbeat to stderr, not stdout

Covered by **TEA-DX-001.7-INT-005** and **TEA-DX-001.7-E2E-001** above.

### AC-12 — Documented in CLI reference

| ID                  | Level       | Priority | Test                                                                                                  | Justification                              |
| ------------------- | ----------- | -------- | ----------------------------------------------------------------------------------------------------- | ------------------------------------------ |
| TEA-DX-001.7-INT-014 | Integration | P1       | `tea run --help` output contains a `--heartbeat` description AND mentions stderr                      | Help text is the contract surface          |

### Tech Note Constraint — Exit codes preserved

| ID                  | Level       | Priority | Test                                                                            | Justification                              |
| ------------------- | ----------- | -------- | ------------------------------------------------------------------------------- | ------------------------------------------ |
| TEA-DX-001.7-UNIT-007 | Unit      | P1       | Guard predicate: heartbeat path never raises; exceptions in formatter swallowed safely | Defensive wiring around hot path           |
| TEA-DX-001.7-E2E-002 | E2E         | P3       | CI smoke: `subprocess.run(..., check=False)` long-ish workflow returns expected exit code under `--heartbeat` (success and failure scenarios) | Real-world OPS-1 sanity                    |

## Risk Coverage

Maps to risks identified in `TEA-DX-001.7-risk-20260501.md`:

| Risk ID  | Severity | Mitigated by                                                                                             |
| -------- | -------- | -------------------------------------------------------------------------------------------------------- |
| TECH-1   | MEDIUM   | INT-003 (sequential timing path), INT-008 (failure timing path); both assume the resolved design (wall-clock delta vs explicit `node_start`) — pick one *before* implementing |
| TECH-2   | LOW      | INT-013 (set-equal, no ordering); UNIT-006 (prefix correctness)                                          |
| TECH-3   | LOW      | INT-009 (failure duration > 0 and bounded); UNIT-007 (defensive guard)                                   |
| OPS-1    | LOW      | E2E-002 (CI smoke)                                                                                       |
| COMPAT-1 | LOW      | INT-010 (heartbeat + stream, no crash)                                                                   |
| COMPAT-2 | LOW/HIGH | INT-006, INT-007 (default-off regression guards); INT-002                                                |
| PERF-1   | MINIMAL  | Implicitly covered by INT-003 timing; no dedicated perf test needed                                      |
| SEC-1    | MINIMAL  | No dedicated test; node-name exposure parallels existing `--stream`                                      |
| TEST-1   | LOW      | All INT tests use `CliRunner(mix_stderr=False)`; E2E tests use real subprocess pipes                     |

## Test Data & Environment Requirements

**Fixtures / agents required:**

1. **`fixtures/heartbeat/three_node_seq.yaml`** — three sequential nodes (`a → b → c`) with non-trivial but bounded sleeps (e.g., `time.sleep(0.05)` each) to make duration > 0 deterministic without slowing CI.
2. **`fixtures/heartbeat/failing_middle.yaml`** — three-node workflow where node `b` raises `RuntimeError("boom")`. Used by INT-008/INT-009.
3. **`fixtures/heartbeat/parallel_fanout.yaml`** — fan-out to 3 branches that join at a fan-in node. Used by INT-013.
4. **`fixtures/heartbeat/with_state.yaml`** — small 2-node workflow used by INT-012 alongside `--debug-state` (reuse existing TEA-DX-001.3 fixture if compatible).

**Environment / tooling:**

- Python ≥ 3.9 (matches project floor); no extra deps.
- `pytest` with `capsys` / `capfd` available.
- `typer.testing.CliRunner(mix_stderr=False)` for all `INT-*` cases.
- `subprocess.run(...)` with explicit `stdout=PIPE, stderr=PIPE` for `E2E-*` cases.
- No network, DB, LTM backend, or LLM credentials required — heartbeat is a pure-CLI feature.

**Test data shape (INT-003 sample assertion):**

```python
result = runner.invoke(app, ["run", str(yaml_path), "--heartbeat"])
assert result.exit_code == 0
heartbeat_lines = [
    ln for ln in result.stderr.splitlines()
    if re.match(r"^\[\S+ done in \S+\]$", ln)
]
assert [ln.split()[0].strip("[") for ln in heartbeat_lines] == ["a", "b", "c"]
```

**Test data shape (INT-013 parallel — set semantics, no ordering):**

```python
heartbeat_lines = [ln for ln in result.stderr.splitlines() if "done in" in ln]
branches = {re.search(r"parallel:(\w+)", ln).group(1) for ln in heartbeat_lines}
assert branches == {"branch_1", "branch_2", "branch_3"}  # set, not list
```

**Negative-assertion shape (INT-006 quiet alone):**

```python
result = runner.invoke(app, ["run", str(yaml_path), "--quiet"])
assert result.exit_code == 0
assert not re.search(r"\[\S+ done in \S+\]", result.stderr)
```

## Recommended Execution Order

1. **P0 Unit** (UNIT-001..004) — duration formatter; fail fast on pure logic.
2. **P0 Integration** (INT-001..006, INT-008) — CLI surface, default-off, success path, failure path, stream isolation.
3. **P1 Unit** (UNIT-005..007) — hour boundary, prefix helper, defensive guard.
4. **P1 Integration** (INT-007, INT-009, INT-013, INT-014) — implicit default, failure duration bound, parallel attribution, help text.
5. **P1 E2E** (E2E-001) — real subprocess stream isolation.
6. **P2 Integration** (INT-010..012) — combined-flag matrix.
7. **P3 E2E** (E2E-002) — CI smoke (manual / nightly only).

## Coverage Gaps

None. Every AC (1–12) and the four QA-recommended additions (AC-13..AC-16) have at least one mapped scenario. The only unscored risk (SEC-1) is intentionally not promoted to a test scenario per the NFR assessment (parallels existing `--stream` exposure).

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 22
  by_level:
    unit: 7
    integration: 13
    e2e: 2
  by_priority:
    p0: 9
    p1: 8
    p2: 4
    p3: 1
  coverage_gaps: []
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.7-test-design-20260501.md
P0 tests identified: 9
```

## Quality Checklist

- [x] Every AC (1–12) and recommended AC (13–16) has test coverage
- [x] Test levels are appropriate (formatter → unit; CLI → integration; OS pipe semantics → e2e)
- [x] No duplicate coverage across levels (formatter only at unit; stream isolation split deliberately between INT-005 and E2E-001 for different threats)
- [x] Priorities align with risk profile (TECH-1, COMPAT-2 → P0)
- [x] Test IDs follow naming convention `TEA-DX-001.7-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent — no test depends on another's side effects
