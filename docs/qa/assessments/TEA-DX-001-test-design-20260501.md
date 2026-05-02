# Test Design: Epic TEA-DX-001 — YAML Runner Developer Experience

Date: 2026-05-01
Designer: Quinn (Test Architect)
Mode: YOLO

## Test Strategy Overview

This is an **epic-level** test design: it does not re-derive every per-story scenario (per-story files already exist for 7 of 8 stories), but instead (a) **rolls up** each child story's test counts into a single epic-wide matrix and (b) **adds cross-bundle scenarios** that no single child story owns — examples-yaml regression smoke, `--help` snapshot guards, validator/engine error parity, redaction reuse, and an end-to-end verification of the originating field-feedback scenario.

### Roll-up across child stories

| Story | Title                                          | Total | Unit | Int  | E2E | P0 | P1 | P2 | P3 | Source file |
|-------|------------------------------------------------|------:|-----:|-----:|----:|---:|---:|---:|---:|-------------|
| 1.1   | Settings-block env & template expansion        | 14    | 9    | 5    | 0   | 7  | 5  | 2  | 0  | `TEA-DX-001.1-test-design-20260501.md` |
| 1.2   | CLI `--trace-file` flag                        | 17    | 2    | 14   | 0   | 4  | 8  | 4  | 1  | `TEA-DX-001.2-test-design-20260501.md` |
| 1.3   | Intermediate state dumps for debug             | 18    | 7    | 10   | 1   | 7  | 7  | 3  | 1  | `TEA-DX-001.3-test-design-20260501.md` |
| 1.4   | `variables` in `run:` exec_globals             | 13    | 9    | 4    | 0   | 4  | 5  | 3  | 1  | `TEA-DX-001.4-test-design-20260501.md` |
| 1.5   | Better `dynamic_parallel` error messages       | —     | —    | —    | —   | —  | —  | —  | —  | **MISSING — to be authored before story leaves Draft** |
| 1.6   | `tea validate <workflow.yaml>` command         | 42    | 23   | 19   | 0   | 27 | 10 | 4  | 1  | `TEA-DX-001.6-test-design-20260501.md` |
| 1.7   | Quiet-mode heartbeat                           | 22    | 7    | 13   | 2   | 9  | 8  | 4  | 1  | `TEA-DX-001.7-test-design-20260501.md` |
| 1.8   | `action`/`steps`/`subgraph` doc table          | 18    | 7    | 8    | 3   | 7  | 7  | 3  | 1  | `TEA-DX-001.8-test-design-20260501.md` |
| **+ Epic-level (this doc)** | **Cross-bundle scenarios**         | **12**| **0**| **10**|**2** |**6**|**4**|**2**|**0**| `TEA-DX-001-test-design-20260501.md` |
| **TOTAL (with 1.5 pending)** |                                  | **156** | **64** | **83** | **8** | **71** | **54** | **25** | **5** | |

- Per-level mix: **Unit 41% · Integration 53% · E2E 5%** — appropriate for a CLI/config DX bundle. Shift-left bias is strong; E2E reserved for the one originating field-feedback scenario plus a few doc and heartbeat workflows.
- Per-priority mix: **P0 46% · P1 35% · P2 16% · P3 3%** — P0 share is inflated by Story 1.6 (validator: 27 P0 of 42 total) which is the riskiest single story in the bundle and hosts most of the structural-integrity contracts.
- **One gap:** Story 1.5 has no test design committed. Per NFR-AC-6 it must exist before the epic exits Draft.

### Strategy intent

1. **The per-story test designs are authoritative for each child story's ACs.** This document does not re-derive or re-number their scenarios. It cross-references them.
2. **Cross-story coverage gaps are caught here, not in any single story.** When the bundle ships together, certain regressions are only visible across stories (e.g., `--trace-file` flag from 1.2 must inherit env expansion from 1.1; debug-state from 1.3 must reuse the same redaction the existing checkpoint code is missing).
3. **Epic-level scenarios are exclusively integration- or E2E-level.** A unit-level concern that crosses stories is a sign the abstraction is wrong; flag it as a refactor instead.
4. **Every epic-level scenario maps to a recommended Epic-NFR-AC** from the NFR assessment. They are the executable counterpart to those advisory ACs.

---

## Epic-level Test Scenarios

These 12 scenarios are owned by the **bundle**, not by any single child story. They MUST land alongside the bundle (in any constituent PR) — assigning ownership belongs to whoever lands the *last* story of the bundle, OR to a separate scaffolding PR landed first.

### Bundle integrity & regression net (P0)

| ID                   | Level       | Priority | Test                                                                                                                                                                                                                                                                                                                            | Justification / Source                                                                                                                                                                                                  |
| -------------------- | ----------- | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| TEA-DX-001-INT-001   | Integration | P0       | Parametrized smoke: for every `examples/yaml/*.yaml`, `YAMLEngine(yaml_path)` instantiates without exception, and after Story 1.6 lands `tea validate <yaml>` exits 0. Fixture: glob `examples/yaml/**/*.yaml`. Asserts: no exception, no spurious WARNING about empty `trace_file`, exit code 0 for `tea validate`.            | Epic DoD: "No regression in `examples/yaml/` workflows". NFR-AC-3 (Reliability). Closes risk-profile gap that no single story currently promotes the examples-suite gate to AC level.                                  |
| TEA-DX-001-INT-002   | Integration | P0       | `tea run --help` snapshot test in `python/tests/cli/test_help_snapshots.py`. Strips trailing whitespace and ANSI codes; compares to `python/tests/snapshots/run_help.txt`. Updates required for Stories 1.2 (`--trace-file`), 1.3 (`--debug-state`), 1.7 (`--heartbeat`). PR breaks if flags drift without snapshot update.    | NFR-AC-2. Three new flags ship in this bundle without an enforced help-snapshot contract; this is the contract.                                                                                                         |
| TEA-DX-001-INT-003   | Integration | P0       | `tea validate --help` snapshot test in same file. Compares to `python/tests/snapshots/validate_help.txt`. Created by Story 1.6.                                                                                                                                                                                                  | NFR-AC-2. New subcommand needs the same drift guard.                                                                                                                                                                    |
| TEA-DX-001-INT-004   | Integration | P0       | Validator/engine error parity: for each broken fixture in `python/tests/fixtures/yaml_broken/*.yaml` (or equivalent: missing `fan_in`, duplicate `action`+`steps`, undefined edge target), assert that `tea validate <fixture>` and `YAMLEngine(<fixture>)` produce the **same set of error categories** (not byte-identical messages, but the same set of error codes / classes / fields-mentioned). | Epic risk profile TECH-001 (Story 1.6): validator extraction shifts `tea run` error timing/messages. Without a parity test, drift is invisible. Required testing priority 1.                                            |
| TEA-DX-001-INT-005   | Integration | P0       | Redaction reuse for `--debug-state`: load TEA-BUILTIN-012.3 redaction fixtures (nested dicts, exception arg with secret, traceback string) into a workflow that hits `--debug-state <dir>`. Read every dumped JSON file. Assert no canary marker present (e.g., `sk-CANARY-xxx`, `password=hunter2`).                            | Epic risk profile SEC-001 (Story 1.3) — secret/PII leakage. NFR Security FAIL today; this test MUST exist before AC-4 of Story 1.3 is considered done. Required testing priority 1.                                    |
| TEA-DX-001-INT-006   | Integration | P0       | Traceback redaction canary: a `run:` block that raises `RuntimeError("token=sk-CANARY-12345")`. With `--debug-state <dir>`, the FAILED-state dump must NOT contain the substring `sk-CANARY-12345`.                                                                                                                              | Epic risk profile SEC-001. Complements INT-005 with the traceback path — exception args / `__traceback__` strings are the most likely leakage surface.                                                                  |

### Cross-story integration (P1)

| ID                   | Level       | Priority | Test                                                                                                                                                                                                                                                                                                                              | Justification / Source                                                                                                                                                                                                                  |
| -------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| TEA-DX-001-INT-007   | Integration | P1       | `${VAR:-default}` env-expansion parity: write a workflow with `settings.trace_file: "${UNSET:-/tmp/yaml.jsonl}"`. Run twice — once relying on the YAML setting (Story 1.1), once with `tea run --trace-file '${UNSET:-/tmp/cli.jsonl}'` (Story 1.2). Assert both invocations produce a `FileExporter` whose path matches the resolved default. | NFR-AC-5 (cross-story ordering: 1.1 → 1.2). If 1.2 lands first or stops short of inheriting 1.1's expansion, this test catches the divergence at PR time.                                                                              |
| TEA-DX-001-INT-008   | Integration | P1       | Validator-then-error-messages dependency: with Story 1.6 landed, run an introspection test that asserts `the_edge_agent.yaml_validation` module exists AND Story 1.5's improved `dynamic_parallel` errors are produced through it. Skip-with-clear-marker if 1.5 has not landed.                                                  | NFR-AC-4 (cross-story ordering: 1.6 → 1.5). Forces the `yaml_validation.py` extraction to actually be consumed by 1.5 instead of duplicated.                                                                                            |
| TEA-DX-001-INT-009   | Integration | P1       | Help-snapshot drift CI guard: a CI step that runs INT-002 / INT-003 with `--update` disabled. Any unreviewed flag change (added, renamed, default changed, help-string edited) fails the build. Bonus: separate failure message between "you forgot to update the snapshot" and "the flag set actually changed".                  | NFR-AC-2 enforcement loop. Without a CI guard the snapshots drift during the bundle's parallel implementation.                                                                                                                          |
| TEA-DX-001-INT-010   | Integration | P2       | NFR-completeness check: a doc-presence test asserting that `docs/qa/assessments/TEA-DX-001.{1..8}-nfr-20260501.md` all exist. Fails if any child-story NFR is missing.                                                                                                                                                              | NFR-AC-6. Story 1.5 NFR is the current outstanding item; this test forces visibility.                                                                                                                                                   |

### Originating-feedback E2E (P0/P1)

| ID                   | Level       | Priority | Test                                                                                                                                                                                                                                                                                                                                              | Justification / Source                                                                                                                                                                                                                                                                  |
| -------------------- | ----------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| TEA-DX-001-E2E-001   | E2E         | P0       | External-runner replacement: a small shell-driven test that mimics the field-feedback runner. Old way: render YAML to a temp file with a per-run `trace_file`. New way: `tea run sample.yaml --trace-file "$(mktemp).jsonl"`. Asserts both paths produce equivalent trace JSONL output and the new way needs zero YAML rewrites.                | Epic Success Criteria: "External runner integration (the source of this feedback) can replace its YAML-templating workaround with a single CLI flag." Without this, the bundle technically ships its features but does not verify the originating UX problem is solved.                |
| TEA-DX-001-E2E-002   | E2E         | P1       | `plan_batches`-style mid-run failure: a workflow with three nodes A→B→C where B raises. Run with `--debug-state <dir>` and `--output result.json`. Assert: `result.json` exists (even if empty/partial), `state-after-A.json` exists with A's output, FAILED dump for B exists with sanitized traceback, `state-after-C.json` does NOT exist. | Epic Success Criteria + Story 1.3 originating field-feedback (mid-run failure leaves blind debug). Verifies the `--output` blindness gap is closed end-to-end.                                                                                                                          |

---

## Acceptance-Criteria Coverage (Epic-level)

The epic story enumerates 6 success criteria and 5 DoD items. These map to the 12 epic-level scenarios above (per-story ACs are covered by per-story test designs, not re-listed here).

| Epic Success Criterion / DoD                                                                              | Covered by                                          |
|-----------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| All 8 child stories merged with passing tests                                                              | CI: per-story test suites (not in this doc)         |
| No breaking changes to existing YAML files in `examples/`                                                  | TEA-DX-001-INT-001                                  |
| `tea run --help` shows `--trace-file`, `--debug-state`, and (if implemented) `--heartbeat`                 | TEA-DX-001-INT-002, TEA-DX-001-INT-009              |
| `tea validate <yaml>` exits 0 on valid workflows, non-zero with line-anchored errors on invalid            | TEA-DX-001-INT-001, TEA-DX-001-INT-003, Story 1.6   |
| `docs/shared/YAML_REFERENCE.md` includes a `dynamic_parallel` mode comparison table                        | Story 1.8 per-story tests                           |
| External runner integration can replace YAML-templating workaround with a single CLI flag                  | TEA-DX-001-E2E-001                                  |
| `pytest python/tests/` green                                                                               | CI: every per-story PR                              |
| `tea run --help` and `tea validate --help` reflect new flags                                               | TEA-DX-001-INT-002, TEA-DX-001-INT-003              |
| `docs/shared/YAML_REFERENCE.md` updated for stories 1, 8                                                   | Stories 1.1 & 1.8 per-story tests                   |
| External-runner pain points (Stories 1-3) verified end-to-end against a sample runner script              | TEA-DX-001-E2E-001, TEA-DX-001-E2E-002              |
| No regression in `examples/yaml/` workflows                                                                | TEA-DX-001-INT-001                                  |

**Coverage gap:** Story 1.5 has no test design committed. Until it lands, the **scenario count and per-story coverage table above are incomplete**. Recommend the QA agent be re-invoked with `*test-design TEA-DX-001.5` before the epic exits Draft.

---

## Risk Coverage (Epic-level)

Maps the 12 epic-level scenarios to the highest-priority risks from `TEA-DX-001-risk-20260501.md`:

| Risk                                                          | Severity | Mitigated by                                                            |
|---------------------------------------------------------------|----------|-------------------------------------------------------------------------|
| SEC-001 (Story 1.3): Secret/PII leakage via debug dumps       | High (6) | TEA-DX-001-INT-005, TEA-DX-001-INT-006, TEA-DX-001-E2E-002              |
| TECH-002 (Story 1.1): AC-4 contradicts `expand_env_vars`      | High (6) | Story 1.1 per-story design (UNIT-006, UNIT-007); reinforced by INT-001  |
| TECH-001 (Story 1.6): Validator extraction shifts error timing | High (6) | TEA-DX-001-INT-004 (parity)                                             |
| TECH-005 (Story 1.1): Empty-string silent FileExporter drop   | Med (4)  | Story 1.1 per-story design + TEA-DX-001-INT-001 (no spurious WARNING)   |
| DATA-001 (Story 1.3): Disk fill from large state              | Med (4)  | Per-story design; epic-level `--help` text validated by INT-002         |
| OPS-001 (Story 1.3): `--debug-state` accidentally on in prod  | Med (4)  | Per-story design; no epic-level test (single-story concern)             |
| TECH-001 (Story 1.3): Parallel branch event interleaving      | Med (4)  | Per-story design + indirectly TEA-DX-001-E2E-002                        |
| BUS-001 (Story 1.6): `--strict` false positives erode trust   | Med (4)  | Per-story design + TEA-DX-001-INT-001 zero-warning baseline             |
| BUS-002 (Story 1.6): Validator gives false confidence         | Med (4)  | TEA-DX-001-INT-004 (parity ensures runtime/validator alignment)         |
| Cross-story drift (1.1 ↔ 1.2, 1.6 ↔ 1.5)                     | Med (4)  | TEA-DX-001-INT-007, TEA-DX-001-INT-008                                  |
| `examples/yaml/` regression                                   | Med (4)  | TEA-DX-001-INT-001                                                      |
| `--help` flag drift                                           | Low (2)  | TEA-DX-001-INT-002, TEA-DX-001-INT-003, TEA-DX-001-INT-009              |

---

## Test Data & Environment Requirements

### Fixtures

1. **`examples/yaml/**/*.yaml`** — used as-is by INT-001. No new files.
2. **`python/tests/fixtures/yaml_broken/`** — REQUIRED to exist by Story 1.6 per its test design. Reused by INT-004. If 1.6 lands first, INT-004 inherits the directory automatically; otherwise INT-004 contributes the directory.
3. **`python/tests/snapshots/run_help.txt` and `validate_help.txt`** — created by INT-002 / INT-003. One-time scaffolding.
4. **TEA-BUILTIN-012.3 redaction fixtures** — leverage the existing fixture/helper from the checkpoint redaction story. If that story has not landed in this codebase, the SEC-001 mitigation is **deferred** until it does, and Story 1.3 cannot exit Draft per NFR Security FAIL gate.
5. **Sample external-runner script** — small bash/Python harness for E2E-001. Lives in `python/tests/scripts/` or equivalent. Exercises both old (YAML-render) and new (`--trace-file`) UX paths.

### Environment / tooling

- Python ≥ 3.9; `pytest`; `Click`/`Typer.testing.CliRunner` for CLI subcommand exec; `subprocess` for E2E shell-driven tests; `monkeypatch` for env vars; `caplog` for warning assertions; `tmp_path` for temp dirs.
- **No** network, **no** real LLM credentials, **no** real LTM backend. Every scenario must be runnable on a stock CI runner with `pip install -e .[dev]` and nothing else.
- **One** new dependency tolerated only if Story 1.6 brings it in (e.g., `pyyaml` is already present; no PR may introduce a net-new top-level dep through this bundle per Story 1.1 AC-9 and Story 1.6 NFR notes).

### Sample assertion shapes

Examples-yaml smoke (INT-001):
```python
import pathlib, pytest, yaml
from the_edge_agent.yaml_engine import YAMLEngine

EXAMPLES = sorted(pathlib.Path("examples/yaml").rglob("*.yaml"))

@pytest.mark.parametrize("path", EXAMPLES, ids=[str(p) for p in EXAMPLES])
def test_examples_yaml_loads_clean(path, caplog):
    with caplog.at_level("WARNING"):
        engine = YAMLEngine(str(path))
    assert engine is not None
    assert not [r for r in caplog.records
                if "trace_file" in r.message and "empty" in r.message], \
        "Spurious empty-trace_file warning on shipped example"
```

Help snapshot (INT-002):
```python
from typer.testing import CliRunner
from the_edge_agent.cli import app

def test_run_help_snapshot(snapshot):
    result = CliRunner().invoke(app, ["run", "--help"])
    assert result.exit_code == 0
    snapshot.assert_match(result.stdout, "run_help.txt")
```

Validator/engine parity (INT-004):
```python
import pathlib, pytest
from typer.testing import CliRunner
from the_edge_agent.cli import app
from the_edge_agent.yaml_engine import YAMLEngine

BROKEN = sorted(pathlib.Path("python/tests/fixtures/yaml_broken").glob("*.yaml"))

@pytest.mark.parametrize("path", BROKEN, ids=[p.name for p in BROKEN])
def test_validator_engine_error_parity(path):
    runner_result = CliRunner().invoke(app, ["validate", str(path)])
    try:
        YAMLEngine(str(path))
        engine_errors = []
    except Exception as e:
        engine_errors = sorted({type(e).__name__})
    cli_errors = sorted(_extract_error_codes(runner_result.stdout))
    assert cli_errors == engine_errors, \
        f"Validator/engine error sets diverge for {path.name}"
```

Redaction reuse (INT-005):
```python
import json, pathlib
from typer.testing import CliRunner
from the_edge_agent.cli import app

def test_debug_state_dumps_are_redacted(tmp_path, caplog):
    workflow = tmp_path / "leak.yaml"
    workflow.write_text(_LEAKY_WORKFLOW_WITH_SECRETS)
    debug_dir = tmp_path / "debug"
    CliRunner().invoke(app, [
        "run", str(workflow), "--debug-state", str(debug_dir)
    ])
    for dump in debug_dir.glob("state-after-*.json"):
        body = dump.read_text()
        assert "sk-CANARY" not in body, f"Secret leaked in {dump.name}"
        assert "hunter2"   not in body, f"Password leaked in {dump.name}"
```

External-runner replacement (E2E-001):
```python
import subprocess, tempfile, pathlib

def test_external_runner_replacement():
    yaml_path = pathlib.Path("python/tests/fixtures/external_runner_sample.yaml")
    with tempfile.NamedTemporaryFile(suffix=".jsonl", delete=False) as tf:
        trace_path = tf.name
    result = subprocess.run(
        ["tea", "run", str(yaml_path), "--trace-file", trace_path],
        capture_output=True, text=True
    )
    assert result.returncode == 0
    assert pathlib.Path(trace_path).stat().st_size > 0, \
        "Trace file must be populated by --trace-file flag"
```

---

## Recommended Execution Order

1. **P0 epic Integration** (INT-001, INT-002, INT-003, INT-004, INT-005, INT-006) — establish the bundle's regression net before merging any individual story; SEC-001 / TECH-001 (1.6) / examples-yaml are the load-bearing gates.
2. **P0 epic E2E** (E2E-001) — verify the originating field-feedback UX problem is solved.
3. **P1 epic Integration** (INT-007, INT-008, INT-009) — cross-story drift guards.
4. **P1 epic E2E** (E2E-002) — `plan_batches`-style mid-run failure debug verification.
5. **P2 epic Integration** (INT-010) — NFR doc-presence guard.
6. **Per-story P0 → P1 → P2 → P3** — see each child story's per-story test design for ordering.

---

## Coverage Gaps & Open Items

1. **Story 1.5 has no per-story test design committed.** Re-run `*test-design TEA-DX-001.5` before the epic exits Draft (per NFR-AC-6).
2. **TEA-DX-001-INT-005 / INT-006 depend on TEA-BUILTIN-012.3 redaction primitives that do not exist in the codebase today** (per NFR Security FAIL). The tests can be written now using a placeholder `redact_state()` helper, but they will fail until either (a) Story 1.3's AC-4 is rewritten to one of the three resolution paths (Option A: build `redact_state` helper; Option B: no redaction + startup banner; Option C: per-call `sanitize_keys` opt-in) AND the chosen path lands, or (b) `redact_state` is shipped under a separate epic. **Epic cannot enter implementation without resolving this.**
3. **TEA-DX-001-INT-008 (validator-then-error-messages dependency) has a soft-skip path** if Story 1.5 lands without 1.6. Recommend INT-008 be implemented as a hard fail if 1.5 ships and 1.6 has not — the soft-skip is a documentation-of-intent only.
4. **Story 1.7 NFR is PASS but NFR Reliability is CONCERNS** because heartbeat depends on a non-existent `node_start` event. The per-story test design (1.7) covers this. No epic-level scenario added; per-story coverage is sufficient.

---

## Gate YAML Block

```yaml
test_design:
  scope: epic
  scenarios_total: 156   # 144 across 7 child stories + 12 epic-level; story 1.5 still pending
  by_story:
    "TEA-DX-001.1": 14
    "TEA-DX-001.2": 17
    "TEA-DX-001.3": 18
    "TEA-DX-001.4": 13
    "TEA-DX-001.5": 0    # MISSING — pre-condition for epic Done
    "TEA-DX-001.6": 42
    "TEA-DX-001.7": 22
    "TEA-DX-001.8": 18
    "EPIC-cross-bundle": 12
  by_level:
    unit: 64
    integration: 83
    e2e: 8
  by_priority:
    p0: 71
    p1: 54
    p2: 25
    p3: 5
  coverage_gaps:
    - "Story 1.5 has no test design — pre-condition for epic Done (per NFR-AC-6)."
    - "INT-005 / INT-006 require redact_state() primitives that do not exist; pending Story 1.3 AC-4 rewrite (per NFR Security FAIL)."
  preconditions:
    - "Story 1.5 test-design committed."
    - "Story 1.3 AC-4 rewritten to one of the three resolution paths in TEA-DX-001.3-nfr-20260501.md."
    - "TEA-BUILTIN-012.3 redaction primitives shipped OR Story 1.3 AC-4 selects Option B (no-redaction + banner)."
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001-test-design-20260501.md
P0 tests identified (epic-level only): 6 INT + 1 E2E = 7
P0 tests identified (bundle-wide, including child stories): 71
Per-story test designs:
  - TEA-DX-001.1-test-design-20260501.md
  - TEA-DX-001.2-test-design-20260501.md
  - TEA-DX-001.3-test-design-20260501.md
  - TEA-DX-001.4-test-design-20260501.md
  - TEA-DX-001.5-test-design-{date}.md  [MISSING]
  - TEA-DX-001.6-test-design-20260501.md
  - TEA-DX-001.7-test-design-20260501.md
  - TEA-DX-001.8-test-design-20260501.md
```

---

## Quality Checklist

- [x] Every Epic Success Criterion / DoD item maps to at least one epic-level scenario or per-story design (see AC coverage table above).
- [x] Epic-level scenarios are exclusively integration/E2E — no unit-level cross-story tests.
- [x] No duplicate coverage with per-story designs — epic scenarios cover the bundle seams (examples regression, help snapshots, validator/engine parity, redaction reuse, originating-UX E2E) that no single story owns.
- [x] Priorities align with risk profile — every High risk has a P0 epic-level scenario (SEC-001 → INT-005/006/E2E-002; TECH-001 1.6 → INT-004; TECH-002 1.1 → per-story + INT-001 negative assertion).
- [x] Test IDs follow naming convention `TEA-DX-001-{LEVEL}-{SEQ}` (epic-level) and `TEA-DX-001.{N}-{LEVEL}-{SEQ}` (per-story).
- [x] Scenarios are atomic and independent — every fixture is `tmp_path`-scoped or read-only; no shared mutable state across scenarios.
- [x] Coverage gaps explicitly documented — Story 1.5 + Story 1.3 AC-4 prerequisites are called out as blocking pre-conditions, not silent omissions.
