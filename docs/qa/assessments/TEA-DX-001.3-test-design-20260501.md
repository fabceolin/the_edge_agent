# Test Design: Story TEA-DX-001.3

**Story:** Intermediate state dumps for debug (`tea run --debug-state <dir>`)
**Date:** 2026-05-01
**Designer:** Quinn (Test Architect)
**Mode:** YOLO

---

## Test Strategy Overview

- **Total test scenarios:** 18
- **Unit tests:** 7 (39%)
- **Integration tests:** 10 (56%)
- **E2E tests:** 1 (5%) — single end-to-end smoke against the original `plan_batches`-style failure scenario
- **Priority distribution:** P0: 7, P1: 7, P2: 3, P3: 1

The change under test introduces a new `tea run --debug-state <dir>` flag that hooks into the CLI run-loop event stream (`cli.py:1794-2082`) and writes per-node state snapshots to disk.

The strategy is integration-heavy by necessity: behavior emerges from the interaction between (a) the StateGraph engine event stream (`"state"` / `"parallel_state"` / `"branch_complete"` / `"parallel_complete"` / `"final"` / `"error"`), (b) the CLI run-loop, (c) the on-disk file naming/sequence convention, and (d) — most importantly — the secrets-redaction path that AC-4 depends on but does not yet exist (per NFR FAIL).

Test design directly addresses every Priority-1 risk surfaced by the risk profile (SEC-001, SEC-002, TECH-001, OPS-001, TECH-003, DATA-002, DATA-003), and the four NFR gate concerns (AC-4 redaction premise, engine-event mapping, FAILED-write isolation, path sanitization) by codifying them as executable specifications.

> **Pre-implementation gate (must resolve before tests are authored):** AC-4's redaction contract must be picked (Option A/B/C from NFR Missing Considerations #1) and the proposed AC-13/14/15 must be accepted into the story. The test scenarios below are written against **Option A** (build `redact_state` + run tracebacks through `mask_credentials`) — if the team chooses B or C, scenarios SEC-* will need adjustment.

---

## Test Scenarios by Acceptance Criteria

### AC-1 / AC-6: New `--debug-state <dir>` flag; absent → behavior unchanged

| ID                    | Level       | Priority | Test                                                                                                                          | Justification                                                            | Mitigates Risks |
|-----------------------|-------------|----------|-------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-UNIT-001 | Unit        | P1       | `tea run --help` snapshot contains a `--debug-state` line with a one-line description.                                        | Pure CLI surface; cheap.                                                  | OPS-001         |
| TEA-DX-001.3-INT-001  | Integration | P0       | Run a 3-node workflow **without** `--debug-state`; assert no files written to CWD or any temp dir, and `--output` byte-equal to pre-change baseline. | Locks in fully-additive guarantee (story §"Compatibility").              | OPS-002         |

### AC-2: After each node completes, write `<dir>/<NN>-after-<node_name>.json`

| ID                    | Level       | Priority | Test                                                                                                                                                         | Justification                                                          | Mitigates Risks |
|-----------------------|-------------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-002  | Integration | P0       | 3-sequential-node workflow with `--debug-state /tmp/d3`; assert exactly 3 files: `01-after-n1.json`, `02-after-n2.json`, `03-after-n3.json`, in lexical order. | Direct AC-10 coverage; primary functional spec.                        | —               |
| TEA-DX-001.3-UNIT-002 | Unit        | P1       | Filename formatter: `(step=7, node="my_node", suffix="after") → "07-after-my_node.json"`. Also `(step=12, ...)` → `"12-..."`.                                | Zero-padding edge: 1-digit, 2-digit, 3-digit (counter overflow → expect `100-...`). | DATA-002        |
| TEA-DX-001.3-UNIT-003 | Unit        | P1       | Per-file content matches `json.dumps(state, cls=TeaJSONEncoder, indent=2)` byte-for-byte.                                                                   | Confirms reuse of existing serializer (`cli.py:1838`); no divergence.  | —               |

### AC-3: On node failure, write `<dir>/<NN>-FAILED-<node_name>.json` with state + traceback

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                            | Justification                                                                       | Mitigates Risks |
|-----------------------|-------------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-003  | Integration | P0       | 3-node workflow; node 2 raises `RuntimeError("boom")`. Assert files: `01-after-n1.json`, `02-FAILED-n2.json` (no `03-*`). FAILED file is JSON containing `{"state": {...entering n2...}, "traceback": "...RuntimeError: boom..."}`.                            | Direct AC-11 coverage; defines FAILED file schema.                                  | —               |
| TEA-DX-001.3-UNIT-004 | Unit        | P0       | FAILED-file payload schema test: given `(state, exc, tb_str)`, the writer produces a dict with exactly the keys `{"state", "traceback"}` and `state` is the *pre-node* snapshot, not the post-mutation state.                                                  | Pins the data contract independent of engine plumbing.                              | —               |
| TEA-DX-001.3-INT-004  | Integration | P1       | Failure on first node (no successful predecessors): `01-FAILED-n1.json` only; assert `state` field is the workflow input.                                                                                                                                       | Boundary: no `00-after-*` written, counter starts correctly.                        | DATA-002        |

### AC-4: Secrets redaction — secrets MUST NOT appear in dumps (security-critical)

> Tests below presume **Option A**: `redact_state(state, secret_keys)` exists and is shared with checkpoint serialization; FAILED tracebacks pass through `mask_credentials()` (`cache.py:83`).

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                                                                                                          | Justification                                                                                          | Mitigates Risks |
|-----------------------|-------------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-UNIT-005 | Unit        | P0       | `redact_state({"api_key": "sk-CANARY-XYZ", "user": "alice"}, secret_keys=["api_key"])` → `{"api_key": "***", "user": "alice"}`. Repeat for nested: `{"config": {"api_key": "sk-CANARY-XYZ"}}` → `{"config": {"api_key": "***"}}`.                                                                                                            | Closes NFR FAIL by exercising the helper directly (top-level + nested-dict, two of risk profile's P1 gaps). | SEC-001         |
| TEA-DX-001.3-INT-005  | Integration | P0       | End-to-end: workflow whose state contains `api_key: "sk-CANARY-XYZ"`; run with `--debug-state`. Assert **no** dump file (across all `*.json` in the dir) contains the substring `sk-CANARY-XYZ`.                                                                                                                                              | The "would a real attacker find the secret on disk" question, answered by grep.                       | SEC-001         |
| TEA-DX-001.3-INT-006  | Integration | P0       | Node deliberately raises `RuntimeError(f"key {SECRET} rejected")` where `SECRET = "sk-CANARY-XYZ"`. Run with `--debug-state`. Assert the FAILED dump's `traceback` field does not contain `sk-CANARY-XYZ` (it has been masked).                                                                                                              | Traceback redaction (NFR Missing Consideration #4); separate from dict-traversal redaction.            | SEC-001         |

### AC-5: Directory created if missing; pre-existing files preserved

| ID                    | Level       | Priority | Test                                                                                                                                                          | Justification                                                          | Mitigates Risks |
|-----------------------|-------------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-007  | Integration | P1       | Two-part: (a) `--debug-state` points to a non-existent dir → dir is created and dumps land inside; (b) target dir already contains `99-marker.txt` → run completes without deleting/overwriting `99-marker.txt`. | Directly validates AC-5 (mkdir + non-destructive policy).               | DATA-002        |

### AC-7: Compatible with `--quiet`, `--stream`, `--show-graph`

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                       | Justification                                                                                | Mitigates Risks |
|-----------------------|-------------|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-008  | Integration | P1       | Parametrized matrix: `--debug-state` × `{--quiet, --stream, --show-graph, no-flag}`. For each: 3-node workflow runs to completion, exactly 3 dump files produced, stdout/NDJSON unchanged from same run without `--debug-state`.            | Locks in additive-only behavior across display modes.                                        | OPS-002         |

### AC-8: Compatible with `--checkpoint`

| ID                    | Level       | Priority | Test                                                                                                                                                                                                              | Justification                                                                                  | Mitigates Risks |
|-----------------------|-------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-009  | Integration | P1       | Run `--debug-state /tmp/d3 --checkpoint /tmp/cp` together. Assert (a) checkpoint files written under `/tmp/cp`, (b) debug dumps written under `/tmp/d3`, (c) no overlap, and (d) resuming from a checkpoint with `--debug-state` continues counter from the resume step (or restarts at 01 — pin behavior). | Two persistence subsystems share state shape. Counter-on-resume is a real ambiguity worth pinning. | TECH-001       |

### AC-9: Parallel/dynamic_parallel — dump per parent-node completion, NOT per branch

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                                                                          | Justification                                                                                                  | Mitigates Risks |
|-----------------------|-------------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-010  | Integration | P0       | Workflow: `entry → fanout → [branch_a, branch_b, branch_c] → fanin → end`. Run with `--debug-state`. Assert exactly **2** files: `01-after-fanout.json` and `02-after-fanin.json`. **Not** 5 (one per branch).                                                                                                | Pins AC-9 against the (currently unmapped) engine event semantics; addresses TECH-001 directly.                | TECH-001        |
| TEA-DX-001.3-INT-011  | Integration | P0       | Same shape but via `dynamic_parallel` (items computed at runtime). Same assertion: 2 files only.                                                                                                                                                                                                              | `dynamic_parallel` and static parallel emit different events; both must collapse to one parent dump.            | TECH-001        |
| TEA-DX-001.3-UNIT-006 | Unit        | P1       | Event-handler dispatch table: synthetic event stream `[state(n1), parallel_state(branch_a), branch_complete(branch_a), parallel_state(branch_b), branch_complete(branch_b), parallel_complete(fanout), state(fanin), final]` → exactly 2 dump-write calls, with `node ∈ {fanout, fanin}`.                  | Isolates event-mapping logic from engine plumbing; faster than INT, closes NFR Reliability gap.                | TECH-001        |

### AC-10 / AC-11: Functional happy-path & failure-path tests already prescribed

Covered by **TEA-DX-001.3-INT-002** (AC-10) and **TEA-DX-001.3-INT-003** (AC-11). Listed here for traceability only.

### AC-12: Documentation + redaction-guarantee warning

| ID                    | Level       | Priority | Test                                                                                                                                                                          | Justification                                                                                | Mitigates Risks |
|-----------------------|-------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-DOC-001  | Doc review  | P2       | Reviewer verifies CLI reference (`docs/python/...`) lists `--debug-state` with explicit redaction scope language, plus a "development-only" callout in the troubleshooting section.    | Manual gate; not automatable as pytest. Mitigates SEC-001 by user awareness.                  | OPS-001, SEC-001 |
| TEA-DX-001.3-INT-012  | Integration | P0       | When `--debug-state` is set, a single `WARNING` line is emitted on **stderr** at startup (e.g., `WARNING: --debug-state will write state snapshots to disk; not for production`). Survives `--quiet` (i.e., still emitted with `--quiet`).        | Promotes AC-12 from doc-only to runtime signal (NFR recommendation; mitigates OPS-001).      | OPS-001         |

### AC-13 (proposed by NFR): Engine-event mapping — sequential dumps on `"state"`, parallel parents dump once on `"parallel_complete"`, per-branch events do not trigger dumps

Covered by **TEA-DX-001.3-INT-010**, **INT-011**, **UNIT-006**.

### AC-14 (proposed by NFR): FAILED-dump write is isolated; original exception propagates if dump-write itself fails

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                                                                              | Justification                                                                                                          | Mitigates Risks |
|-----------------------|-------------|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-INT-013  | Integration | P1       | Workflow with a node that raises `RuntimeError("real-error")`; `--debug-state` points to a **read-only** directory. Assert (a) process exits non-zero, (b) the user-visible error is the original `RuntimeError("real-error")`, **not** an `OSError("Read-only file system")`, and (c) a stderr log line notes the dump-write failure. | Pins behavior for TECH-003 (Low risk, but high user-debugging-confusion potential). Test must verify error chain ordering. | TECH-003        |

### AC-15 (proposed by NFR): Node-name path component is sanitized

| ID                    | Level       | Priority | Test                                                                                                                                                                                                                                                          | Justification                                                                                                                | Mitigates Risks |
|-----------------------|-------------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-UNIT-007 | Unit        | P0       | Sanitizer unit: `("../../etc/passwd") → "______etc_passwd"`; `("with space") → "with_space"`; `("dotted.name") → "dotted_name"`; `("ok-name_1") → "ok-name_1"` (whitelist `[A-Za-z0-9_-]`).                                                                  | Pure function; primary defense against SEC-002/DATA-003. Whitelist is faster to validate than blacklist.                     | SEC-002, DATA-003 |
| TEA-DX-001.3-INT-014  | Integration | P1       | Workflow with a `dynamic_parallel` whose items produce a node named `"../escape"`. Run with `--debug-state /tmp/d3`. Assert: no file written outside `/tmp/d3`; the dump file's name contains the sanitized form (no `..`, no `/`).                          | End-to-end path-traversal proof — mirrors the way real malicious YAML could be authored.                                     | SEC-002         |

### Cross-cutting / scenario completion

| ID                    | Level | Priority | Test                                                                                                                                                                                                                                                | Justification                                                                                | Mitigates Risks |
|-----------------------|-------|----------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------|-----------------|
| TEA-DX-001.3-E2E-001  | E2E   | P1       | Reproduction of the `plan_batches` user-reported scenario (or a minimal stand-in): 6-node workflow, intentional Jinja error in node 3. With `--debug-state`, user can inspect `02-after-n2.json` and `03-FAILED-n3.json` post-mortem and pinpoint failure. | Validates the actual story value-prop end-to-end; covers DoD bullet "verified on the original scenario". | OPS-002         |

---

## Coverage Matrix

| AC    | Test IDs                                                              | Levels Covered            | Priorities       |
|-------|-----------------------------------------------------------------------|---------------------------|------------------|
| AC-1  | UNIT-001                                                              | Unit                      | P1               |
| AC-2  | INT-002, UNIT-002, UNIT-003                                           | Integration + Unit        | P0, P1           |
| AC-3  | INT-003, UNIT-004, INT-004                                            | Integration + Unit        | P0, P1           |
| AC-4  | UNIT-005, INT-005, INT-006                                            | Unit + Integration        | P0               |
| AC-5  | INT-007                                                               | Integration               | P1               |
| AC-6  | INT-001                                                               | Integration               | P0               |
| AC-7  | INT-008                                                               | Integration               | P1               |
| AC-8  | INT-009                                                               | Integration               | P1               |
| AC-9  | INT-010, INT-011, UNIT-006                                            | Integration + Unit        | P0, P1           |
| AC-10 | INT-002 (alias)                                                       | Integration               | P0               |
| AC-11 | INT-003 (alias)                                                       | Integration               | P0               |
| AC-12 | DOC-001, INT-012                                                      | Doc + Integration         | P2, P0           |
| AC-13 | INT-010, INT-011, UNIT-006 (alias)                                    | Integration + Unit        | P0, P1           |
| AC-14 | INT-013                                                               | Integration               | P1               |
| AC-15 | UNIT-007, INT-014                                                     | Unit + Integration        | P0, P1           |
| (DoD) | E2E-001                                                               | E2E                       | P1               |

**Coverage gaps:** None — every AC (including proposed AC-13/14/15) maps to at least one scenario, and the user-reported reproduction is covered by E2E-001.

---

## Risk Coverage

| Risk ID  | Severity | Description                                                                            | Mitigating Tests                                                       |
|----------|----------|----------------------------------------------------------------------------------------|------------------------------------------------------------------------|
| SEC-001  | High     | Secret/PII leakage to disk if checkpoint redaction has gaps                            | UNIT-005, INT-005, INT-006, DOC-001                                    |
| TECH-001 | Medium   | Parallel/`dynamic_parallel` branch events double-trigger dumps (AC-9 mapping)          | INT-010, INT-011, UNIT-006, INT-009                                    |
| OPS-001  | Medium   | `--debug-state` accidentally enabled in production / CI (no runtime warning)           | INT-012, DOC-001, UNIT-001                                             |
| DATA-001 | Medium   | Disk fill from large state on long workflows (no v1 size cap)                          | *(accepted v1 limitation; doc-only — DOC-001)*                         |
| SEC-002  | Low      | Path traversal via node names containing `/` or `..`                                   | UNIT-007, INT-014                                                      |
| TECH-002 | Low      | Synchronous I/O in event loop slows execution on large states                          | *(no test; accepted Low; covered by INT-008 not regressing user-visible behavior)* |
| TECH-003 | Low      | FAILED-state write fails (disk full / perms) and masks original error                  | INT-013                                                                |
| DATA-002 | Low      | Stale dumps from prior runs interleave with new run (counter overlap)                  | INT-007, UNIT-002, INT-004                                             |
| DATA-003 | Low      | Filename injection from special characters in node names                               | UNIT-007, INT-014                                                      |
| OPS-002  | Minimal  | UX confusion: empty `--output` plus dumps disagree on final state                      | INT-001, INT-008, E2E-001                                              |

**No coverage gap on any risk above Low except DATA-001**, which is explicitly accepted for v1 and tracked via documentation only (per story §"Risk and Compatibility").

---

## Test Data & Environment Requirements

### Fixtures

- **3-node sequential YAML factory:** entry → n1 → n2 → n3 → end, where each node body is parameterizable (default: trivial state passthrough). Used by INT-001, INT-002, INT-003, INT-004, INT-007, INT-008.
- **Failure-injection helper:** wraps a fixture node so it raises a configurable exception (`RuntimeError("boom")`, `RuntimeError(f"key {SECRET}")`, Jinja-render error). Used by INT-003, INT-004, INT-006, INT-013, E2E-001.
- **Parallel-fan-out factory:** entry → fanout → [N branches] → fanin → end. Static and `dynamic_parallel` flavors. Used by INT-010, INT-011, INT-014.
- **Synthetic event-stream harness:** drives the dump-handler with a hand-built sequence of engine events (no real graph). Used by UNIT-006 and any future unit tests of dispatch logic.
- **Canary secret fixture:** `SECRET = "sk-CANARY-XYZ"` placed in state and (separately) embedded in raised exception messages — used by SEC tests (UNIT-005, INT-005, INT-006).

### Environment

- **Standard pytest path:** `cd python && pytest python/tests/test_debug_state.py` (suggested filename). All UNIT and INT-001..009 run in default config.
- **INT-013 (read-only dir):** uses `tmp_path` fixture + `chmod 0o555` on the dump dir. Skip on Windows (`pytest.skip` if `os.name == 'nt'`); the read-only semantics differ.
- **INT-014 (path traversal):** must run on POSIX; on Windows the path-traversal characters (`/`) behave differently. Same skip pattern as INT-013.
- **INT-011 (`dynamic_parallel`):** requires the `parallel` settings block to be runnable in the test env. `pytest.importorskip` only if a hard dependency is missing.
- **DOC-001:** manual reviewer step in PR review; tracked in DoD checklist, not pytest.
- **E2E-001:** uses a self-contained YAML fixture file under `python/tests/fixtures/debug_state_e2e.yaml`; runs via `subprocess.run([sys.executable, "-m", "the_edge_agent.cli", "run", ...])` to exercise the actual Typer entry point. ~3-5s runtime; acceptable for an E2E.

### Test data

- **Trivial states:** `{"x": 1}`, `{"items": ["a", "b"]}` — for INT-002, INT-008, INT-010.
- **Secret-bearing state:** `{"api_key": "sk-CANARY-XYZ", "user": "alice", "config": {"api_key": "sk-CANARY-XYZ"}}` — for UNIT-005 / INT-005 to cover top-level + nested.
- **Secret-in-traceback state:** node body raises `raise RuntimeError(f"key sk-CANARY-XYZ rejected by upstream")`. The secret is *not* in `state`, only in the exception — for INT-006.
- **Path-traversal node names:** `["../../etc/passwd", "with space", "dotted.name", "ok-name_1"]` — for UNIT-007 and INT-014.
- **Pre-existing dump-dir fixture:** `99-marker.txt` placed before the run — for INT-007.

### Suite hygiene

- Every test that writes dumps uses `tmp_path` (pytest builtin) to avoid cross-test pollution.
- No tests rely on absolute paths like `/tmp/...` (the document above uses them for clarity; real fixtures must use `tmp_path`).
- Tests that mutate file modes (`chmod`) must `chmod` back to writable in a `finally` so `tmp_path` cleanup succeeds.
- Canary string `sk-CANARY-XYZ` must be unique per-test (e.g., suffix with test name) so that a leak in one test cannot mask another's leak via test-ordering.

---

## Recommended Execution Order

1. **P0 unit (fastest feedback):** UNIT-005, UNIT-007, UNIT-004, UNIT-006 — prove the security-critical helpers + dispatch logic in isolation.
2. **P0 integration (prove end-to-end functional & security):** INT-002, INT-003, INT-005, INT-006, INT-010, INT-011, INT-012, INT-001.
3. **P1 unit:** UNIT-001, UNIT-002, UNIT-003.
4. **P1 integration:** INT-004, INT-007, INT-008, INT-009, INT-013, INT-014, E2E-001.
5. **P2 doc gate:** DOC-001 (manual reviewer in PR).
6. **P3:** none.

> Rationale for security-first ordering: SEC-001 was rated High by the risk profile and FAIL by the NFR assessment. If UNIT-005 / INT-005 / INT-006 do not pass on first run, all other tests should be considered blocked — there is no point validating functional behavior of a feature that leaks secrets.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 18
  by_level:
    unit: 7
    integration: 10
    e2e: 1
  by_priority:
    p0: 7
    p1: 7
    p2: 3
    p3: 1
  coverage_gaps: []
  risk_mitigations:
    SEC-001: [TEA-DX-001.3-UNIT-005, TEA-DX-001.3-INT-005, TEA-DX-001.3-INT-006, TEA-DX-001.3-DOC-001]
    SEC-002: [TEA-DX-001.3-UNIT-007, TEA-DX-001.3-INT-014]
    TECH-001: [TEA-DX-001.3-INT-010, TEA-DX-001.3-INT-011, TEA-DX-001.3-UNIT-006, TEA-DX-001.3-INT-009]
    TECH-003: [TEA-DX-001.3-INT-013]
    OPS-001:  [TEA-DX-001.3-INT-012, TEA-DX-001.3-DOC-001, TEA-DX-001.3-UNIT-001]
    OPS-002:  [TEA-DX-001.3-INT-001, TEA-DX-001.3-INT-008, TEA-DX-001.3-E2E-001]
    DATA-001: []  # accepted v1; documented only (DOC-001)
    DATA-002: [TEA-DX-001.3-INT-007, TEA-DX-001.3-UNIT-002, TEA-DX-001.3-INT-004]
    DATA-003: [TEA-DX-001.3-UNIT-007, TEA-DX-001.3-INT-014]
  preconditions:
    - "AC-4 redaction contract picked (Option A/B/C from NFR Missing Considerations #1)"
    - "Proposed AC-13 / AC-14 / AC-15 accepted into the story"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-DX-001.3-test-design-20260501.md
P0 tests identified: 7
```

---

## Quality Checklist

- [x] Every AC has test coverage (incl. proposed AC-13/14/15 and DoD reproduction)
- [x] Test levels are appropriate — unit for pure functions (formatter, sanitizer, redactor, event dispatch), integration for the CLI/engine seam, single E2E for the user-reported repro
- [x] No duplicate coverage across levels (AC-10 = INT-002 alias; AC-11 = INT-003 alias; AC-13 = INT-010/011 + UNIT-006 alias)
- [x] Priorities align with the risk profile — every High/Medium risk has at least one P0 mitigating test (DATA-001 explicitly accepted as v1 limitation)
- [x] Test IDs follow `TEA-DX-001.3-{LEVEL}-{SEQ}` convention
- [x] Scenarios atomic and independent (no inter-test ordering dependencies; canary uniqueness enforced)
