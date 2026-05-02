# NFR Assessment: TEA-DX-001.4

Date: 2026-05-01
Reviewer: Quinn (Test Architect)
Mode: YOLO (non-interactive — core four NFRs assessed by default)
Story: [`docs/stories/TEA-DX-001.4-variables-in-run-exec-globals.md`](../../stories/TEA-DX-001.4-variables-in-run-exec-globals.md)
Related: [`TEA-DX-001.4-risk-20260501.md`](TEA-DX-001.4-risk-20260501.md)

## Scope

Single-line additive change at `python/src/the_edge_agent/yaml_nodes.py:715` to expose `engine.variables` as a name in the `exec_globals` dict for inline Python `run:` blocks. Mirrors the existing Jinja-context pattern at `yaml_engine.py:533`.

## Summary

- **Security: PASS** — No new attack surface; `run:` already executes arbitrary Python under the existing trust boundary.
- **Performance: PASS** — One dict-key insertion per node invocation; negligible cost.
- **Reliability: CONCERNS** — Mutable, by-reference engine state shared across parallel flows is a documented race; cross-process parallel strategy makes mutations invisible. Mitigation is documentation + a regression test, both already enumerated in the risk profile but not yet committed.
- **Maintainability: PASS** — One-line change, mirrors an existing pattern, ACs require unit tests + docs.

**Quality Score: 90 / 100** (1 CONCERNS × −10).

## Gate YAML Block

```yaml
nfr_validation:
  _assessed: [security, performance, reliability, maintainability]
  security:
    status: PASS
    notes: 'No new attack surface — run: already executes arbitrary Python; trust boundary is the YAML source. SEC-001 (risk profile) confirmed minimal.'
  performance:
    status: PASS
    notes: 'O(1) dict insertion per node invocation; no I/O or copy.'
  reliability:
    status: CONCERNS
    notes: 'engine.variables is shared by reference across parallel flows (race on writes). Cross-process parallel strategy: mutations are invisible. Needs explicit doc warning + regression test (TECH-006 in risk profile, P1 test in priorities).'
  maintainability:
    status: PASS
    notes: 'Single-line change mirroring yaml_engine.py:533. ACs 8-10 require read/write tests, kwarg-override test, and docs update in actions-reference.md. DoD gates on green pytest + examples regression.'
```

## Detailed Findings

### Security — PASS

Inline Python `run:` blocks already call `exec()` on user-authored code (`yaml_nodes.py:760, 765`). Anything callable from `run:` could already access `engine` via reflection if a malicious author wished. Adding `variables` as a named global does not lower the existing trust boundary, which is and remains the YAML source. Confirmed by SEC-001 in the risk profile (score 1, minimal).

**Validated:**
- No new I/O surface.
- No new credential or secret handling path.
- Jinja sandbox is unaffected (this is the `exec()` path, not the template path).

**Caveats:**
- Authors *can* now write `variables["api_key"] = "..."` from a `run:` block and have it persist for subsequent Jinja templates. This is an intended capability (AC-3) rather than a security regression — it matches the existing `extraction_prompt` precedent at `yaml_engine.py:1263`.

### Performance — PASS

The change adds one entry to the `exec_globals` dict constructed once per `run:` invocation. There is no copy of `engine.variables` (assignment is by reference) and no I/O. Hot-path impact is unmeasurable.

**Validated:**
- No additional template processing.
- No allocation beyond a single dict slot.
- No additional imports.

### Reliability — CONCERNS

This is the only non-PASS dimension. Three concrete concerns:

1. **Parallel-flow race (TECH-006).** `StateGraph` parallel flows get deep copies of `state` (per `CLAUDE.md`), but `engine.variables` is shared by reference. Two branches that each do `variables["counter"] += 1` race; the post-fan-in value is non-deterministic. AC-3 documents *that* writes propagate; it does not warn that they race in parallel.
2. **Cross-process invisibility.** `settings.parallel.strategy` supports `"process"` and `"remote"` (`yaml_engine.py:1325`). In those modes, `run:` blocks execute in a worker that has its own copy of `engine.variables`; in-block writes are silently lost on fan-in. Authors who write `variables["x"] = …` in a process-mode parallel branch will see that mutation discarded.
3. **Read-after-write ordering.** Within a single sequential graph, AC-9 ensures writes are visible in subsequent Jinja. There is no concern here — flagging only to confirm the "thread" strategy and sequential paths are fine.

**Recommended remediations** (additive to current ACs):

- Add an explicit "⚠ parallel safety" callout to AC-10's documentation update.
- Add a regression test (already P1 in the risk profile) that two parallel branches each write `variables["x"]` and assert the fan-in value is *one of* the writes — this serves as executable documentation of the race.
- Add a second test asserting that under `settings.parallel.strategy: process`, in-branch writes to `variables` are *not* visible at fan-in (or change ACs to forbid this and document it as such).

### Maintainability — PASS

The change is a single line. The pattern it mirrors (`yaml_engine.py:533`) is already established and tested. Acceptance criteria require:

- Unit tests for read (AC-8), write-visibility (AC-9), kwarg-override (AC-5).
- Documentation in `docs/python/actions-reference.md` (AC-10).
- Existing examples regression-clean (AC-6).
- DoD gates on `pytest python/tests/` green.

Test coverage commitment is explicit and proportionate. Code locality is excellent (one file, one function, one line).

## Critical Issues

1. **Parallel-flow write race not in ACs** (Reliability)
   - **Risk:** Authors expect `variables` writes to behave like `state` writes (per-flow), but they propagate by reference and race across threads. In process/remote mode, writes silently disappear.
   - **Fix:** Add to AC-10 a documentation requirement: "Document that `variables` is a shared mutable reference; parallel-flow writes race; cross-process parallel strategies do not propagate writes; use `state` for per-flow data." Add the two regression tests above.

## Quick Wins

- Documentation callout in actions reference: ~30 minutes.
- Parallel race regression test (thread mode): ~1 hour.
- Process-mode invisibility regression test: ~1 hour.
- Total to upgrade Reliability → PASS: ~2.5 hours.

## Test Recommendations Summary

**P0 (must add to satisfy ACs as written)**
- Read test (AC-8).
- Write-visibility test across nodes (AC-9).
- Kwarg-override test (AC-5).
- Lua/Prolog non-injection test (AC-7).

**P1 (close the Reliability CONCERNS)**
- Parallel-thread race observability test (TECH-006).
- Parallel-process invisibility test (new — surfaced by this assessment).

**P2 (regression)**
- Full `pytest python/tests/` suite green.
- Smoke-run `examples/yaml/*.yaml` (AC-6).

## Acceptance-Criteria Health

| AC | Coverage | Notes |
|----|----------|-------|
| AC-1 | Direct | Read test (AC-8) covers existence in scope. |
| AC-2 | Direct | AC-8 covers both `[]` and `.get()`. |
| AC-3 | Direct | AC-9 covers write-through. |
| AC-4 | Implicit | AC-6 (examples regression) covers Jinja non-regression. Recommend an explicit unit test asserting `{{ variables.x }}` still resolves. |
| AC-5 | Direct | Kwarg-override test in Task 2. |
| AC-6 | Direct | DoD requirement. |
| AC-7 | Direct | Recommend P3 unit test confirming Lua/Prolog runtimes do not see `variables`. |
| AC-8 | Direct | Specified in Task 2. |
| AC-9 | Direct | Specified in Task 2. |
| AC-10 | Direct | Recommend extending to include the parallel-safety warning surfaced above. |

**Suggested AC additions** (non-blocking, advisory):

- **AC-11 (proposed):** Documentation in `actions-reference.md` includes a "Parallel safety" warning describing the shared-reference semantics and the cross-process invisibility.
- **AC-12 (proposed):** Regression test demonstrating that parallel-thread writes to `variables` produce a non-deterministic but bounded fan-in value (one of the writes).

## Story Update Line

```
NFR assessment: docs/qa/assessments/TEA-DX-001.4-nfr-20260501.md
```

## Gate Integration Line

```
Gate NFR block ready → paste into docs/qa/gates/TEA-DX-001.4-variables-in-run-exec-globals.yml under nfr_validation
```
