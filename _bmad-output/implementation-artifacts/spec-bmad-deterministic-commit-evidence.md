---
title: 'Prove BMAD story commits from Git'
type: 'bugfix'
created: '2026-09-05'
status: 'done'
review_loop_iteration: 0
baseline_commit: 'ece7dc156a6ff078c7d2e143d5ef51ab99caa4c9'
context: []
---

<frozen-after-approval reason="user supplied implementation requirements">

## Intent

**Problem:** Epic Waves classified a clean, committed story as incomplete because finish output lacked an exact committed=true marker. This blocked dependencies despite max_review_cycles=0 and review_handoff=manual.

**Approach:** Use read-only Git evidence to prove implementation commits relative to a frozen, explicit base. Treat finish markers as diagnostics and secondary vetoes, never sole proof. Preserve the existing integration authority and review routing.

## Boundaries & Constraints

**Always:** Fail closed on missing HEAD/base, Git command errors, dirty trees (including untracked files), no commits beyond base, metadata-only or empty changes, and explicit conflicting finish claims. An absent/malformed marker is diagnostic rather than a veto when Git proves implementation. Store full canonical SHAs, evidence/reason, cleanliness and marker divergence in story receipts and returned state. Git integration and ancestry/merge decisions remain in bmad_epic_waves_integrate.py. Summary proves local contribution, not merge eligibility. Preserve skipped-review and approved-review classification.

**Ask First:** Expanding scope beyond this classification fix.

**Never:** Touch licityeasy, push, merge the working repository into main, destructively reset, stash, or remove existing worktrees. Tests may exercise existing integration only in disposable temporary repositories. Do not invoke LLM review/fix during tests. Do not change review/fix graph edges.

## I/O & Edge-Case Matrix

| Scenario | Input / State | Expected Output / Behavior | Error Handling |
|----------|--------------|---------------------------|----------------|
| Confirmed manual work | Clean implementation commit beyond base, true marker, SKIPPED/manual | manual_review | Record Git evidence |
| Missing or malformed marker | Same Git state, marker absent or malformed (including structured outputs) | manual_review | Record divergence |
| No contribution | HEAD equals base, even with true marker | incomplete | Record no commits |
| Dirty worktree | Tracked, staged, or untracked changes | incomplete | Record dirty evidence |
| Approved contribution | Valid Git commit and APPROVED | completed | Record proof |
| Explicit refusal | Valid Git contribution and committed=false or contradictory markers | incomplete | Record conflict |
| Metadata or empty commit | Only story and/or sprint-status changes, or empty commit | incomplete | Record no implementation changes |
| Missing Git evidence | Unknown/invalid base, missing HEAD or failed command | incomplete | Record reason |

</frozen-after-approval>

## Code Map

- `examples/workflows/bmad-story-cycle.yaml` -- init_policy precedes agents; summary currently parses marker exclusively, classifies, and writes story receipt. repo_path is worktree root. story_path may be absolute or relative; sprint_status_path defaults to `_bmad-output/implementation-artifacts/sprint-status.yaml`. Add schema fields for evidence. For sequential non-worktree mode, capture starting HEAD before agents if no explicit base; summary must still require Git proof.
- `examples/workflows/bmad-epic-waves.yaml` -- build_waves prepares non-in-flight worktrees but does not currently record their base. Freeze target SHA while preparing these and pass explicit base_sha in dot_payloads. In-flight prepare runs immediately before story and records base in receipts_dir; do not replace this with a current-target lookup at summary. Retry reuses payloads after resetting to the target: refresh frozen base/prepare evidence there, checking reset success, so inherited sibling commits cannot count as retry implementation.
- `python/src/the_edge_agent/bmad_epic_waves_integrate.py` -- read_receipt and write_receipt helpers; prepare receipt contains key/stage/path/branch/base/status; integrate_story owns ancestry and merges. Keep this module unchanged.
- `python/src/the_edge_agent/bmad_story_commit_evidence.py` -- proposed read-only helper for evidence, to keep YAML summary concise. Validate receipt identity/path and explicit base disagreement. Count base..HEAD commits and net changed files from the unique merge-base of the frozen base and HEAD, excluding story and sprint metadata; record both the original base_sha and diff_base_sha. This handles resumed branches where prepare base is the advanced target, and prevents sibling-only changes from counting as story implementation; avoid adding merge eligibility rules. Use argument-list subprocesses and checked return codes. Fail closed on exceptions. Missing configured metadata paths must not let conventional story/sprint metadata count as implementation. Avoid choosing arbitrary target refs as fallback bases at summary.
- `python/tests/test_bmad_epic_waves_safety_nets.py` -- run_node extracts real YAML Python; existing summary tests use text-only fake states and need real temporary Git evidence.
- `python/tests/test_bmad_epic_waves_integrate.py`, `python/tests/test_bmad_epic_waves_workflow.py`, `python/tests/test_cli_fail_on_state.py python/tests/test_bmad_story_commit_integration.py` -- integration and regression suites; CLI app has existing --fail-on-state tests.

## Tasks & Acceptance

**Execution:**
- [x] Implement Git evidence helper and wire summary/receipts, preserving classification and review graph.
- [x] Provide frozen base for non-in-flight and sequential paths without overriding prepare evidence.
- [x] Add real-Git tests covering every matrix row, receipt fields, CLI exit status, and generated Epic Waves commands; adapt existing text-only summary tests.
- [x] Execute specific tests and Epic Waves/CLI regressions; resolve failures and report exact commands.

**Acceptance Criteria:**
- Given skipped review and valid Git proof, when summary runs, then final status is manual_review and never completed; no review/fix runs.
- Given failed proof, when the existing --fail-on-state CLI processes summary, then exit code is 1; valid manual_review exits 0.
- Given a valid story receipt, when existing integration reads it, then manual_review maps to review, with merge/ancestry authority unchanged.

## Spec Change Log

- Review hardening: require full object IDs, preserve metadata exclusion through symlinks, keep receipts outside the proven worktree, clear stale diagnostics, and recheck HEAD around final cleanliness. These are fail-closed proof refinements; preserve manual-review routing and merge authority.
- Integration review: normalize relative receipt paths once before dispatch; archive previous story/merge receipts on retry so an earlier merge-stage failure cannot authorize the new incomplete attempt. Add agent-free prepare/CLI-summary/merge coverage and retry preparation-failure tests. Preserve existing integration module and ancestry checks.
- Rejected review suggestions: marker-history heuristics would weaken explicit-conflict refusal; early rejection before agents is not required for summary classification; stale non-in-flight receipts are already removed when a new run creates its receipt directory.

## Design Notes

A frozen preparation SHA survives sibling merges. Git commit count alone admits empty commits; net changed paths excluding tracking metadata must also prove contribution. Explicit false markers remain a safety veto even when Git changes exist because the agent may have refused finalization for a substantive reason. Missing formatting is a recoverable observation, not such a refusal.

## Verification

- `.venv/bin/python -m pytest -q python/tests/test_bmad_story_commit_evidence.py python/tests/test_bmad_epic_waves_safety_nets.py python/tests/test_bmad_epic_waves_workflow.py python/tests/test_bmad_epic_waves_integrate.py python/tests/test_cli_fail_on_state.py python/tests/test_bmad_story_commit_integration.py`
- `git diff --check`

**Final results:** 244 passed, 2 subtests passed, 8 existing pydot/pyparsing deprecation warnings (53.29 s). Both YAMLs pass `tea validate`; helper and new tests pass `compileall`; `git diff --check` passes. The integration module is unchanged.

## Suggested Review Order

- See Git proof drive classification and durable review handoff.
  [bmad-story-cycle.yaml:757](../../examples/workflows/bmad-story-cycle.yaml#L757)

- Inspect frozen bases, net contribution, cleanliness, and secondary marker vetoes.
  [bmad_story_commit_evidence.py:133](../../python/src/the_edge_agent/bmad_story_commit_evidence.py#L133)

- Trace receipt paths and refreshed bases through preparation and retries.
  [bmad-epic-waves.yaml:1152](../../examples/workflows/bmad-epic-waves.yaml#L1152)

- Verify real-Git cases including missing markers and fail-closed edge cases.
  [test_bmad_story_commit_evidence.py:74](../../python/tests/test_bmad_story_commit_evidence.py#L74)

- Follow the actual CLI gate into the unchanged integration authority.
  [test_bmad_story_commit_integration.py:36](../../python/tests/test_bmad_story_commit_integration.py#L36)

- Check generated commands, retry bases, and preparation failures.
  [test_bmad_epic_waves_safety_nets.py:1255](../../python/tests/test_bmad_epic_waves_safety_nets.py#L1255)
