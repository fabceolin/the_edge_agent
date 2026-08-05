---
title: 'Make BMAD epic waves fail closed without losing or falsely completing stories'
type: 'bugfix'
created: '2026-08-05'
status: 'done'
review_loop_iteration: 0
baseline_commit: '5818f1f3424d0f32dd161fe4b67607a578ec5d67'
context: []
---

<frozen-after-approval reason="human-owned intent — do not modify unless human renegotiates">

## Intent

**Problem:** `bmad-epic-waves.yaml` can discard LLM-resolved merge commits when its post-resolution suite is red, while retaining a later sprint-status commit that hides the discarded work. It can also treat an unchanged/empty story branch as successfully merged and mark that story `done`, as happened to Epic 30 story 30-13.

**Approach:** Make integration accounting commit-based and transactional. Preserve resolved merge commits as recoverable branch state when verification fails, mark only stories whose commits are actually ancestors of the final target HEAD, and expose incomplete resolution as an explicit failed outcome rather than a misleading completion.

## Boundaries & Constraints

**Always:** Keep fail-closed behavior for unresolved markers and red verification; retain every story branch whose work is not accepted into the final target HEAD; derive `merged_ok` and sprint completion from Git ancestry, not subprocess return codes or LLM text alone; preserve already-integrated stories; emit enough state to diagnose partial runs.

**Ask First:** Changing the policy that the configured verification suite must pass before conflict-resolved stories are accepted; automatically merging the recovered Epic 30 branches into `licityeasy`; rewriting published history.

**Never:** Delete unresolved story branches; mark a story done merely because `git merge` returned zero or the DOT node printed `Completed`; silently reset commits without retaining their tips; weaken test commands to force a green result.

## I/O & Edge-Case Matrix

| Scenario | Input / State | Expected Output / Behavior | Error Handling |
|----------|--------------|---------------------------|----------------|
| Clean story merge | Story branch contains commits and merges without conflict | Commit becomes ancestor of target HEAD and story enters `merged_ok` | Preserve branch when ancestry check fails |
| Empty story branch | Story branch tip is already reachable from target before merge | Story is not integrated or marked done | Report `no_story_commits` as a merge conflict/failure |
| Multiple resolved conflicts, green suite | LLM creates sequential merge commits and suite passes | All resolved keys stay in HEAD and enter `merged_ok` | Verify each branch tip is an ancestor before deletion |
| Multiple resolved conflicts, red suite | LLM creates merge commits but suite fails | Target resets to `post_merge_head`; story branches remain; no resolved key is marked done | Report verification failure and retained commit/branch evidence |
| Mixed DOT result | Some story cycles succeed and others fail | Only successful stories with real commits are considered for merge | Failed branches/worktrees remain recoverable and statuses remain unchanged |

</frozen-after-approval>

## Code Map

- `examples/workflows/bmad-epic-waves.yaml` -- owns DOT result parsing, worktree merges, LLM conflict resolution, rollback, sprint marking, and final summary.
- `python/tests/test_bmad_epic_waves_workflow.py` -- new focused regression suite that extracts/executes deterministic workflow node code against temporary Git repositories.

## Tasks & Acceptance

**Execution:**
- [x] `examples/workflows/bmad-epic-waves.yaml` -- validate that every candidate branch contributes commits before accepting a merge; reconcile `merged_ok` against final Git ancestry before status updates.
- [x] `examples/workflows/bmad-epic-waves.yaml` -- make resolution rollback preserve diagnostic/recovery refs and propagate explicit verification failures without falsely marking resolved keys.
- [x] `python/tests/test_bmad_epic_waves_workflow.py` -- cover empty branches, sequential resolved merges with green verification, red-suite rollback, branch retention, and sprint-status marking.

**Acceptance Criteria:**
- Given a story branch with no commits beyond the target, when merge accounting runs, then the story remains not-done and is reported as failed/incomplete.
- Given conflict-resolution commits followed by a red suite, when fail-closed rollback runs, then target HEAD returns to `post_merge_head`, all story branches remain recoverable, and none of those stories is marked done.
- Given resolved branches followed by a green suite, when verification completes, then each branch tip is proven reachable from target HEAD before its branch is deleted or its story marked done.
- Given any partial run, when the workflow summarizes, then `final_status` is `incomplete` and the output identifies failed/unmerged story keys.

## Spec Change Log

## Design Notes

Git ancestry is the durable invariant: for a story key to be `done`, the corresponding story tip (or a recorded resolution merge containing it) must be reachable from the final target HEAD. A zero exit from `git merge` only means the command succeeded; it does not prove the story produced work.

## Verification

**Commands:**
- `.venv/bin/python -m pytest -q python/tests/test_bmad_epic_waves_workflow.py` -- 14 passed.
- `.venv/bin/python -m pytest -q python/tests/test_yaml_engine_core.py python/tests/test_yaml_engine_code.py` -- 58 passed.
- `.venv/bin/tea validate examples/workflows/bmad-epic-waves.yaml` -- 20 nodes and 16 edges valid.
- `.venv/bin/python -m compileall -q python/src/the_edge_agent/bmad_epic_waves_git.py python/tests/test_bmad_epic_waves_workflow.py` -- passed.
- `git diff --check` -- no whitespace errors.

## Suggested Review Order

**Fail-closed orchestration**

- Start with commit-backed merge accounting and retained recovery state.
  [`bmad-epic-waves.yaml:811`](../../examples/workflows/bmad-epic-waves.yaml#L811)

- Refuse retries that would overwrite preserved, unintegrated story branches.
  [`bmad-epic-waves.yaml:634`](../../examples/workflows/bmad-epic-waves.yaml#L634)

- Anchor resolution commits before verified rollback and recheck ancestry after tests.
  [`bmad-epic-waves.yaml:928`](../../examples/workflows/bmad-epic-waves.yaml#L928)

- Update statuses only from proven ancestry; defer branch deletion until final green gate.
  [`bmad-epic-waves.yaml:1010`](../../examples/workflows/bmad-epic-waves.yaml#L1010)

- Require complete candidate accounting and a green final suite.
  [`bmad-epic-waves.yaml:1070`](../../examples/workflows/bmad-epic-waves.yaml#L1070)

**Git invariants**

- Centralize tip, ancestry, reconciliation, and immutable recovery-ref operations.
  [`bmad_epic_waves_git.py:17`](../../python/src/the_edge_agent/bmad_epic_waves_git.py#L17)

**Regression evidence**

- Exercise empty branches, dirty targets, rollback, marker scans, timeout, and final gates.
  [`test_bmad_epic_waves_workflow.py:64`](../../python/tests/test_bmad_epic_waves_workflow.py#L64)
