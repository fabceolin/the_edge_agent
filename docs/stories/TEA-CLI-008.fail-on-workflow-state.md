# TEA-CLI-008: Non-Zero Exit Code on Workflow Failure & DOT Stop-on-Failure

## Status
Draft

## Story

**As a** workflow orchestrator running `tea run` and `tea run --from-dot`,
**I want** the CLI to return a non-zero exit code when a workflow ends in a failure state, and for `--from-dot` to stop execution when an intermediate node fails,
**so that** I can chain workflows reliably and avoid wasting compute on dependent phases after a failure.

## Story Context

**Existing System Integration:**

- Integrates with: `cli.py` run command (lines ~700-839 for `--from-dot`, lines ~1300-1580 for normal `tea run`)
- Technology: Python, typer CLI, tmux (for `--from-dot`)
- Follows pattern: existing `raise typer.Exit(1)` on `event_type == "error"`
- Touch points: `cli.py` (main), `stategraph.py` (engine error events)

**Problem Statement:**

Two related gaps exist:

1. **`tea run` always exits 0 on normal completion** — Even when the workflow reaches a terminal node indicating failure (e.g., `validation_failed` in `bmad-full-cycle.yaml`), the engine emits a `"final"` event and the CLI exits with code 0. The `final_status` state field is data only; the CLI never inspects it.

2. **`tea run --from-dot` has no stop-on-failure** — The phase-by-phase DOT executor continues to all phases regardless of node failure. Additionally, it assumes `success: True` whenever a tmux window closes (line 789-800), without capturing the actual exit code of the command that ran inside.

## Acceptance Criteria

### Part A: `tea run --fail-on-state`

1. **AC-1:** New CLI option `--fail-on-state` accepts a string in the format `key=value` (e.g., `--fail-on-state "final_status=validation_failed_after_correction"`)
2. **AC-2:** After the workflow completes normally (reaches `__end__`), the CLI inspects the final state. If `state[key] == value`, exit with code 1
3. **AC-3:** Multiple `--fail-on-state` options can be provided; if ANY match, exit code is 1
4. **AC-4:** When `--fail-on-state` triggers, a clear message is printed: `"Exit condition matched: {key}={value}"`
5. **AC-5:** Works with all output modes (`--stream`, `--show-graph`, default)
6. **AC-6:** When not provided, behavior is unchanged (exit 0 on normal completion)

### Part B: `--from-dot` Exit Code Capture

7. **AC-7:** The tmux command wrapper captures the actual exit code of the executed command (e.g., writing `$?` to a temp file, then reading it on window close)
8. **AC-8:** A node whose command exits with non-zero is marked `success: False` with the exit code recorded
9. **AC-9:** The existing timeout detection continues to work unchanged

### Part C: `--from-dot` Stop-on-Failure

10. **AC-10:** New CLI option `--dot-stop-on-failure` (default: `True`) stops execution after the current phase completes if any node in that phase failed
11. **AC-11:** When stopping, a clear summary is printed showing which node(s) failed and which phases were skipped
12. **AC-12:** `--no-dot-stop-on-failure` disables this behavior, preserving the current run-all-phases behavior
13. **AC-13:** Overall exit code is 1 when any node failed

### Part D: Documentation Updates

14. **AC-14:** `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` command reference table includes `--dot-stop-on-failure` and `--fail-on-state`
15. **AC-15:** `docs/articles/dot-workflow-orchestration.md` CLI reference table and troubleshooting section include the new flags and failure handling guidance

## Tasks / Subtasks

- [ ] **Task 1: Add `--fail-on-state` CLI option** (AC: 1, 2, 3, 4, 5, 6)
  - [ ] Add `--fail-on-state` option to `run` command in `cli.py` (accepts multiple values)
  - [ ] Parse `key=value` format with validation
  - [ ] After `event_type == "final"`, check final state against all fail conditions
  - [ ] If matched, print message and `raise typer.Exit(1)`
  - [ ] Works in all three output code paths (default, `--stream`, `--show-graph`)

- [ ] **Task 2: Capture actual exit codes in `--from-dot`** (AC: 7, 8, 9)
  - [ ] Change tmux send-keys from `{cmd}; exit` to `{cmd}; echo $? > /tmp/tea_dot_exit_{window_name}; exit`
  - [ ] On window close, read the exit code file
  - [ ] If exit code != 0 or file missing, mark `success: False` with error details
  - [ ] Clean up temp files after reading
  - [ ] Preserve existing timeout logic

- [ ] **Task 3: Add `--dot-stop-on-failure` flag** (AC: 10, 11, 12, 13)
  - [ ] Add `--dot-stop-on-failure / --no-dot-stop-on-failure` option (default: True)
  - [ ] After each phase batch completes, check `errors` list
  - [ ] If stop-on-failure and errors exist, break out of phase loop
  - [ ] Print summary of failed nodes and skipped phases
  - [ ] Ensure final summary still prints with correct counts

- [ ] **Task 4: Testing** (AC: all)
  - [ ] Unit test: `--fail-on-state` triggers exit 1 when state matches
  - [ ] Unit test: `--fail-on-state` does not trigger when state doesn't match
  - [ ] Unit test: multiple `--fail-on-state` conditions (any match)
  - [ ] Unit test: exit code capture reads correct value from temp file
  - [ ] Unit test: failed exit code marks node as failed
  - [ ] Unit test: `--dot-stop-on-failure` skips remaining phases after failure
  - [ ] Unit test: `--no-dot-stop-on-failure` continues all phases

- [ ] **Task 5: Update BMad workflow YAMLs to emit non-zero exit on failure**
  - [ ] `examples/workflows/bmad-full-cycle.yaml`:
    - [ ] In the `validation_failed` node (node 15), replace `return {"final_status": "validation_failed_after_correction"}` with `raise Exception(...)` so the engine emits an `"error"` event and the CLI exits with code 1
    - [ ] Verify the summary banner still prints before the raise
  - [ ] `examples/workflows/bmad-story-validation.yaml`:
    - [ ] In the `summary` node (node 6), after `print(summary)`, add: if `final_status == "incomplete"`, `raise Exception("Validation incomplete - one or more phases failed")` instead of returning
    - [ ] The summary banner prints before the raise so output is preserved
  - [ ] `examples/workflows/bmad-story-development.yaml`:
    - [ ] In the `summary` node (node 6), after `print(summary)`, add: if `final_status == "incomplete"`, `raise Exception("Development incomplete - one or more phases failed")` instead of returning
    - [ ] The summary banner prints before the raise so output is preserved
  - [ ] Test: all three workflows exit non-zero when their respective failure conditions are met

- [ ] **Task 6: Update documentation** (AC: 14, 15)
  - [ ] Update `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md`:
    - [ ] Add `--dot-stop-on-failure` and `--fail-on-state` to the Command Reference table (line ~160)
    - [ ] Add `--dot-stop-on-failure` to the `run --from-dot` options table (line ~162)
    - [ ] Add error handling guidance for stop-on-failure in the Error Handling table (line ~816)
    - [ ] Update execution command examples to show `--dot-stop-on-failure` usage
  - [ ] Update `docs/articles/dot-workflow-orchestration.md`:
    - [ ] Add `--dot-stop-on-failure` and `--fail-on-state` to the CLI Reference table (Section 3, line ~132)
    - [ ] Add a new section or subsection on failure handling and stop-on-failure behavior
    - [ ] Update the Troubleshooting table (Section 8.3, line ~397) with failure-related entries
    - [ ] Update execution examples (Section 3.1) to show `--dot-stop-on-failure` usage

## Dev Notes

### Key Files

- `python/src/the_edge_agent/cli.py` — CLI changes (Tasks 1-3)
  - Lines ~700-839: `--from-dot` execution block (Tasks 2, 3)
  - Lines ~1300-1580: Normal `tea run` event loop (Task 1)
  - The three output mode branches (default ~1333, show-graph ~1359, stream ~1513) all need the `--fail-on-state` check at the `"final"` event
- `examples/workflows/bmad-full-cycle.yaml` — Workflow fix (Task 5)
  - Node 15 `validation_failed` (line ~709): change `return` to `raise Exception`
- `examples/workflows/bmad-story-validation.yaml` — Workflow fix (Task 5)
  - Node 6 `summary` (line ~341): raise on `final_status == "incomplete"`
- `examples/workflows/bmad-story-development.yaml` — Workflow fix (Task 5)
  - Node 6 `summary` (line ~387): raise on `final_status == "incomplete"`
- `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` — Doc update (Task 6)
- `docs/articles/dot-workflow-orchestration.md` — Doc update (Task 6)

### Exit Code Capture Approach

The tmux window approach loses exit codes because `; exit` always exits the shell with 0. The fix:

```bash
# Before (loses exit code):
{cmd}; exit

# After (captures exit code):
{cmd}; echo $? > /tmp/tea_dot_exit_{window_name}; exit
```

On window close, read `/tmp/tea_dot_exit_{window_name}`. If contents != "0" or file doesn't exist, mark failure.

### `--fail-on-state` Usage with bmad-full-cycle

```bash
# Normal run — exits 1 if validation failed after correction
tea run examples/workflows/bmad-full-cycle.yaml \
  --fail-on-state "final_status=validation_failed_after_correction" \
  --fail-on-state "final_status=incomplete"

# DOT orchestration — stops phases on failure, workflow exits non-zero via --fail-on-state
tea run --from-dot workflow.dot \
  --dot-workflow examples/workflows/bmad-full-cycle.yaml \
  --dot-stop-on-failure
```

### Testing

- Tests should use `typer.testing.CliRunner` with the `run` command
- For tmux tests, mock `subprocess.run` calls
- For `--fail-on-state`, create minimal YAML workflows that set specific state values

### Existing Patterns

- `raise typer.Exit(1)` is the standard error exit pattern (used ~30 times in cli.py)
- `emit_ndjson_event("error", ...)` is used before exit in stream mode
- `--from-dot` already tracks `results` and `errors` lists

## Definition of Done

- [ ] `tea run` with `--fail-on-state` returns exit 1 when condition matches
- [ ] `tea run --from-dot` captures actual command exit codes
- [ ] `tea run --from-dot` stops on failure by default
- [ ] All existing tests pass (no regressions)
- [ ] New tests cover the three features
- [ ] `bmad-full-cycle.yaml` `validation_failed` node raises exception for non-zero exit
- [ ] `bmad-story-validation.yaml` `summary` node raises exception when incomplete
- [ ] `bmad-story-development.yaml` `summary` node raises exception when incomplete
- [ ] Works end-to-end with `bmad-full-cycle.yaml`
- [ ] DOT Workflow Orchestration LLM Guide updated with new flags
- [ ] DOT Workflow Orchestration article updated with new flags and failure handling

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-28 | 1.0 | Initial story creation | PO (Sarah) |
