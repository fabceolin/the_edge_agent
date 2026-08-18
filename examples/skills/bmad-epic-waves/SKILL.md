---
name: bmad-epic-waves
description: Execute the BMad Epic Waves workflow from the_edge_agent for one epic or the pending epics in a repository. Use when the user asks to run, resume, simulate, or inspect BMAD epic waves, parallelize stories by dependency, create missing story files, integrate isolated worktrees, or close completed epics.
---

# BMAD Epic Waves

Use the TEA workflow at `/home/ubuntu/src/the_edge_agent/examples/workflows/bmad-epic-waves.yaml` to execute BMAD stories in dependency waves. The target repository is supplied by the user or inferred from the current workspace.

## Safety gate

Before any mutating run:

1. Resolve the absolute target repository path and inspect `git status -sb`, `git worktree list`, and active `tea run`/Codex processes.
2. Check for an active `.claude/.bmad-epic-waves.lock` or another workflow using the same repository. Do not start a second mutating run while one is active.
3. Run a dry plan first for a new or multi-epic request. Report the candidate stories, missing story files, dependency waves, and explicit blockers.
4. Preserve unrelated user changes. Do not reset, stash, delete worktrees, or resolve conflicts manually unless the workflow or the user explicitly authorizes that exact action.

If the repository is dirty, distinguish pre-existing changes from workflow changes and stop for direction when the workflow's merge/commit safety cannot be established.

## Commands

Run from any directory with the absolute workflow and repository paths. Always set
`TEA_BIN` explicitly because the example workflow may otherwise resolve a developer-specific
fallback path. Run in debug/streaming mode by default for mutating waves:

- `TEA_SHELL_VERBOSE=1` streams shell-provider LLM output into the story's tmux window in real time.
- `-vvv` enables TEA DEBUG logging for the orchestrator.
- The environment is inherited by the nested `tea` processes started in tmux, so each story window
  shows the agent's live output instead of remaining blank until completion.

Dry plan:

```bash
TEA_BIN=/home/ubuntu/src/the_edge_agent/.venv/bin/tea \
TEA_SHELL_VERBOSE=1 \
/home/ubuntu/src/the_edge_agent/.venv/bin/tea run -vvv \
  /home/ubuntu/src/the_edge_agent/examples/workflows/bmad-epic-waves.yaml \
  --input '{"arg":"pending","repo_path":"/home/ubuntu/src/licityeasy","dry_run":true}'
```

Use these `arg` forms:

- `"epic-30"` or `"30"`: one epic.
- `"pending"`: all non-`done` stories discovered in the repository; use only when the user explicitly requests all pending work.
- `"all"`: all stories, including stories from every discovered epic that are not `done`.
- `"8,29"` or `"epic-8,epic-29"`: a selected multi-epic run.

For an approved mutating run, omit `dry_run` or set it to `false`:

```bash
TEA_BIN=/home/ubuntu/src/the_edge_agent/.venv/bin/tea \
TEA_SHELL_VERBOSE=1 \
/home/ubuntu/src/the_edge_agent/.venv/bin/tea run -vvv \
  /home/ubuntu/src/the_edge_agent/examples/workflows/bmad-epic-waves.yaml \
  --input '{"arg":"epic-30","repo_path":"/home/ubuntu/src/licityeasy","max_review_cycles":3,"run_retrospective":true,"flip_epic_status":true}'
```

Monitor the tmux session while the parent command remains active. The session name is normally the
epic key (`epic-30`) or `epics-all` for a multi-epic run:

```bash
tmux list-windows -t epic-30
tmux attach -t epic-30
tmux capture-pane -p -t epic-30:<window-name> -S -200
```

Detaching with `Ctrl-b d` leaves the wave running. Do not kill a window to leave the viewer. DEBUG
output can contain source excerpts, prompts, model responses, and command output; treat the tmux
session and captured panes as potentially sensitive.

Useful controls:

- `only_wave`: run one numbered wave after inspecting the plan.
- `stop_after_prep`: create missing story files and stop before implementation.
- `auto_create_stories`: disable only when every candidate story file already exists.
- `auto_model`: defaults to `true`; classifies complexity plus risk and applies the balanced routing policy below. Set `false` for the conservative single-model path.
- `dev_model`: model used by story creation, graph/orchestration and the conservative fallback; default is `gpt-5.6-sol`. Classified story routes use their explicit policy models.
- `review_model`: model used for adversarial review; default is `gpt-5.6-sol` and effort remains `xhigh`.
- `model_tiers`: optional mapping from an effective route to a named shell provider in `bmad-story-cycle.yaml`.
- `fix_model_tiers`: optional mapping from an effective route to the provider used by the first promoted fix.
- `dot_max_parallel`: maximum number of stories concurrently executed within each dependency wave; default is `4`.
- `max_review_cycles`: review limit per story; default is 3.
- `run_retrospective`: write the retrospective after a complete single-epic run; default is true.
- `flip_epic_status`: allow the workflow to mark the epic and retrospective done; default is true.
- `stop_on_wave_failure`: keep true unless the user explicitly asks to continue after a failed wave.
- `reprocess_done`: normally false; enable only for intentional reprocessing.
- `story_keys`: restrict a controlled pilot to named stories when supported by the workflow.

Default balanced routing preserves a stronger reviewer:

| Effective route | Development | First normal fix |
|---|---|---|
| `trivial` | GPT-5.6 Luna / low | Terra / medium |
| `small` | GPT-5.6 Terra / medium | Terra / high |
| `standard` | GPT-5.6 Terra / high | Sol / high |
| `transversal` | GPT-5.6 Terra / xhigh | Sol / xhigh |
| `high_risk` | GPT-5.6 Sol / high | Sol / xhigh |
| `frontier` | GPT-5.6 Sol / xhigh | Sol / xhigh |

The reviewer remains `review_model`/xhigh (Sol/xhigh by default). A structural finding or a second
`CHANGES_REQUESTED` forces the fix to Sol/xhigh. Non-approved stories skip the finish LLM; approved
stories use Luna/low to execute the existing test/commit/status protocol. Missing classifications,
explicit `deps`, malformed risk flags, and `auto_model:false` fail safe to the conservative
`dev_model`/xhigh route.

To force the former single-model behavior:

```bash
TEA_BIN=/home/ubuntu/src/the_edge_agent/.venv/bin/tea \
TEA_SHELL_VERBOSE=1 \
/home/ubuntu/src/the_edge_agent/.venv/bin/tea run -vvv \
  /home/ubuntu/src/the_edge_agent/examples/workflows/bmad-epic-waves.yaml \
  --input '{"arg":"all","repo_path":"/home/ubuntu/src/licityeasy","auto_model":false,"dev_model":"gpt-5.6-sol","review_model":"gpt-5.6-sol","dot_max_parallel":4}'
```

Inspect the dry-plan model lines before execution: they include base tier, risk flags, effective
route, concrete model/effort label, development provider, and promoted fix provider.

## Execution protocol

1. Discover status from `_bmad-output/implementation-artifacts/sprint-status.yaml`; treat `done` as complete and preserve explicit gates, research-only stories, and stories blocked by prerequisites.
2. Use `dry_run:true` for `pending`, `all`, or a list of epics. Do not silently turn a dry plan into execution.
3. Check the generated dependency graph. The workflow creates isolated git worktrees, executes each wave, serializes merges, verifies ancestry and tests, then marks only proven integrations as `done`.
4. Let the workflow handle story creation, review cycles, merges, conflict gates, status commits, verification, and single-epic retrospectives. Do not duplicate those edits in the parent agent.
5. Monitor long runs through the debug-enabled tmux windows and communicate wave boundaries, failures, conflicts, and blockers. Prefer `tmux capture-pane` for non-interactive checks; use `tmux attach` when a human wants to watch directly. If a story fails while `stop_on_wave_failure:true`, stop scheduling new stories for that wave but let every story already running finish its current cycle and report its result before closing the wave. Never terminate the wave's tmux/session or kill in-flight story processes merely because the wave has failed.
6. If the workflow stops, inspect its final report and repository status before resuming. A wave is considered finished only after all already-started stories have reached a terminal result or their configured timeout.
7. After completion, verify `git status -sb`, the sprint status, the final test result, and any remaining worktrees. Report exact stories/waves completed and anything not integrated.

## Failure handling

- A failed or conflicted story remains incomplete; do not mark it `done` manually.
- With `stop_on_wave_failure:true`, failure stops only the dispatch of new stories; it does not cancel stories already executing. Wait for the active stories in that wave to complete or reach their configured timeout, preserve their worktrees and evidence, and only then finalize the failed wave.
- A red suite, missing story, invalid sprint status, unresolved conflict, or failed ancestry gate is fail-closed. Report the blocker and preserve the evidence.
- Never use `git reset --hard`, `git checkout --`, `git merge --abort`, or recursive deletion as a generic cleanup step. Only the workflow's own guarded rollback may perform its documented rollback, and only after confirming its target.
- Do not run `pending` against a repository with another active epic-wave session, uncommitted operator work, or unexplained locked worktrees.

## Communication

Communicate in the user's language. Before execution state the target repo, selected epics, dry-run result, and mutating options. During execution give concise progress updates. Finish with the workflow's final status, stories integrated, test result, retrospective/status changes, and explicit blockers.
