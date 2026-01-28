# TEA-CLI-009: Wave and Step Selection for DOT Workflow Execution

## Status
Draft

## Story

**As a** workflow orchestrator using `tea run --from-dot`,
**I want** to specify which wave (phase) to start execution from and, when using `--dot-max-parallel`, which step within a wave to resume,
**so that** I can resume failed workflows from a specific point without re-executing completed phases or steps.

## Story Context

**Existing System Integration:**

- Integrates with: `cli.py` run command (lines ~700-839 for `--from-dot` execution)
- Technology: Python, typer CLI, tmux (for DOT execution)
- Follows pattern: existing `--dot-*` options (`--dot-max-parallel`, `--dot-session`, etc.)
- Touch points: `cli.py` (CLI), `dot_parser.py` (phase detection and YAML generation)

**Problem Statement:**

Currently, `tea run --from-dot` always starts from the beginning of the workflow. When a workflow fails mid-execution:

1. **Phase-level resume**: Users cannot skip already-completed waves/phases and must re-run everything from the start
2. **Step-level resume**: When using parallel execution (`--dot-max-parallel`), users cannot specify which step within a wave to resume from

This wastes compute time and risks re-executing side effects on already-completed nodes.

**Terminology:**

- **Wave/Phase**: A group of nodes that can execute in parallel (defined by DOT clusters or detected fan-out/fan-in patterns)
- **Step**: An individual node within a wave
- **Parallel wave**: A wave where multiple steps execute concurrently (controlled by `--dot-max-parallel`)

## Acceptance Criteria

### Part A: Wave Selection (`--dot-start-wave`)

1. **AC-1:** New CLI option `--dot-start-wave <N>` accepts a 1-based wave number (e.g., `--dot-start-wave 3` starts from the 3rd wave)
2. **AC-2:** Waves before `<N>` are skipped entirely (no execution, no tmux windows created)
3. **AC-3:** A summary message is printed: `"Skipping waves 1-{N-1}, starting from wave {N}"`
4. **AC-4:** If `--dot-start-wave` exceeds the total number of waves, an error is raised with the actual wave count
5. **AC-5:** Works with both Command Mode (nodes with `command` attribute) and Workflow Mode (`--dot-workflow`)
6. **AC-6:** `--dot-dry-run` shows the execution plan with skipped waves clearly marked

### Part B: Step Selection (`--dot-start-step`)

7. **AC-7:** New CLI option `--dot-start-step <M>` accepts a 1-based step number within the starting wave
8. **AC-8:** Steps before `<M>` in the starting wave are skipped
9. **AC-9:** A summary message is printed: `"Skipping steps 1-{M-1} in wave {N}, starting from step {M}"`
10. **AC-10:** If `--dot-start-step` exceeds the number of steps in the wave, an error is raised with the actual step count
11. **AC-11:** `--dot-start-step` without `--dot-start-wave` implies `--dot-start-wave 1`
12. **AC-12:** `--dot-start-step` only affects the starting wave; subsequent waves execute all steps

### Part C: Combined Usage

13. **AC-13:** `--dot-start-wave 2 --dot-start-step 3` skips wave 1 entirely, then skips steps 1-2 in wave 2
14. **AC-14:** The final summary correctly reports only the nodes that were actually executed (not skipped)
15. **AC-15:** `--dot-dry-run` with both options shows complete execution plan with skipped waves and steps marked

### Part D: Label-Based Selection (Alternative)

16. **AC-16:** `--dot-start-from <label>` accepts a node label and starts from the wave containing that node
17. **AC-17:** When using `--dot-start-from`, execution starts from the specified node within its wave (other nodes in the wave before it are skipped)
18. **AC-18:** If the label doesn't exist in the DOT file, an error is raised listing available node labels

### Part E: Documentation Updates

19. **AC-19:** `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` command reference table includes the new options
20. **AC-20:** Help text (`tea run --help`) includes clear descriptions of the new options

## Tasks / Subtasks

- [ ] **Task 1: Add `--dot-start-wave` CLI option** (AC: 1, 2, 3, 4, 5, 6)
  - [ ] Add `--dot-start-wave` option to `run` command in `cli.py` (type: int, default: 1)
  - [ ] Validate wave number is positive and within bounds
  - [ ] Modify phase iteration to skip waves before start_wave
  - [ ] Print skip summary message
  - [ ] Update dry-run output to show skipped waves

- [ ] **Task 2: Add `--dot-start-step` CLI option** (AC: 7, 8, 9, 10, 11, 12)
  - [ ] Add `--dot-start-step` option to `run` command in `cli.py` (type: int, default: 1)
  - [ ] Validate step number is positive and within bounds for the starting wave
  - [ ] Modify step iteration within the starting wave to skip steps before start_step
  - [ ] Print skip summary message for steps
  - [ ] Subsequent waves execute all steps normally

- [ ] **Task 3: Implement combined wave/step selection** (AC: 13, 14, 15)
  - [ ] Handle combined `--dot-start-wave` and `--dot-start-step` logic
  - [ ] Update final summary to report only executed nodes
  - [ ] Update dry-run to show complete plan with skipped items marked

- [ ] **Task 4: Add `--dot-start-from` label-based selection** (AC: 16, 17, 18)
  - [ ] Add `--dot-start-from` option to `run` command in `cli.py` (type: str, default: None)
  - [ ] Parse DOT file to build label-to-wave/step mapping
  - [ ] Find the wave and step index for the specified label
  - [ ] Apply same skip logic as wave/step selection
  - [ ] Validate label exists; if not, show error with available labels

- [ ] **Task 5: Testing** (AC: all)
  - [ ] Unit test: `--dot-start-wave 2` skips wave 1
  - [ ] Unit test: `--dot-start-wave` exceeds wave count raises error
  - [ ] Unit test: `--dot-start-step 3` skips steps 1-2 in starting wave
  - [ ] Unit test: `--dot-start-step` exceeds step count raises error
  - [ ] Unit test: combined wave + step selection works correctly
  - [ ] Unit test: `--dot-start-from` finds correct wave/step
  - [ ] Unit test: `--dot-start-from` with invalid label raises error with suggestions
  - [ ] Unit test: `--dot-dry-run` shows skipped items correctly
  - [ ] Integration test: resume from wave 2 with real DOT file

- [ ] **Task 6: Update documentation** (AC: 19, 20)
  - [ ] Update `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md`:
    - [ ] Add `--dot-start-wave`, `--dot-start-step`, `--dot-start-from` to Command Reference table
    - [ ] Add usage examples for resuming failed workflows
    - [ ] Add section on "Resuming Failed Workflows"
  - [ ] Verify `--help` output includes clear descriptions

## Dev Notes

### Key Files

- `python/src/the_edge_agent/cli.py` — CLI changes (Tasks 1-4)
  - Lines ~700-839: `--from-dot` execution block
  - The phase loop (around line ~780) iterates through phases
  - Need to add skip logic before phase execution
- `python/src/the_edge_agent/dot_parser.py` — Phase detection (Task 4 for label lookup)
  - `PhaseInfo` dataclass contains phase name, label, items list
  - `detect_phases()` function returns ordered list of phases

### Phase/Wave Structure

From `dot_parser.py`, phases are represented as:

```python
@dataclass
class PhaseInfo:
    name: str                      # e.g., "phase1"
    label: str                     # e.g., "1. Setup"
    items: List[str]               # Node labels in this phase (parallel items)
    fan_out_node: Optional[str]    # Entry point
    fan_in_node: Optional[str]     # Consolidation point
```

The CLI receives phases as a list, so wave selection is simply index-based:

```python
# Proposed implementation sketch
for i, phase in enumerate(phases):
    wave_num = i + 1  # 1-based for user-facing
    if wave_num < start_wave:
        print(f"Skipping wave {wave_num}: {phase.label}")
        continue

    for j, step in enumerate(phase.items):
        step_num = j + 1  # 1-based for user-facing
        if wave_num == start_wave and step_num < start_step:
            print(f"Skipping step {step_num} in wave {wave_num}: {step}")
            continue
        # Execute step...
```

### Label Lookup for `--dot-start-from`

```python
# Build label-to-location mapping
label_map = {}
for i, phase in enumerate(phases):
    for j, item_label in enumerate(phase.items):
        label_map[item_label] = (i + 1, j + 1)  # (wave, step)

# Lookup
if start_from_label:
    if start_from_label not in label_map:
        available = list(label_map.keys())
        raise ValueError(f"Label '{start_from_label}' not found. Available: {available[:10]}...")
    start_wave, start_step = label_map[start_from_label]
```

### Error Messages

- Wave out of bounds: `"Invalid wave number {N}. This DOT file has {total} waves."`
- Step out of bounds: `"Invalid step number {M} for wave {N}. Wave {N} has {total} steps."`
- Label not found: `"Label '{label}' not found in DOT file. Available labels: {labels}"`

### Mutual Exclusivity

`--dot-start-from` should be mutually exclusive with `--dot-start-wave` and `--dot-start-step`:

```python
if start_from and (start_wave != 1 or start_step != 1):
    raise ValueError("Cannot use --dot-start-from with --dot-start-wave or --dot-start-step")
```

### Testing

- Tests should use `typer.testing.CliRunner` with the `run` command
- Create test DOT files with multiple waves and steps
- Mock tmux execution to avoid actual subprocess calls

### Existing Patterns

- `--dot-dry-run` already shows the execution plan
- Phase iteration exists in cli.py around line ~780
- Error handling uses `typer.Exit(1)` with `console.print(..., style="red")`

## Definition of Done

- [ ] `--dot-start-wave N` skips waves 1 through N-1
- [ ] `--dot-start-step M` skips steps 1 through M-1 in the starting wave
- [ ] `--dot-start-from <label>` finds and starts from the specified node
- [ ] Combined options work correctly together
- [ ] Dry-run shows skipped waves and steps
- [ ] Final summary reports only executed nodes
- [ ] Error messages are clear when wave/step/label is invalid
- [ ] All existing tests pass (no regressions)
- [ ] New tests cover all acceptance criteria
- [ ] Documentation updated with new options and examples

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-28 | 1.0 | Initial story creation | PO (Sarah) |
