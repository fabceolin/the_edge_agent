# TEA-CLI-009: Wave and Step Selection for DOT Workflow Execution

## Status
Done

**Validated:** 2026-01-28 by SM (Bob) - All Definition of Ready criteria passed
**Implemented:** 2026-01-28 by James (Dev Agent) - All tasks complete, 30 tests passing
**QA Gate:** 2026-01-28 - PASS (100/100) - All ACs verified, all risks mitigated
**Done:** 2026-01-28 by SM (Bob) - Gate PASS, story complete

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

- [x] **Task 1: Add `--dot-start-wave` CLI option** (AC: 1, 2, 3, 4, 5, 6)
  - [x] Add `--dot-start-wave` option to `run` command in `cli.py` (type: int, default: 1, min=1)
  - [x] Validate wave number within bounds AFTER `levels` computed (~line 757): `if dot_start_wave > len(levels)`
  - [x] Modify phase loop (~line 800) to skip waves: `if phase_idx < dot_start_wave: continue`
  - [x] Print skip summary message using `typer.echo(..., err=True)`
  - [x] Update dry-run output (~line 764-770) to show `[SKIPPED]` prefix for waves < start_wave

- [x] **Task 2: Add `--dot-start-step` CLI option** (AC: 7, 8, 9, 10, 11, 12)
  - [x] Add `--dot-start-step` option to `run` command in `cli.py` (type: int, default: 1, min=1)
  - [x] Validate step number within bounds for starting wave: check against `len(levels[dot_start_wave - 1])`
  - [x] Modify step iteration within node loop (~line 811): `if phase_idx == dot_start_wave and step_idx < dot_start_step: continue`
  - [x] Print skip summary message for steps
  - [x] Subsequent waves (phase_idx > start_wave) execute all steps (step skip condition only applies when `phase_idx == start_wave`)

- [x] **Task 3: Implement combined wave/step selection** (AC: 13, 14, 15)
  - [x] Combined logic already handled by Task 1 and 2 conditions working together
  - [x] Track skipped nodes count: `skipped_count` counter incremented on each skip
  - [x] Update final summary: `executed_count = total_nodes - skipped_count`
  - [x] Update dry-run to show complete plan with skipped items marked (loop over all waves/steps, mark those that would be skipped)

- [x] **Task 4: Add `--dot-start-from` label-based selection** (AC: 16, 17, 18)
  - [x] Add `--dot-start-from` option to `run` command in `cli.py` (type: Optional[str], default: None)
  - [x] Build `label_map: Dict[str, Tuple[int, int]]` after `levels` computed (~line 757)
  - [x] Add mutual exclusivity check: error if `dot_start_from` and (`dot_start_wave != 1` or `dot_start_step != 1`)
  - [x] Resolve label to (wave, step): `dot_start_wave, dot_start_step = label_map[dot_start_from]`
  - [x] Validate label exists; if not, raise `typer.BadParameter` with available labels (limit to 10)

- [x] **Task 5: Testing** (AC: all)
  - [x] Create test fixtures: `multi_wave_workflow.dot`, `single_wave.dot`, `special_chars.dot` in `python/tests/fixtures/`
  - [x] Unit test boundary: `--dot-start-wave 0` raises error (typer min=1 handles this)
  - [x] Unit test boundary: `--dot-start-wave N+1` raises error with wave count
  - [x] Unit test boundary: `--dot-start-step 0` raises error
  - [x] Unit test boundary: `--dot-start-step M+1` raises error with step count
  - [x] Unit test: `--dot-start-wave 2` skips wave 1 (check stdout for skip message)
  - [x] Unit test: `--dot-start-step 3` skips steps 1-2 (check stdout for skip messages)
  - [x] Unit test: combined `--dot-start-wave 2 --dot-start-step 2` skips wave 1 + step 1 of wave 2
  - [x] Unit test: `--dot-start-from "Build"` resolves to correct wave/step
  - [x] Unit test: `--dot-start-from "NonExistent"` raises error listing available labels
  - [x] Unit test: mutual exclusivity `--dot-start-from X --dot-start-wave 2` raises error
  - [x] Unit test: `--dot-dry-run --dot-start-wave 2` shows `[SKIPPED]` markers
  - [x] Integration test: resume from wave 2 with mocked tmux (verify only wave 2+ windows created)

- [x] **Task 6: Update documentation** (AC: 19, 20)
  - [x] Update `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md`:
    - [x] Add `--dot-start-wave`, `--dot-start-step`, `--dot-start-from` to Command Reference table
    - [x] Add usage examples for resuming failed workflows
    - [x] Add section on "Resuming Failed Workflows"
  - [x] Update `docs/shared/cli-reference.md` with new options
  - [x] Verify `--help` output includes clear descriptions (typer auto-generates from option help text)

## Dev Notes

> **ARCHITECTURAL NOTE (v1.4):** The original Dev Notes incorrectly referenced `PhaseInfo` and `detect_phases()` from `dot_parser.py`. The actual `--from-dot` implementation in `cli.py` computes `levels` inline using Kahn's algorithm and does NOT use `dot_parser.analyze_graph()`. Implementation MUST follow the actual code pattern using `levels: List[List[str]]`, not `PhaseInfo`. This correction mitigates TECH-002 risk.

### Key Files

- `python/src/the_edge_agent/cli.py` — CLI changes (Tasks 1-4)
  - Lines ~700-900: `--from-dot` execution block
  - Lines ~744-755: Kahn's algorithm computes `levels` (list of lists)
  - Line ~764: `enumerate(levels, 1)` iterates phases with 1-based index
  - Line ~800: Phase execution loop (`for phase_idx, level in enumerate(levels, 1)`)
  - Need to add skip logic BEFORE phase execution at line ~800 and within batch iteration at line ~811

**IMPORTANT:** The CLI does NOT use `PhaseInfo` or `detect_phases()` from `dot_parser.py`. Instead, it computes `levels` inline using Kahn's algorithm. The `levels` variable is a `List[List[str]]` where each inner list contains node_ids that can execute in parallel.

### Phase/Wave Structure (Actual Implementation)

The CLI computes phases inline using Kahn's algorithm (lines 744-755):

```python
# Kahn's algorithm with levels (from cli.py lines 744-755)
levels = []  # List[List[str]] - each inner list is node_ids in that phase
queue = deque([n for n in all_nodes if in_degree[n] == 0])
while queue:
    level = list(queue)
    levels.append(level)
    queue.clear()
    for node in level:
        for neighbor in adj[node]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)
```

Wave selection is index-based on the `levels` list:

```python
# Proposed implementation sketch (integrate at line ~800)
for phase_idx, level in enumerate(levels, 1):
    # Skip entire waves before start_wave
    if phase_idx < start_wave:
        typer.echo(f"Skipping wave {phase_idx}: {len(level)} node(s)", err=True)
        continue

    # ... existing phase_size = len(level) ...

    for batch_start in range(0, phase_size, dot_max_parallel):
        batch = level[batch_start : batch_start + dot_max_parallel]

        for node_id in batch:
            step_idx = level.index(node_id) + 1  # 1-based step index

            # Skip steps before start_step in the starting wave only
            if phase_idx == start_wave and step_idx < start_step:
                node = parsed.nodes.get(node_id)
                label = node.label if node else node_id
                typer.echo(f"  Skipping step {step_idx}: {label}", err=True)
                continue

            # ... existing node execution logic ...
```

### Label Lookup for `--dot-start-from`

Build label-to-location mapping from `levels` and `parsed.nodes`:

```python
# Build label-to-location mapping (insert after levels computed, ~line 757)
label_map = {}
for wave_idx, level in enumerate(levels, 1):
    for step_idx, node_id in enumerate(level, 1):
        node = parsed.nodes.get(node_id)
        label = node.label if node else node_id
        label_map[label] = (wave_idx, step_idx)

# Lookup (in option validation)
if start_from_label:
    if start_from_label not in label_map:
        available = list(label_map.keys())
        raise typer.BadParameter(
            f"Label '{start_from_label}' not found. Available: {', '.join(available[:10])}..."
        )
    start_wave, start_step = label_map[start_from_label]
```

### Error Messages

Use `typer.BadParameter` for CLI option validation errors (consistent with existing CLI patterns):

- Wave out of bounds: `typer.BadParameter(f"Wave {N} exceeds total waves ({total}). Valid range: 1-{total}")`
- Step out of bounds: `typer.BadParameter(f"Step {M} exceeds steps in wave {N} ({total} steps). Valid range: 1-{total}")`
- Label not found: `typer.BadParameter(f"Label '{label}' not found. Available: {labels}")`
- Zero/negative value: `typer.BadParameter("Value must be at least 1")`

### Mutual Exclusivity

`--dot-start-from` should be mutually exclusive with `--dot-start-wave` and `--dot-start-step`. Use typer's callback pattern:

```python
# Add early in the --from-dot block (after options parsed, before execution)
if dot_start_from and (dot_start_wave != 1 or dot_start_step != 1):
    raise typer.BadParameter(
        "--dot-start-from is mutually exclusive with --dot-start-wave and --dot-start-step"
    )
```

### Testing

- Tests should use `typer.testing.CliRunner` with the `run` command (existing pattern in `test_cli_fail_on_state.py`)
- Create test DOT files with multiple waves and steps (see Test Design for fixtures)
- Mock `subprocess.run` for tmux commands, not the whole subprocess module
- Use `@patch('subprocess.run')` decorator pattern from TEA-CLI-008 tests

### Existing Patterns (Reference TEA-CLI-008)

- `--dot-dry-run` shows execution plan (lines 762-777)
- Phase iteration at line ~800: `for phase_idx, level in enumerate(levels, 1)`
- Error handling uses `typer.echo(..., err=True)` for stderr output
- Exit codes: `raise typer.Exit(1)` for failures
- Option validation happens BEFORE execution starts (fail fast)

### New CLI Options Definition

Add these options to the `run` command (follow existing `--dot-*` pattern at lines ~620-660):

```python
dot_start_wave: int = typer.Option(
    1,
    "--dot-start-wave",
    help="Start from wave N (1-based). Waves 1 to N-1 are skipped.",
    min=1,
),
dot_start_step: int = typer.Option(
    1,
    "--dot-start-step",
    help="Start from step M in the starting wave (1-based). Steps 1 to M-1 are skipped.",
    min=1,
),
dot_start_from: Optional[str] = typer.Option(
    None,
    "--dot-start-from",
    help="Start from the node with this label. Mutually exclusive with --dot-start-wave/--dot-start-step.",
),
```

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

## QA Notes - Risk Profile

**Date:** 2026-01-28
**Reviewer:** Quinn (Test Architect)
**Risk Level:** MEDIUM (Score: 71/100)

### Identified Risks

| Risk ID | Category | Description | Score | Priority |
|---------|----------|-------------|-------|----------|
| TECH-001 | Technical | Off-by-one errors in wave/step 1-based vs 0-based indexing | 6 | High |
| TECH-002 | Technical | Phase structure mismatch between CLI levels and dot_parser PhaseInfo | 4 | Medium | **MITIGATED v1.4** |
| TECH-003 | Technical | Mutual exclusivity validation missing for --dot-start-from | 4 | Medium |
| DATA-001 | Data | Label lookup fails for nodes with transformed/special-char names | 4 | Medium |
| TECH-004 | Technical | Dry-run output doesn't clearly show skipped items | 4 | Medium |
| OPS-001 | Operational | Summary message counts incorrect after skipping | 2 | Low |
| OPS-002 | Operational | Error messages expose internal structure | 1 | Low |

### Key Mitigations

1. **TECH-001 (High Priority):** Use `enumerate(levels, 1)` consistently; add assertions at validation boundaries; thorough boundary testing for wave 1, N, N+1
2. **TECH-002 (MITIGATED v1.4):** Dev Notes corrected to use actual `levels` list from Kahn's algorithm instead of incorrect `PhaseInfo` reference. Tasks updated with correct line numbers.
3. **DATA-001:** Build label map with both original and normalized keys; include available labels in error messages
4. **TECH-003:** Add explicit mutual exclusivity check early in command handler

### Testing Priorities

**Priority 1 - Must Test:**
- Boundary conditions for `--dot-start-wave` (0, 1, N, N+1)
- Boundary conditions for `--dot-start-step` (0, 1, M, M+1)
- Combined wave+step selection
- Label lookup with special characters

**Priority 2 - Should Test:**
- `--dot-start-from` mutual exclusivity with numeric options
- Dry-run output format with skipped items
- Final summary accuracy

**Priority 3 - Nice to Test:**
- Error message formatting
- Edge case: single-wave workflow
- Edge case: single-step wave

### Gate Recommendation

**CONCERNS** - The TECH-001 risk (score 6) requires careful implementation due to common off-by-one error patterns. Implementation should proceed with mandatory boundary testing.

### Assessment File

Risk profile: docs/qa/assessments/TEA-CLI-009-risk-20260128.md

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-28 | 1.0 | Initial story creation | PO (Sarah) |
| 2026-01-28 | 1.1 | Added QA Notes - Risk Profile | Quinn (QA) |
| 2026-01-28 | 1.2 | Added QA Notes - NFR Assessment | Quinn (QA) |
| 2026-01-28 | 1.3 | Added QA Notes - Requirements Trace | Quinn (QA) |
| 2026-01-28 | 1.4 | **ARCHITECTURAL CORRECTION**: Fixed Dev Notes to use actual CLI implementation pattern (`levels` from Kahn's algorithm) instead of incorrectly referencing `PhaseInfo`/`detect_phases()`. Updated Tasks with correct line numbers and implementation details. This mitigates TECH-002 risk (Phase structure mismatch). | Winston (Architect) |
| 2026-01-28 | 1.5 | **PO PRODUCT CORRECTION**: Updated SM Validation section to reflect Architect v1.4 corrections. Changed Technical approach status to CORRECTED. Reset Status to Draft for re-validation. Added development recommendation to follow v1.4 corrections. Updated Assessment Summary technical guidance to match corrected architecture. | Sarah (PO) |
| 2026-01-28 | 2.0 | **IMPLEMENTATION COMPLETE**: All 6 tasks implemented. Added `--dot-start-wave`, `--dot-start-step`, `--dot-start-from` CLI options. Created 30 unit/integration tests (all passing). Updated documentation. Status changed to "Ready for Review". | James (Dev Agent - Claude Opus 4.5) |

## QA Notes - NFR Assessment

**Date:** 2026-01-28
**Reviewer:** Quinn (Test Architect)
**Quality Score:** 80/100

### NFR Coverage

| NFR | Status | Summary |
|-----|--------|---------|
| Security | PASS | CLI input validation present, no command injection vectors |
| Performance | PASS | O(1) skip logic, no re-execution overhead |
| Reliability | CONCERNS | Off-by-one indexing risk (TECH-001) requires defensive coding |
| Maintainability | CONCERNS | Test coverage target 80%, comprehensive boundary tests required |

### Missing Considerations

1. **Boundary Assertion Guards** - Implementation should add explicit assertions at validation boundaries:
   ```python
   assert start_wave >= 1, "Wave number must be 1-based"
   ```

2. **Label Normalization** - `--dot-start-from` label lookup should handle special characters by building map with both original and normalized keys

3. **Mutual Exclusivity Enforcement** - Explicit check for `--dot-start-from` vs `--dot-start-wave`/`--dot-start-step` conflict needed early in command handler

### Test Recommendations

**Mandatory Boundary Tests (parameterized):**
- Wave boundaries: `[0, 1, N-1, N, N+1]` where N = total waves
- Step boundaries: `[0, 1, M-1, M, M+1]` where M = steps in wave
- Combined: `--dot-start-wave N --dot-start-step M` edge cases

**Integration Tests:**
- Resume from wave 2 with real DOT file (existing test fixture)
- Dry-run output verification with skipped items marked
- Label-based selection with special characters

**Error Path Tests:**
- Invalid wave number error message format
- Invalid step number error message format
- Unknown label error with available labels list
- Mutual exclusivity violation error

### Acceptance Criteria NFR Coverage

| AC | NFR Impact | Notes |
|----|------------|-------|
| AC-1 to AC-6 | Performance, Reliability | Skip logic must be O(1) and boundary-safe |
| AC-7 to AC-12 | Performance, Reliability | Step skip follows same pattern |
| AC-13 to AC-15 | Reliability | Combined mode increases boundary test surface |
| AC-16 to AC-18 | Security, Reliability | Label lookup must sanitize, handle missing labels |
| AC-19 to AC-20 | Maintainability | Documentation keeps code maintainable |

### Assessment File

NFR assessment: docs/qa/assessments/TEA-CLI-009-nfr-20260128.md

## QA Notes - Test Design

**Date:** 2026-01-28
**Designer:** Quinn (Test Architect)

### Test Coverage Matrix

| AC | Unit Tests | Integration Tests | E2E Tests | Total |
|----|------------|-------------------|-----------|-------|
| AC-1 (--dot-start-wave option) | 3 | 0 | 0 | 3 |
| AC-2 (Skip waves) | 0 | 2 | 0 | 2 |
| AC-3 (Skip message) | 2 | 0 | 0 | 2 |
| AC-4 (Wave bounds error) | 2 | 0 | 0 | 2 |
| AC-5 (Command/Workflow modes) | 0 | 2 | 0 | 2 |
| AC-6 (Dry-run skipped waves) | 1 | 0 | 0 | 1 |
| AC-7 (--dot-start-step option) | 2 | 0 | 0 | 2 |
| AC-8 (Skip steps) | 0 | 1 | 0 | 1 |
| AC-9 (Step skip message) | 1 | 0 | 0 | 1 |
| AC-10 (Step bounds error) | 2 | 0 | 0 | 2 |
| AC-11 (Implicit wave 1) | 1 | 0 | 0 | 1 |
| AC-12 (Step skip isolation) | 0 | 1 | 0 | 1 |
| AC-13 (Combined wave+step) | 0 | 1 | 0 | 1 |
| AC-14 (Summary accuracy) | 0 | 1 | 0 | 1 |
| AC-15 (Combined dry-run) | 1 | 0 | 0 | 1 |
| AC-16 (Label lookup) | 1 | 0 | 0 | 1 |
| AC-17 (Label execution) | 1 | 0 | 1 | 2 |
| AC-18 (Invalid label error) | 1 | 0 | 0 | 1 |
| AC-19/20 (Documentation) | 0 | 0 | 1 | 1 |
| **Mutual exclusivity** | 2 | 0 | 0 | 2 |
| **Totals** | **18** | **8** | **2** | **28** |

### Test Scenarios with Expected Results

#### Priority 0 (Must Pass) - 10 Tests

| ID | Scenario | Input | Expected Result |
|----|----------|-------|-----------------|
| CLI009-UNIT-001 | Valid minimum wave | `--dot-start-wave 1` | Accepted, no skip message |
| CLI009-UNIT-002 | Wave zero rejected | `--dot-start-wave 0` | Exit code 1, error "must be at least 1" |
| CLI009-UNIT-003 | Negative wave rejected | `--dot-start-wave -1` | Exit code 1, validation error |
| CLI009-UNIT-006 | Wave exceeds total | `--dot-start-wave 5` (3 waves) | Exit code 1, "has 3 waves" in error |
| CLI009-UNIT-008 | Valid minimum step | `--dot-start-step 1` | Accepted, no skip message |
| CLI009-UNIT-009 | Step zero rejected | `--dot-start-step 0` | Exit code 1, error "must be at least 1" |
| CLI009-UNIT-011 | Step exceeds count | `--dot-start-step 5` (3 steps) | Exit code 1, "has 3 steps" in error |
| CLI009-UNIT-013 | Mutual exclusivity | `--dot-start-from X --dot-start-wave 2` | Exit code 1, "mutually exclusive" |
| CLI009-UNIT-017 | Invalid label | `--dot-start-from "NonExistent"` | Exit code 1, lists available labels |
| CLI009-INT-007 | Combined skip | `--dot-start-wave 2 --dot-start-step 3` | Skips wave 1, skips steps 1-2 in wave 2 |

#### Priority 1 (Should Pass) - 12 Tests

| ID | Scenario | Expected Result |
|----|----------|-----------------|
| CLI009-INT-001 | Skip wave 1 | Only wave 2+ tmux windows created |
| CLI009-INT-002 | Skip waves 1-2 | Only wave 3 tmux windows created |
| CLI009-INT-003 | Command mode | Nodes with `command` attribute executed |
| CLI009-INT-004 | Workflow mode | `--dot-workflow` compatible |
| CLI009-INT-005 | Skip steps in wave | Steps 1-2 skipped in starting wave |
| CLI009-INT-006 | Step skip isolation | Wave 2+ executes all steps |
| CLI009-INT-008 | Summary accuracy | Executed count = total - skipped |
| CLI009-UNIT-004 | Skip message format | "Skipping waves 1-2, starting from wave 3" |
| CLI009-UNIT-010 | Step skip message | "Skipping steps 1-2 in wave 1" |
| CLI009-UNIT-015 | Label lookup | Returns (wave, step) tuple |
| CLI009-UNIT-016 | Mid-wave label | Correct start_step set |
| CLI009-E2E-001 | Full resume | Workflow resumes from label correctly |

### Test Data/Environment Requirements

**DOT Fixtures Required:**

1. **`multi_wave_workflow.dot`** - Primary fixture
   - 3 waves: Wave 1 (3 nodes), Wave 2 (2 nodes), Wave 3 (1 node)
   - Nodes have `command` attributes for Command Mode testing
   - Clear fan-out/fan-in structure

2. **`single_wave.dot`** - Edge case
   - 1 wave with 1 node
   - Tests boundary when only 1 wave exists

3. **`special_chars.dot`** - Label lookup
   - Nodes with spaces, dashes, underscores, dots in labels
   - Tests DATA-001 risk mitigation

**Environment:**

| Component | Requirement |
|-----------|-------------|
| Python | 3.10+ |
| tmux | Installed (mocked in unit tests) |
| Temp directory | Writable `/tmp` for fixtures |
| CliRunner | `typer.testing.CliRunner` |

**Mocking Strategy:**

- `subprocess.run` - Mock tmux commands in unit/integration tests
- `time.sleep` - Mock to speed up test execution
- `parse_dot` / `analyze_graph` - Real calls with fixture files

### Assessment File

Test design document: `docs/qa/assessments/TEA-CLI-009-test-design-20260128.md`

## QA Notes - Requirements Trace

**Date:** 2026-01-28
**Reviewer:** Quinn (Test Architect)
**Story Status:** Draft (Pre-Implementation)

### Requirements Coverage Summary

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total Requirements (ACs)** | 20 | 100% |
| **Fully Covered** | 0 | 0% |
| **Partially Covered** | 0 | 0% |
| **Not Covered (Planned)** | 20 | 100% |

**Note:** This story is in Draft status. No implementation or test code exists yet. The traceability matrix below documents the **planned** test mappings from the Test Design document.

### Traceability Matrix

#### Part A: Wave Selection (`--dot-start-wave`)

| AC | Requirement | Planned Test Coverage | Given-When-Then Mapping |
|----|-------------|----------------------|------------------------|
| **AC-1** | `--dot-start-wave <N>` accepts 1-based wave number | CLI009-UNIT-001, CLI009-UNIT-002, CLI009-UNIT-003 | **Given:** DOT file with 3 waves **When:** `--dot-start-wave 1` provided **Then:** Option accepted, no skip message |
| **AC-2** | Waves before `<N>` are skipped entirely | CLI009-INT-001, CLI009-INT-002 | **Given:** DOT file with 3 waves **When:** `--dot-start-wave 2` provided **Then:** Wave 1 skipped, no tmux windows for wave 1 nodes |
| **AC-3** | Summary message printed for skipped waves | CLI009-UNIT-004 | **Given:** DOT with 3 waves **When:** `--dot-start-wave 3` **Then:** Output contains "Skipping waves 1-2, starting from wave 3" |
| **AC-4** | Error if wave exceeds total count | CLI009-UNIT-006 | **Given:** DOT with 3 waves **When:** `--dot-start-wave 5` **Then:** Exit code 1, error "has 3 waves" |
| **AC-5** | Works with Command Mode and Workflow Mode | CLI009-INT-003, CLI009-INT-004 | **Given:** DOT with `command` attrs **When:** `--dot-start-wave 2` without `--dot-workflow` **Then:** Commands executed for wave 2+ nodes |
| **AC-6** | Dry-run shows skipped waves marked | CLI009-UNIT-005 | **Given:** DOT with 3 waves **When:** `--dot-start-wave 2 --dot-dry-run` **Then:** Output shows "Phase 1 (SKIPPED)" |

#### Part B: Step Selection (`--dot-start-step`)

| AC | Requirement | Planned Test Coverage | Given-When-Then Mapping |
|----|-------------|----------------------|------------------------|
| **AC-7** | `--dot-start-step <M>` accepts 1-based step number | CLI009-UNIT-008, CLI009-UNIT-009 | **Given:** DOT with 3-step wave **When:** `--dot-start-step 1` **Then:** Option accepted, no skip message |
| **AC-8** | Steps before `<M>` skipped in starting wave | CLI009-INT-005 | **Given:** Wave 1 has 3 steps **When:** `--dot-start-step 2` **Then:** Step 1 skipped, steps 2-3 execute |
| **AC-9** | Summary message for skipped steps | CLI009-UNIT-010 | **Given:** Wave 1 has 3 steps **When:** `--dot-start-step 3` **Then:** Output contains "Skipping steps 1-2 in wave 1" |
| **AC-10** | Error if step exceeds count in wave | CLI009-UNIT-011 | **Given:** Wave 1 has 3 steps **When:** `--dot-start-step 5` **Then:** Exit code 1, error "has 3 steps" |
| **AC-11** | `--dot-start-step` alone implies wave 1 | CLI009-UNIT-012 | **Given:** DOT with 3 waves **When:** `--dot-start-step 2` (no `--dot-start-wave`) **Then:** Starts from wave 1, step 2 |
| **AC-12** | Step skip only affects starting wave | CLI009-INT-006 | **Given:** `--dot-start-wave 1 --dot-start-step 2` **When:** Execution reaches wave 2 **Then:** All steps in wave 2 execute |

#### Part C: Combined Usage

| AC | Requirement | Planned Test Coverage | Given-When-Then Mapping |
|----|-------------|----------------------|------------------------|
| **AC-13** | Combined `--dot-start-wave 2 --dot-start-step 3` | CLI009-INT-007 | **Given:** DOT 3 waves, wave 2 has 4 steps **When:** `--dot-start-wave 2 --dot-start-step 3` **Then:** Wave 1 skipped, wave 2 steps 1-2 skipped |
| **AC-14** | Final summary reports only executed nodes | CLI009-INT-008 | **Given:** 6 total nodes, skip 3 **When:** Execution completes **Then:** Summary shows "3 nodes executed" |
| **AC-15** | Combined dry-run shows all skips | CLI009-UNIT-014 | **Given:** DOT 3 waves **When:** `--dot-start-wave 2 --dot-start-step 2 --dot-dry-run` **Then:** Output shows wave 1 skipped, wave 2 step 1 skipped |

#### Part D: Label-Based Selection

| AC | Requirement | Planned Test Coverage | Given-When-Then Mapping |
|----|-------------|----------------------|------------------------|
| **AC-16** | `--dot-start-from <label>` finds wave containing node | CLI009-UNIT-015 | **Given:** DOT with node "Process Data" in wave 2 **When:** `--dot-start-from "Process Data"` **Then:** Resolves to wave 2, step position |
| **AC-17** | Starts from specified node within wave | CLI009-UNIT-016, CLI009-E2E-001 | **Given:** "Process Data" is step 3 in wave 2 **When:** `--dot-start-from "Process Data"` **Then:** Wave 1 skipped, wave 2 steps 1-2 skipped |
| **AC-18** | Error with invalid label lists available | CLI009-UNIT-017 | **Given:** DOT with nodes A, B, C **When:** `--dot-start-from "NonExistent"` **Then:** Exit code 1, error lists "A, B, C" |

#### Part E: Documentation

| AC | Requirement | Planned Test Coverage | Given-When-Then Mapping |
|----|-------------|----------------------|------------------------|
| **AC-19** | LLM guide includes new options | CLI009-E2E-002 (manual) | **Given:** Implementation complete **When:** Checking DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md **Then:** Table includes `--dot-start-wave`, `--dot-start-step`, `--dot-start-from` |
| **AC-20** | Help text includes clear descriptions | CLI009-E2E-002 (manual) | **Given:** Implementation complete **When:** `tea run --help` **Then:** Output describes new options |

### Identified Coverage Gaps

| Gap ID | Requirement | Gap Description | Severity | Recommended Action |
|--------|-------------|-----------------|----------|-------------------|
| **GAP-001** | Cross-cutting | No tests exist - story is pre-implementation | High | Implement tests as part of story execution |
| **GAP-002** | AC-5 | Workflow Mode coverage depends on separate `--dot-workflow` option | Medium | Add CLI009-INT-004 test with `--dot-workflow` flag |
| **GAP-003** | TECH-001 Risk | Off-by-one boundary tests not exhaustive | High | Add parameterized tests for `[0, 1, N-1, N, N+1]` |
| **GAP-004** | DATA-001 Risk | Special character label tests not included in P0 | Medium | Add CLI009-UNIT-018 for labels with spaces/dashes |
| **GAP-005** | Mutual Exclusivity | Tests for `--dot-start-from` with both `--dot-start-wave` AND `--dot-start-step` | Medium | Add CLI009-UNIT-019 for triple-option conflict |

### Risk Mitigations via Testing

| Risk ID | Risk Description | Test Mitigation | Status |
|---------|-----------------|-----------------|--------|
| TECH-001 | Off-by-one indexing errors | CLI009-UNIT-001 to CLI009-UNIT-003, CLI009-UNIT-008, CLI009-UNIT-009 with boundary values | Planned |
| TECH-002 | Phase structure mismatch | CLI009-INT-003, CLI009-INT-004 validate both modes | Planned |
| TECH-003 | Missing mutual exclusivity | CLI009-UNIT-013 explicitly tests conflict | Planned |
| DATA-001 | Label lookup with special chars | CLI009-UNIT-018 (recommended addition) | Gap |
| TECH-004 | Dry-run unclear for skips | CLI009-UNIT-005, CLI009-UNIT-014 verify output format | Planned |

### Recommendations

1. **Pre-Implementation:** Create test fixtures (`multi_wave_workflow.dot`, `single_wave.dot`, `special_chars.dot`) before starting implementation
2. **TDD Approach:** Write CLI009-UNIT-001 through CLI009-UNIT-006 first as red tests, then implement AC-1 through AC-4
3. **Boundary Testing:** Use `@pytest.mark.parametrize` for wave/step boundary values to ensure TECH-001 mitigation
4. **Documentation Verification:** AC-19 and AC-20 should be verified manually as part of PR review checklist

### Conclusion

**Requirements Traceability Status:** COMPLETE (for Draft story)

All 20 acceptance criteria have been mapped to planned test scenarios from the Test Design document. No implementation code or tests exist yet as the story is in Draft status. The test design provides comprehensive coverage with 28 planned tests across unit, integration, and E2E levels.

**Gate Contribution:** This traceability analysis supports a **CONCERNS** gate decision due to:
- 5 identified gaps requiring attention during implementation
- TECH-001 (off-by-one) risk requires careful boundary test implementation
- No existing tests to validate - full test suite must be created

### Assessment File

Trace matrix: `docs/qa/assessments/TEA-CLI-009-trace-20260128.md` (this section)

## QA Results

### Review Date: 2026-01-28

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall Grade: EXCELLENT**

The implementation demonstrates high-quality engineering practices:

1. **TECH-001 Mitigation (Off-by-one)**: Consistently uses `enumerate(levels, 1)` throughout (`cli.py:785`, `cli.py:786`, `cli.py:857`, `cli.py:907`). The 1-based user-facing indexing is correctly maintained from validation through execution.

2. **Code Organization**: TEA-CLI-009 changes are well-organized with clear comment markers (e.g., `# TEA-CLI-009: Build label-to-location mapping`) making the code easy to navigate.

3. **Error Handling**: Clear, user-friendly error messages with context (wave counts, step counts, available labels). Uses `typer.Exit(1)` consistently.

4. **Validation Order**: Validates inputs before execution (fail-fast pattern):
   - Mutual exclusivity check (`cli.py:796-804`)
   - Label resolution (`cli.py:806-822`)
   - Wave bounds validation (`cli.py:824-831`)
   - Step bounds validation (`cli.py:833-841`)

### Refactoring Performed

No refactoring needed - the implementation is clean and well-structured.

### Compliance Check

- Coding Standards: ✓ Follows existing CLI patterns, proper type hints
- Project Structure: ✓ Tests in correct location with proper fixtures
- Testing Strategy: ✓ 30 tests covering all ACs, boundaries, and edge cases
- All ACs Met: ✓ All 20 acceptance criteria verified

### Improvements Checklist

All items handled by implementation:

- [x] `--dot-start-wave` option with validation (AC-1 to AC-6)
- [x] `--dot-start-step` option with validation (AC-7 to AC-12)
- [x] Combined wave/step selection (AC-13 to AC-15)
- [x] `--dot-start-from` label-based selection with mutual exclusivity (AC-16 to AC-18)
- [x] Documentation updates in LLM guide and CLI reference (AC-19, AC-20)
- [x] Test fixtures created (`multi_wave_workflow.dot`, `single_wave.dot`, `special_chars.dot`)
- [x] Boundary tests for wave/step values
- [x] TECH-001 mitigation via consistent `enumerate(levels, 1)` usage

### Security Review

**Status: PASS**

- Input validation present (min=1 constraint on typer options)
- Label lookup uses dictionary (no shell interpolation risk)
- Error messages expose only operational data (wave counts, labels)

### Performance Considerations

**Status: PASS**

- Skip logic is O(1) index comparison (`if phase_idx < start_wave: continue`)
- Label map built once after levels computed (O(n) build, O(1) lookup)
- No memory overhead for skipped phases

### Test Coverage Analysis

**30 tests, 100% AC coverage:**

| Test Class | Count | Coverage |
|------------|-------|----------|
| TestDotStartWaveOption | 7 | AC-1 to AC-6 |
| TestDotStartStepOption | 6 | AC-7 to AC-12 |
| TestCombinedWaveStep | 2 | AC-13 to AC-15 |
| TestDotStartFromOption | 6 | AC-16 to AC-18 |
| TestEdgeCases | 4 | Boundary conditions |
| TestIntegrationDryRun | 2 | Integration validation |
| TestSummaryOutput | 1 | Output format |
| TestModeCompatibility | 2 | AC-5 (Command/Workflow modes) |

**Risk Mitigations Verified:**

| Risk ID | Mitigation | Test Evidence |
|---------|------------|---------------|
| TECH-001 | `enumerate(levels, 1)` | Boundary tests for 0, 1, N, N+1 all pass |
| TECH-002 | Uses `levels` from Kahn's algorithm | Integration tests with real DOT files |
| TECH-003 | Mutual exclusivity check | 2 tests verify error on conflicts |
| DATA-001 | Label map with spaces/special chars | `special_chars.dot` test passes |
| TECH-004 | `[SKIPPED]` markers in dry-run | Multiple tests verify output format |

### Files Modified During Review

None - implementation is complete and well-structured.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-CLI-009-dot-wave-step-selection.yml
Risk profile: docs/qa/assessments/TEA-CLI-009-risk-20260128.md
NFR assessment: docs/qa/assessments/TEA-CLI-009-nfr-20260128.md
Test design: docs/qa/assessments/TEA-CLI-009-test-design-20260128.md

### Recommended Status

**✓ Ready for Done**

All acceptance criteria met, all tests passing (30/30), documentation updated, risk mitigations implemented. The implementation properly addresses all identified risks from the pre-implementation risk profile.

---

## SM Validation

**Date:** 2026-01-28
**Scrum Master:** Bob
**Validation Result:** ✅ READY FOR DEVELOPMENT

### Definition of Ready Checklist

| Criterion | Status | Notes |
|-----------|--------|-------|
| Story has clear title and description | ✅ PASS | Clear user story format with problem statement |
| Acceptance criteria are defined and testable | ✅ PASS | 20 ACs across 5 parts, all testable |
| Dependencies are identified | ✅ PASS | Integrates with existing `--dot-*` options |
| Technical approach is documented | ✅ PASS | Architect v1.4 correctly uses `levels` from Kahn's algorithm |
| Story is properly sized | ✅ PASS | 6 tasks with clear subtasks |
| QA Notes - Risk Profile | ✅ PASS | MEDIUM risk (71/100), TECH-002 mitigated |
| QA Notes - NFR Assessment | ✅ PASS | Quality Score 80/100, 4 NFRs assessed |
| QA Notes - Test Design | ✅ PASS | 28 tests planned with priority matrix |
| QA Notes - Requirements Trace | ✅ PASS | All 20 ACs traced to test scenarios |
| No blocking issues or unknowns | ✅ PASS | TECH-002 architectural mismatch resolved |

### Story Draft Checklist Results

| Category | Status | Issues |
|----------|--------|--------|
| 1. Goal & Context Clarity | PASS | None |
| 2. Technical Implementation Guidance | PASS | v1.4 corrections in place |
| 3. Reference Effectiveness | PASS | None |
| 4. Self-Containment Assessment | PASS | None |
| 5. Testing Guidance | PASS | None |

**Clarity Score:** 10/10

### Assessment Summary

This story is exceptionally well-prepared for development:

1. **Comprehensive ACs:** 20 acceptance criteria organized into 5 logical parts (Wave Selection, Step Selection, Combined Usage, Label-Based Selection, Documentation)

2. **Rich Technical Guidance:**
   - Key files identified with line numbers (`cli.py:700-900` for `--from-dot` execution)
   - `levels: List[List[str]]` data structure from Kahn's algorithm documented
   - Implementation sketches provided for wave/step skip logic using actual code patterns
   - Error message formats specified using `typer.BadParameter`

3. **Thorough QA Documentation:**
   - Risk Profile: 7 risks identified with mitigations (TECH-001 off-by-one is high priority, TECH-002 mitigated)
   - NFR Assessment: Security, Performance, Reliability, Maintainability all assessed
   - Test Design: 28 planned tests across unit/integration/E2E
   - Requirements Trace: Complete matrix mapping all 20 ACs to test scenarios

4. **Self-Contained:** Terminology explained, edge cases documented, mutual exclusivity constraint specified

### Recommendations for Development

1. **Prioritize TECH-001 mitigation:** Use `enumerate(items, 1)` consistently to prevent off-by-one errors
2. **Create test fixtures first:** `multi_wave_workflow.dot`, `single_wave.dot`, `special_chars.dot`
3. **Follow TDD approach:** Write boundary tests before implementing skip logic
4. **Follow v1.4 corrections:** Use `levels` list from Kahn's algorithm, NOT `PhaseInfo`

**Final Decision:** ✅ APPROVED - Story meets all Definition of Ready criteria and is ready for development.

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
No debug issues encountered.

### Completion Notes
- All 6 tasks completed successfully
- 30 unit and integration tests created and passing
- Documentation updated in two files
- Used `enumerate(levels, 1)` consistently for 1-based indexing (TECH-001 mitigation)
- Label map built after levels computed for `--dot-start-from` resolution
- Mutual exclusivity check added for `--dot-start-from` vs numeric options
- Error messages include context (available labels, total wave/step counts)

### File List

**New Files:**
- `python/tests/fixtures/multi_wave_workflow.dot` - Test fixture with 3 waves (3+2+1 nodes)
- `python/tests/fixtures/single_wave.dot` - Test fixture with 1 wave, 1 node
- `python/tests/fixtures/special_chars.dot` - Test fixture with labels containing spaces, dashes, underscores, dots
- `python/tests/test_cli_dot_wave_step.py` - 30 unit and integration tests for TEA-CLI-009

**Modified Files:**
- `python/src/the_edge_agent/cli.py` - Added `--dot-start-wave`, `--dot-start-step`, `--dot-start-from` options with validation and skip logic
- `docs/llm-prompts/DOT_WORKFLOW_ORCHESTRATION_LLM_GUIDE.md` - Updated command reference table and added "Resuming Failed Workflows" section
- `docs/shared/cli-reference.md` - Added wave/step selection options to DOT File Execution section
- `docs/stories/TEA-CLI-009.dot-wave-step-selection.md` - Updated status, tasks, and dev agent record
