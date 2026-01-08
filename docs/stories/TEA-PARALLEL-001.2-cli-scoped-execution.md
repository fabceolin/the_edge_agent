# Story: TEA-PARALLEL-001.2 - CLI Scoped Execution

## Status: Done

**Epic**: [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](./TEA-PARALLEL-001-multi-strategy-execution-epic.md)
**Estimated Tests**: 22 scenarios (updated from QA test design)
**Dependencies**: [TEA-PARALLEL-001.1](./TEA-PARALLEL-001.1-executor-abstraction.md) (Executor Abstraction)

---

## User Story

**As a** workflow developer,
**I want** CLI flags to execute a portion of a workflow graph,
**So that** remote executors can run specific branches without full workflow execution.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | `--entry-point <node>` starts execution at specified node instead of `__start__` | Unit test: CLI parsing, Integration: execution starts at node |
| AC2 | `--exit-point <node>` stops execution BEFORE specified node (not executed) | Integration test: node NOT executed |
| AC3 | `--input <file.json>` loads initial state from JSON file | Integration test: state loaded correctly |
| AC4 | `--output <file.json>` writes final state to JSON file | Integration test: state written correctly |
| AC5 | `execute_scoped()` method in StateGraph for programmatic scoped execution | Unit test: method exists and works |
| AC6 | Validation: entry-point node must exist in graph | Unit test: raises ValueError |
| AC7 | Validation: exit-point node must exist in graph | Unit test: raises ValueError |
| AC8 | Validation: path must exist from entry-point to exit-point | Unit test: raises ValueError |
| AC9 | Linear subgraph execution (chains of nodes) supported | E2E test: chain executes correctly |

---

## Technical Design

### Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/cli.py` | Modify | Add --entry-point, --exit-point, --input, --output flags |
| `python/src/the_edge_agent/stategraph.py` | Modify | Add `execute_scoped()` method |

### CLI Flags

```bash
tea run <workflow.yaml> [OPTIONS]

Execution Scope Options:
  --entry-point <node>    Start execution at this node instead of __start__
  --exit-point <node>     Stop execution BEFORE this node (don't execute it)
  --input <file.json>     Load initial state from JSON file
  --output <file.json>    Write final state to JSON file

Examples:
  # Normal execution
  tea run workflow.yaml

  # Scoped execution (for remote parallel branches)
  tea run workflow.yaml --entry-point analyze --exit-point merge --input state.json --output result.json

  # Execute a single node
  tea run workflow.yaml --entry-point process_data --exit-point next_step --input in.json --output out.json
```

### CLI Implementation

```python
# cli.py
import typer
import json
from pathlib import Path

app = typer.Typer()

@app.command()
def run(
    workflow: Path,
    entry_point: str = typer.Option(None, "--entry-point", help="Start at this node"),
    exit_point: str = typer.Option(None, "--exit-point", help="Stop before this node"),
    input_file: Path = typer.Option(None, "--input", help="Load state from JSON"),
    output_file: Path = typer.Option(None, "--output", help="Write state to JSON"),
):
    """Run a workflow with optional scoped execution."""
    # Load workflow
    engine = YAMLEngine.from_file(workflow)
    graph = engine.compile()

    # Load initial state
    if input_file:
        with open(input_file) as f:
            initial_state = json.load(f)
    else:
        initial_state = {}

    # Execute (scoped or full)
    if entry_point or exit_point:
        # Scoped execution
        result = graph.execute_scoped(
            initial_state=initial_state,
            entry_point=entry_point or "__start__",
            exit_point=exit_point or "__end__"
        )
    else:
        # Normal execution
        result = graph.invoke(initial_state)

    # Write output state
    if output_file:
        with open(output_file, "w") as f:
            json.dump(result, f, indent=2)

    return result
```

### StateGraph.execute_scoped()

```python
# stategraph.py
def execute_scoped(
    self,
    initial_state: Dict[str, Any],
    entry_point: str,
    exit_point: str
) -> Dict[str, Any]:
    """
    Execute a portion of the graph from entry_point to exit_point.

    Args:
        initial_state: Starting state (loaded from --input)
        entry_point: Node to start execution (instead of __start__)
        exit_point: Node to stop BEFORE (not executed)

    Returns:
        Final state (written to --output)

    Raises:
        ValueError: If entry_point or exit_point don't exist, or no path between them
    """
    # Validate entry point
    if entry_point != "__start__" and entry_point not in self.nodes:
        raise ValueError(f"Entry point '{entry_point}' not found in graph")

    # Validate exit point
    if exit_point != "__end__" and exit_point not in self.nodes:
        raise ValueError(f"Exit point '{exit_point}' not found in graph")

    # Validate path exists
    if not self._path_exists(entry_point, exit_point):
        raise ValueError(
            f"No execution path from '{entry_point}' to '{exit_point}'"
        )

    # Execute with modified boundaries
    return self._run_graph(
        state=initial_state,
        start_node=entry_point,
        stop_before=exit_point
    )


def _path_exists(self, start: str, end: str) -> bool:
    """Check if there's a path from start to end using BFS."""
    if start == end:
        return False  # Can't have same entry and exit

    visited = set()
    queue = [start]

    while queue:
        current = queue.pop(0)
        if current == end:
            return True
        if current in visited:
            continue
        visited.add(current)

        # Get next nodes from edges
        for edge in self.edges:
            if edge.source == current:
                targets = edge.targets if isinstance(edge.targets, list) else [edge.targets]
                queue.extend(targets)

    return False


def _run_graph(
    self,
    state: Dict[str, Any],
    start_node: str,
    stop_before: str
) -> Dict[str, Any]:
    """Execute graph from start_node, stopping before stop_before."""
    current = start_node

    while current != stop_before and current != "__end__":
        # Execute current node
        node = self.nodes[current]
        state = node.run(state)

        # Get next node
        current = self._get_next_node(current, state)

    return state
```

### Linear Subgraph Example

```yaml
# Original workflow
name: multi-branch-analysis

edges:
  - from: prepare
    to: [branch_a, branch_b]
    parallel: true
    parallel_strategy: remote
    fan_in: merge

  # Branch A: Linear chain
  - from: branch_a
    to: step_a1
  - from: step_a1
    to: step_a2

  # Branch B: Single node
  - from: branch_b
    to: merge  # Direct to fan-in

  - from: merge
    to: __end__
```

**Scoped execution for Branch A:**
```bash
tea run workflow.yaml \
  --entry-point branch_a \
  --exit-point merge \
  --input state.json \
  --output result.json

# Executes: branch_a → step_a1 → step_a2 → (stops before merge)
```

---

## Testing

### Test Location

- `python/tests/test_cli_scope_flags.py` - CLI flag parsing
- `python/tests/test_scoped_execution.py` - StateGraph.execute_scoped()

### Test Scenarios (15 total)

#### AC1-2: Entry/Exit Point Flags (7 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-UNIT-017 | Unit | P0 | `--entry-point` flag parsed correctly |
| 001.2-UNIT-018 | Unit | P0 | `--exit-point` flag parsed correctly |
| 001.2-UNIT-019 | Unit | P0 | Validation: entry-point node must exist |
| 001.2-UNIT-020 | Unit | P0 | Validation: exit-point node must exist |
| 001.2-UNIT-021 | Unit | P0 | Validation: path must exist entry→exit |
| 001.2-INT-024 | Integration | P0 | Execution starts at entry-point |
| 001.2-INT-025 | Integration | P0 | **Critical**: Stops BEFORE exit-point |

#### AC3-4: State I/O Flags (5 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-UNIT-022 | Unit | P1 | `--input` flag parsed correctly |
| 001.2-UNIT-023 | Unit | P1 | `--output` flag parsed correctly |
| 001.2-INT-027 | Integration | P0 | State loaded from `--input` JSON |
| 001.2-INT-028 | Integration | P0 | State written to `--output` JSON |
| 001.2-INT-029 | Integration | P1 | Invalid JSON raises clear error |

#### AC5: execute_scoped() Method (1 test)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-INT-026 | Integration | P1 | `execute_scoped()` works programmatically |

#### AC9: Linear Subgraph (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.2-E2E-004 | E2E | P1 | Linear chain executes correctly |
| 001.2-INT-030 | Integration | P2 | Missing `--input` with `--entry-point` warns |

---

## Definition of Done

- [x] `--entry-point` flag implemented in CLI
- [x] `--exit-point` flag implemented in CLI
- [x] `--input` flag loads state from JSON
- [x] `--output` flag writes state to JSON
- [x] `execute_scoped()` method in StateGraph
- [x] `_path_exists()` helper for validation
- [x] All validation errors have clear messages
- [x] All 41 test scenarios pass (exceeds 22 estimate)
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Invalid scope causes runtime error | High | Pre-execution validation |
| JSON serialization fails | Medium | Validate state before writing |
| Path detection misses conditional edges | Medium | BFS traversal covers all edges |

---

## Notes for Developer

1. **Exit means BEFORE**: The `--exit-point` node is NOT executed. This is critical for remote execution where the fan-in node runs on the main host.

2. **Path validation**: Use BFS to check reachability. Consider conditional edges (all branches should lead to exit eventually).

3. **Empty state warning**: If `--entry-point` is used without `--input`, emit a warning that state will be empty.

4. **Test fixtures**: Create a fixture graph with linear chains for testing:
   ```
   a → b → c → d → e
   ```

---

## QA Notes

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 22 |
| Unit Tests | 11 (50%) |
| Integration Tests | 9 (41%) |
| E2E Tests | 2 (9%) |
| P0 (Critical) | 12 |
| P1 (High) | 8 |
| P2 (Medium) | 2 |

### Risk Areas Identified

1. **Exit-Point Semantic (HIGH)**: The `--exit-point` node must NOT be executed. This is the core semantic for remote parallel branch execution. Incorrect behavior would break the entire remote execution model where fan-in nodes run on the main host only.

2. **State Serialization (HIGH)**: Invalid or non-serializable state would break remote execution. State must round-trip correctly through JSON for distributed workflows.

3. **Path Validation (MEDIUM)**: BFS-based path detection must handle conditional and parallel edges correctly, or invalid scopes could be accepted.

4. **Empty State Warning (MEDIUM)**: Using `--entry-point` without `--input` results in empty state, which may be a user error but shouldn't fail silently.

### Recommended Test Scenarios

**Critical Path (P0 - Must Pass)**:
- `001.2-INT-003`: Exit-point node is NOT executed
- `001.2-INT-004`: Final state returned is from node BEFORE exit-point
- `001.2-INT-001`: Execution starts at entry-point node
- `001.2-INT-006/007`: State correctly loaded from JSON with nested structures
- `001.2-INT-010/011`: Final state correctly written as valid JSON
- `001.2-E2E-001`: Linear chain `a → b → c → d` executes correctly with scope

**Edge Cases (P1)**:
- `001.2-UNIT-014/016`: Path detection with conditional/parallel edges
- `001.2-UNIT-015`: Same entry and exit raises ValueError
- `001.2-INT-008`: Invalid JSON file raises clear error message

### Concerns and Blockers

**None blocking** - Story is ready for development.

**Advisory Notes**:
1. Exit-point behavior has defense-in-depth coverage (two tests) due to its criticality
2. Test fixture graph `a → b → c → d → e` should be created as specified in developer notes
3. Recommend running P0 unit tests first (fail-fast on validation logic)

---

## QA Results

### Test Design Review: 2026-01-01

**Reviewer**: Quinn (Test Architect)

**Test Design Document**: [`docs/qa/assessments/TEA-PARALLEL-001.2-test-design-20260101.md`](../qa/assessments/TEA-PARALLEL-001.2-test-design-20260101.md)

#### Summary

| Metric | Story Estimate | QA Design | Delta |
|--------|----------------|-----------|-------|
| Total Scenarios | 15 | 22 | +7 |
| Unit Tests | - | 11 (50%) | - |
| Integration Tests | - | 9 (41%) | - |
| E2E Tests | - | 2 (9%) | - |

#### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 12 | Critical - must pass for merge |
| P1 | 8 | High - edge cases and robustness |
| P2 | 2 | Medium - nice-to-have coverage |

#### Key Findings

1. **Critical Test Identified**: `001.2-INT-003` - Exit-point node is NOT executed. This is the core semantic differentiator for scoped execution.

2. **Defense-in-Depth**: Exit-point behavior has two tests (INT-003 + INT-004) because incorrect behavior would break remote parallel execution.

3. **Additional Scenarios**: Added 7 scenarios beyond story estimate to cover:
   - Special characters in node names
   - Explicit `__start__`/`__end__` edge cases
   - Conditional and parallel edge path detection
   - Same entry/exit validation
   - Non-serializable state error handling

4. **Test Level Distribution**: Shifted toward unit tests (50%) for validation logic - appropriate given the story's focus on input validation and path checking.

#### AC Coverage Verification

| AC | Tests | Status |
|----|-------|--------|
| AC1 | 5 | ✅ Covered |
| AC2 | 5 | ✅ Covered |
| AC3 | 5 | ✅ Covered |
| AC4 | 4 | ✅ Covered |
| AC5 | 3 | ✅ Covered |
| AC6 | 2 | ✅ Covered |
| AC7 | 2 | ✅ Covered |
| AC8 | 4 | ✅ Covered |
| AC9 | 2 | ✅ Covered |

#### Recommendations

1. **Update Story Test Estimate**: Change from 15 to 22 scenarios
2. **Test File Structure**: Confirm split between `test_cli_scoped_execution.py` and `test_scoped_execution.py`
3. **Fixture Graph**: Developer should create the `a → b → c → d → e` fixture as noted

**Status**: ✅ Test Design Complete - Ready for Development

---

### Implementation Review: 2026-01-06

**Reviewed By**: Quinn (Test Architect)

#### Code Quality Assessment

The implementation is **excellent**. The code is well-structured, properly documented, and follows project coding standards. The developer exceeded the estimated test count (41 vs 22) and implemented comprehensive validation with clear error messages.

**Strengths:**
- Clean separation: `execute_scoped()` for programmatic use, CLI integration for user interface
- Defensive programming: Entry/exit validation before execution, path validation via BFS
- Critical semantic preserved: Exit-point node is NOT executed (essential for remote parallel branches)
- Error handling: Both `raise_exceptions=True` and `False` modes properly handle scoped execution errors
- State immutability: `initial_state.copy()` at execution start prevents mutation issues

#### Requirements Traceability

| AC | Given-When-Then | Test Coverage | Status |
|----|-----------------|---------------|--------|
| AC1 | Given workflow with nodes a→b→c→d, When `--entry-point b`, Then execution starts at node b | 3 tests (UNIT-017, INT-024, programmatic) | ✅ |
| AC2 | Given `--exit-point c`, When executing, Then node c is NOT executed | 3 tests (UNIT-018, INT-025, INT-004) | ✅ |
| AC3 | Given `--input @state.json`, When executing, Then state loaded from file | 4 tests (UNIT-022, INT-027, INT-006/007) | ✅ |
| AC4 | Given `--output result.json`, When execution completes, Then state written to file | 3 tests (UNIT-023, INT-010/011, normal mode) | ✅ |
| AC5 | Given StateGraph instance, When calling `execute_scoped()`, Then works programmatically | 6 tests in TestExecuteScoped | ✅ |
| AC6 | Given nonexistent entry-point, When validating, Then raises clear ValueError | 2 tests (UNIT-019, special chars) | ✅ |
| AC7 | Given nonexistent exit-point, When validating, Then raises clear ValueError | 2 tests (UNIT-020, special chars) | ✅ |
| AC8 | Given no path entry→exit, When validating, Then raises clear ValueError | 2 tests (UNIT-021, same node) | ✅ |
| AC9 | Given linear chain a→b→c→d→e, When scoped execution, Then chain executes correctly | 2 E2E tests (full, partial) | ✅ |

#### Test Architecture Assessment

| Metric | Actual | Design Estimate | Status |
|--------|--------|-----------------|--------|
| Total Tests | 41 | 22 | ✅ +86% |
| Unit Tests | 23 (56%) | 11 (50%) | ✅ |
| Integration/E2E | 18 (44%) | 11 (50%) | ✅ |
| P0 Coverage | 12/12 | 12 | ✅ 100% |

**Test Level Appropriateness:**
- Unit tests (`test_scoped_execution.py`): Validation logic, path detection, error handling
- Integration tests (`test_cli_scoped_execution.py`): CLI flag parsing, file I/O, E2E workflows

#### Compliance Check

- Coding Standards: ✅ Type hints, Google-style docstrings, snake_case naming
- Project Structure: ✅ Tests in `python/tests/`, implementation in `python/src/`
- Testing Strategy: ✅ unittest + parameterized, proper fixture setup/teardown
- All ACs Met: ✅ All 9 acceptance criteria verified with tests

#### Security Review

- Input validation: ✅ Entry/exit points validated before execution
- JSON parsing: ✅ Uses `json.loads()` with proper error handling
- File I/O: ✅ Uses `typer.Exit(1)` on file errors, no path traversal risks
- No security concerns identified

#### Performance Considerations

- BFS path validation is O(V+E) - appropriate for graph sizes
- Deep copy not used in scoped execution (only `.copy()`) - suitable for linear chains
- No performance concerns for intended use case (remote parallel branches)

#### Improvements Checklist

All items completed by developer:
- [x] `execute_scoped()` method with full validation
- [x] `_path_exists()` BFS helper method
- [x] `_run_scoped()` simplified execution loop
- [x] CLI flags `--entry-point`, `--exit-point`, `--output`
- [x] Warning when `--entry-point` without `--input`
- [x] Error propagation via `_scoped_error` state key

Advisory (future enhancement):
- [ ] Consider adding `--timeout` flag for scoped execution (not required for this story)

#### Files Modified During Review

None - implementation is complete and correct.

#### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-PARALLEL-001.2-cli-scoped-execution.yml

#### Recommended Status

✅ **Ready for Done** - All acceptance criteria met, tests pass, code quality excellent.

---

### Story Draft Checklist: 2026-01-01

**Reviewer**: Bob (Scrum Master)

| Category | Status |
|----------|--------|
| 1. Goal & Context Clarity | ✅ PASS (5/5) |
| 2. Technical Implementation Guidance | ✅ PASS (5/5) |
| 3. Reference Effectiveness | ✅ PASS (4/4) |
| 4. Self-Containment Assessment | ✅ PASS (4/4) |
| 5. Testing Guidance | ✅ PASS (4/4) |

**Final Assessment**: ✅ **READY** - Clarity Score: 9/10

**Notes**:
- Story is fully self-contained with embedded code examples
- All 9 acceptance criteria are testable
- Updated test estimate from 15 to 22 scenarios based on QA test design
- No blocking issues identified

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.0 | Story extracted from epic | Sarah (PO) |
| 2026-01-01 | 1.1 | Test design review completed (22 scenarios) | Quinn (QA) |
| 2026-01-01 | 1.2 | Story draft checklist passed - Ready for Development | Bob (SM) |
| 2026-01-06 | 2.0 | Implementation completed - 41 tests pass | James (Dev) |

---

## Dev Agent Record

### Agent Model Used
- Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/stategraph.py` | Modified | Added `execute_scoped()`, `_path_exists()`, `_run_scoped()` methods |
| `python/src/the_edge_agent/cli.py` | Modified | Added `--entry-point`, `--exit-point`, `--output` flags to `run` command |
| `python/tests/test_scoped_execution.py` | Created | 23 unit tests for `execute_scoped()` and `_path_exists()` |
| `python/tests/test_cli_scoped_execution.py` | Created | 18 integration/E2E tests for CLI flags |

### Debug Log References
- None - all tests passed on first run

### Completion Notes

1. **Implementation Summary**:
   - Added `execute_scoped()` method to StateGraph class (lines 1956-2025)
   - Added `_path_exists()` helper for BFS-based path validation (lines 2027-2061)
   - Added `_run_scoped()` for simplified scoped execution (lines 2063-2150)
   - Added `--entry-point`, `--exit-point`, `--output` flags to CLI run command (lines 461-476)
   - Added scoped execution handling in CLI (lines 616-677)
   - Added `--output` support for normal and stream execution modes

2. **Test Results**:
   - 41 total tests pass (23 unit + 18 integration/E2E)
   - Exceeds story estimate of 22 scenarios
   - All P0 critical tests pass
   - Full test suite: 3174 passed, 1 unrelated failure (OpenAI API issue)

3. **Key Design Decisions**:
   - Exit-point node is NOT executed (critical semantic for remote parallel branches)
   - Path validation uses BFS to handle conditional and parallel edges
   - `_scoped_error` key added to state for error reporting when `raise_exceptions=False`
   - Warning emitted when `--entry-point` used without `--input`

4. **Acceptance Criteria Verification**:
   - AC1: `--entry-point` flag implemented and tested
   - AC2: `--exit-point` flag implemented - node NOT executed (critical)
   - AC3: `--input` loads state from JSON file or inline JSON
   - AC4: `--output` writes final state to JSON file
   - AC5: `execute_scoped()` method works programmatically
   - AC6: Entry-point validation raises clear ValueError
   - AC7: Exit-point validation raises clear ValueError
   - AC8: Path validation raises clear ValueError
   - AC9: Linear subgraph execution tested with fixture graph a→b→c→d→e

### Story DoD Checklist

1. **Requirements Met:**
   - [x] All functional requirements specified in the story are implemented.
   - [x] All acceptance criteria defined in the story are met.

2. **Coding Standards & Project Structure:**
   - [x] All new/modified code strictly adheres to `Operational Guidelines`.
   - [x] All new/modified code aligns with `Project Structure` (file locations, naming, etc.).
   - [x] Adherence to `Tech Stack` for technologies/versions used.
   - [N/A] Adherence to `Api Reference` and `Data Models` - no API changes.
   - [x] Basic security best practices applied (input validation, error handling).
   - [x] No new linter errors or warnings introduced.
   - [x] Code is well-commented where necessary.

3. **Testing:**
   - [x] All required unit tests implemented (23 unit tests).
   - [x] All required integration tests implemented (18 integration/E2E tests).
   - [x] All 41 tests pass successfully.
   - [x] Test coverage exceeds story estimate of 22 scenarios.

4. **Functionality & Verification:**
   - [x] Functionality manually verified via `tea run` command.
   - [x] Edge cases handled (nonexistent nodes, invalid paths, same entry/exit).

5. **Story Administration:**
   - [x] All DoD checkboxes marked complete.
   - [x] Dev Agent Record section completed with implementation notes.
   - [x] Change Log updated with implementation completion entry.

6. **Dependencies, Build & Configuration:**
   - [x] Project builds successfully without errors.
   - [N/A] No new dependencies added.
   - [N/A] No new environment variables introduced.

7. **Documentation:**
   - [x] Inline code documentation complete (docstrings for new methods).
   - [N/A] No user-facing documentation changes needed.
   - [N/A] No architectural changes requiring diagram updates.

**Final Confirmation:**
- [x] I, the Developer Agent, confirm that all applicable items above have been addressed.
