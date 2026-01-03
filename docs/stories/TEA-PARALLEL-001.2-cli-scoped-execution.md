# Story: TEA-PARALLEL-001.2 - CLI Scoped Execution

## Status: Ready for Development ✅

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

- [ ] `--entry-point` flag implemented in CLI
- [ ] `--exit-point` flag implemented in CLI
- [ ] `--input` flag loads state from JSON
- [ ] `--output` flag writes state to JSON
- [ ] `execute_scoped()` method in StateGraph
- [ ] `_path_exists()` helper for validation
- [ ] All validation errors have clear messages
- [ ] All 15 test scenarios pass
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
