# TEA-CLI-006: Show Graph Progress During Execution

## Status
Ready for Review

## Story

**As a** workflow developer,
**I want** to see an ASCII graph visualization during `tea run` execution showing which node is currently running,
**so that** I can better understand workflow progress and structure at a glance.

## Acceptance Criteria

1. **AC-1:** `--show-graph` flag added to `tea run` command
2. **AC-2:** ASCII graph displays workflow structure before execution starts
3. **AC-3:** Current executing node is highlighted with a marker (e.g., `>>>` or `*`)
4. **AC-4:** Completed nodes are marked with `✓` prefix
5. **AC-5:** Graph refreshes/updates as execution progresses through nodes
6. **AC-6:** Works with both sequential and parallel (fan-out/fan-in) workflows
7. **AC-7:** Parallel branches rendered side-by-side (horizontal layout)
8. **AC-8:** Flag is mutually exclusive with `--stream` (NDJSON mode)
9. **AC-9:** Flag is compatible with `--quiet` (suppresses other output, shows graph only)
10. **AC-10:** No state data shown - purely structural visualization
11. **AC-11:** Assumes standard terminal width (80+ characters)

## Out of Scope

- Dynamic terminal width detection/wrapping
- State value display at each step
- Interactive graph manipulation
- Rust implementation (Python-only for initial release)

## Tasks / Subtasks

- [x] **Task 1: Add CLI flag** (AC: 1, 8, 9)
  - [x] Add `--show-graph` / `-g` boolean option to `run` command in `cli.py`
  - [x] Validate mutual exclusivity with `--stream`
  - [x] When combined with `--quiet`, suppress node completion messages but show graph

- [x] **Task 2: Implement ASCII graph renderer** (AC: 2, 6, 7, 10, 11)
  - [x] Create `graph_renderer.py` module in `python/src/the_edge_agent/`
  - [x] Parse compiled graph structure (nodes, edges) into renderable format
  - [x] Generate ASCII layout with vertical flow, horizontal parallel branches
  - [x] Handle `__start__` and `__end__` special nodes
  - [x] Handle `dynamic_parallel` nodes with fan-in visualization

- [x] **Task 3: Implement progress state tracking** (AC: 3, 4, 5)
  - [x] Create `GraphProgressTracker` class to manage node states
  - [x] States: `pending`, `running`, `completed`
  - [x] Method to render current state as ASCII string

- [x] **Task 4: Integrate with CLI execution loop** (AC: 3, 4, 5)
  - [x] Hook into `stream()` event loop in `cli.py`
  - [x] On `state` event: mark previous node complete, mark current node running
  - [x] Clear and redraw graph on each state change (using ANSI escape codes)
  - [x] On `final` event: mark all nodes complete

- [x] **Task 5: Testing** (AC: all)
  - [x] Unit tests for graph renderer with linear topology
  - [x] Unit tests for parallel branch rendering
  - [x] Unit tests for progress state transitions
  - [x] Test `--show-graph --quiet` combination shows only graph output
  - [x] Test non-TTY fallback produces simple text output
  - [x] Integration test with example workflow file

## Dev Notes

### Files to Create/Modify

| File | Action | Purpose |
|------|--------|---------|
| `python/src/the_edge_agent/cli.py` | Modify | Add `--show-graph` flag, integrate renderer |
| `python/src/the_edge_agent/graph_renderer.py` | Create | ASCII graph rendering logic |
| `python/tests/test_graph_renderer.py` | Create | Unit tests |

### ASCII Graph Example Output

**Linear flow:**
```
┌─────────┐
│ __start__│
└────┬────┘
     │
     ▼
┌─────────┐
│  setup  │ ✓
└────┬────┘
     │
     ▼
┌─────────────────────┐
│ foundation_parallel │ * running
└─────────┬───────────┘
          │
          ▼
┌─────────────────────┐
│  foundation_collect │
└─────────┬───────────┘
          │
          ▼
┌─────────┐
│ __end__ │
└─────────┘
```

**Parallel branches (horizontal side-by-side):**
```
┌─────────┐
│  setup  │ ✓
└────┬────┘
     │
     ▼
┌────────────┬────────────┐
│  branch_a  │  branch_b  │ * running
└─────┬──────┴──────┬─────┘
      │             │
      └──────┬──────┘
             ▼
┌──────────────┐
│   fan_in     │
└──────────────┘
```

### Node State Markers

| State | Marker | Example |
|-------|--------|---------|
| Pending | (none) | `│  setup  │` |
| Running | `* running` | `│  setup  │ * running` |
| Completed | `✓` | `│  setup  │ ✓` |

### Terminal Rendering Strategy

1. **Initial render:** Print full graph with all nodes pending
2. **On state change:**
   - Use ANSI escape `\033[{n}A` to move cursor up n lines
   - Redraw entire graph with updated states
   - Works in most modern terminals (iTerm2, Terminal.app, GNOME Terminal, Windows Terminal)
3. **Fallback for non-TTY:** Print simple progress line without redrawing
   - Use existing `is_interactive_terminal()` function at `cli.py:302`
   - When `not is_interactive_terminal()`: print `"[node_name] running..."` / `"[node_name] ✓"` instead of ANSI redraw

### Graph Data Source

**Use the compiled `StateGraph` object** (not raw YAML) for consistency:
- Graph is already validated and compiled when `--show-graph` executes
- Access via the `compiled` variable in `cli.py` run command (line ~691)

### Graph Parsing Approach

Extract from compiled `StateGraph`:
- `compiled.graph.nodes` - dict of node names to node objects (NetworkX graph)
- `compiled.graph.edges` - edge connections
- Access original YAML config via `engine._config` for `dynamic_parallel` type detection
- Detect parallel branches from `dynamic_parallel` type nodes
- Detect fan-in from `fan_in: true` attribute

### Testing

- **Location:** `python/tests/test_graph_renderer.py`
- **Framework:** pytest
- **Test cases:**
  1. Linear 3-node graph
  2. Graph with single parallel fan-out/fan-in
  3. Graph with multiple sequential parallel sections
  4. State transition: pending → running → completed
  5. Edge case: single node graph
  6. Edge case: empty graph

## Example Usage

```bash
# Show graph during execution
tea-python run workflow.yaml --show-graph

# Graph only, no other output
tea-python run workflow.yaml --show-graph --quiet

# With initial state
tea-python run workflow.yaml --show-graph --input '{"key": "value"}'
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-11 | 0.1 | Initial draft | PO (Sarah) |
| 2026-01-11 | 0.2 | Added user decisions: completed markers, standard width, horizontal parallel | PO (Sarah) |
| 2026-01-11 | 0.3 | Validation fixes: non-TTY fallback detail, test coverage, graph data source | PO (Sarah) |
| 2026-01-11 | 1.0 | **Approved** - Ready for implementation | PO (Sarah) |
| 2026-01-12 | 1.1 | **Implementation complete** - All tasks done, tests passing | Dev (James) |
| 2026-01-15 | 1.2 | **Enhancement** - DOT-generated workflows now show parallel items side-by-side via `_render_items` field | Dev (Claude Opus 4.5) |

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/cli.py` | Modified | Added `--show-graph`/`-g` flag, mutual exclusivity check with `--stream`, integrated `GraphProgressTracker` into execution loop |
| `python/src/the_edge_agent/graph_renderer.py` | Created | ASCII graph rendering module with `NodeState` enum, `RenderedNode`, `GraphLayout`, `GraphProgressTracker` class, and `render_simple_progress` helper |
| `python/tests/test_graph_renderer.py` | Created | 20 unit tests covering linear topology, parallel branches, state transitions, edge cases, CLI integration |
| `python/src/the_edge_agent/dot_parser.py` | Modified (v1.2) | Added `_render_items` field to `dynamic_parallel` nodes for static item rendering |

### Debug Log References
- None (no blockers encountered)

### Completion Notes
- All 5 tasks completed successfully
- 20 unit tests pass (test_graph_renderer.py)
- CLI flag `--show-graph`/`-g` added with mutual exclusivity validation against `--stream`
- `GraphProgressTracker` class handles graph parsing, state management, ASCII rendering with ANSI escape codes for TTY, and simple text fallback for non-TTY
- Integration with CLI execution loop: displays initial graph, updates on each `state` event, marks all complete on `final` event
- Existing test failures in test suite are due to missing `pydantic` dependency in test environment, not related to this story's changes

### Enhancement v1.2: DOT-Generated Parallel Item Rendering
- **Issue**: DOT-generated workflows with `dynamic_parallel` nodes didn't show parallel items side-by-side in `--show-graph` until runtime
- **Root cause**: Items were defined via template expression (`{{ state.phase1_items }}`) and only registered at runtime
- **Fix**:
  1. `dot_parser.py`: Added `_render_items` field to `dynamic_parallel` nodes containing static list of items known at generation time
  2. `graph_renderer.py`: Modified `_parse_graph()` to read `_render_items` and pre-register items via `register_parallel_items()`
- **Result**: Parallel items now display horizontally in the initial graph render before execution starts
