# TEA-CLI-006: Show Graph Progress During Execution

## Status
Approved

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

- [ ] **Task 1: Add CLI flag** (AC: 1, 8, 9)
  - [ ] Add `--show-graph` / `-g` boolean option to `run` command in `cli.py`
  - [ ] Validate mutual exclusivity with `--stream`
  - [ ] When combined with `--quiet`, suppress node completion messages but show graph

- [ ] **Task 2: Implement ASCII graph renderer** (AC: 2, 6, 7, 10, 11)
  - [ ] Create `graph_renderer.py` module in `python/src/the_edge_agent/`
  - [ ] Parse compiled graph structure (nodes, edges) into renderable format
  - [ ] Generate ASCII layout with vertical flow, horizontal parallel branches
  - [ ] Handle `__start__` and `__end__` special nodes
  - [ ] Handle `dynamic_parallel` nodes with fan-in visualization

- [ ] **Task 3: Implement progress state tracking** (AC: 3, 4, 5)
  - [ ] Create `GraphProgressTracker` class to manage node states
  - [ ] States: `pending`, `running`, `completed`
  - [ ] Method to render current state as ASCII string

- [ ] **Task 4: Integrate with CLI execution loop** (AC: 3, 4, 5)
  - [ ] Hook into `stream()` event loop in `cli.py`
  - [ ] On `state` event: mark previous node complete, mark current node running
  - [ ] Clear and redraw graph on each state change (using ANSI escape codes)
  - [ ] On `final` event: mark all nodes complete

- [ ] **Task 5: Testing** (AC: all)
  - [ ] Unit tests for graph renderer with linear topology
  - [ ] Unit tests for parallel branch rendering
  - [ ] Unit tests for progress state transitions
  - [ ] Test `--show-graph --quiet` combination shows only graph output
  - [ ] Test non-TTY fallback produces simple text output
  - [ ] Integration test with example workflow file

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
