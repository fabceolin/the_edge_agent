# TEA-REFACTOR-001: StateGraph Modularization - Brownfield Enhancement

## Status: Draft

## Epic Goal

Split the monolithic `stategraph.py` (2982 lines) into smaller, focused modules to improve maintainability and navigability while preserving 100% API compatibility.

## Epic Description

### Existing System Context

- **Current relevant functionality:** `StateGraph` is the core orchestration class for TEA's graph-based workflow execution, handling sequential, parallel, and streaming execution patterns
- **Technology stack:** Python 3.10+, networkx for graph structure, threading for parallel execution
- **Integration points:**
  - Public API: `from the_edge_agent import StateGraph, START, END`
  - Mixins already extracted: `CheckpointMixin`, `VisualizationMixin`
  - Used by: `YAMLEngine`, CLI, tests, user code
- **Previous refactoring:** TD.12 extracted checkpoint and visualization modules

### Enhancement Details

**What's being changed:**

Extract logical groupings from `stategraph.py` into separate modules using the established mixin pattern:

| Module | Lines | Responsibility |
|--------|-------|----------------|
| `parallel_execution.py` | ~700 | `_execute_flow*`, `_stream_parallel_flow*`, reliability wrappers |
| `streaming_execution.py` | ~450 | `stream()`, `_stream_from_checkpoint()` |
| `scoped_execution.py` | ~300 | `execute_scoped()`, `_run_scoped()`, path validation |
| `session_state.py` | ~100 | `_maybe_inject_session_state()`, `_maybe_auto_save_session()` |
| `stategraph.py` (core) | ~1400 | Core class, `invoke()`, `compile()`, graph construction |

**How it integrates:**

- Each module becomes a mixin class (e.g., `ParallelExecutionMixin`)
- `StateGraph` inherits from all mixins (like existing `CheckpointMixin`, `VisualizationMixin`)
- All public imports remain unchanged through `__init__.py` re-exports

**Success criteria:**

1. All existing tests pass without modification
2. `from the_edge_agent import StateGraph, START, END` works unchanged
3. No module exceeds ~1500 lines
4. Each module has a single, clear responsibility
5. Import cycles are avoided through careful dependency ordering

---

## Stories

### Story 1: TEA-REFACTOR-001.1 - Extract Parallel Execution Module

**As a** developer maintaining TEA,
**I want** parallel execution logic in its own module,
**so that** I can navigate and modify parallel flow code without scrolling through 3000 lines.

**Acceptance Criteria:**

1. New file `python/src/the_edge_agent/parallel_execution.py` created
2. Contains `ParallelExecutionMixin` class with methods:
   - `_execute_flow()`
   - `_execute_flow_with_reliability()`
   - `_stream_parallel_flow()`
   - `_stream_parallel_flow_with_reliability()`
   - `_get_parallel_config_for_edge()`
3. `StateGraph` inherits from `ParallelExecutionMixin`
4. All parallel execution tests pass unchanged
5. No circular imports

**Tasks:**

- [ ] Create `parallel_execution.py` with `ParallelExecutionMixin`
- [ ] Move parallel methods to new module
- [ ] Add mixin inheritance to `StateGraph`
- [ ] Verify all tests pass
- [ ] Update `__init__.py` if needed

---

### Story 2: TEA-REFACTOR-001.2 - Extract Streaming Execution Module

**As a** developer maintaining TEA,
**I want** streaming execution logic in its own module,
**so that** streaming code changes don't risk breaking non-streaming execution.

**Acceptance Criteria:**

1. New file `python/src/the_edge_agent/streaming_execution.py` created
2. Contains `StreamingExecutionMixin` class with methods:
   - `stream()`
   - `_stream_from_checkpoint()` (if exists)
   - Related streaming helpers
3. `StateGraph` inherits from `StreamingExecutionMixin`
4. All streaming tests pass unchanged
5. No circular imports

**Tasks:**

- [ ] Create `streaming_execution.py` with `StreamingExecutionMixin`
- [ ] Move streaming methods to new module
- [ ] Add mixin inheritance to `StateGraph`
- [ ] Verify all tests pass

---

### Story 3: TEA-REFACTOR-001.3 - Extract Scoped & Session Modules

**As a** developer maintaining TEA,
**I want** scoped execution and session state logic in dedicated modules,
**so that** each module has a single responsibility.

**Acceptance Criteria:**

1. New file `python/src/the_edge_agent/scoped_execution.py` created with:
   - `ScopedExecutionMixin` containing `execute_scoped()`, `_run_scoped()`, path validation methods
2. New file `python/src/the_edge_agent/session_state.py` created with:
   - `SessionStateMixin` containing `_maybe_inject_session_state()`, `_maybe_auto_save_session()`
3. `StateGraph` inherits from both new mixins
4. All related tests pass unchanged
5. Core `stategraph.py` is now under 1500 lines
6. No circular imports

**Tasks:**

- [ ] Create `scoped_execution.py` with `ScopedExecutionMixin`
- [ ] Create `session_state.py` with `SessionStateMixin`
- [ ] Move respective methods to new modules
- [ ] Add mixin inheritance to `StateGraph`
- [ ] Verify all tests pass
- [ ] Verify final line count of `stategraph.py` < 1500

---

## Compatibility Requirements

- [x] Existing APIs remain unchanged (`StateGraph`, `START`, `END` exports)
- [x] Database schema changes are backward compatible (N/A - no DB)
- [x] UI changes follow existing patterns (N/A - no UI)
- [x] Performance impact is minimal (mixin inheritance is negligible)

## Risk Mitigation

**Primary Risk:** Breaking existing user code or internal tests due to import changes

**Mitigation:**
- Use mixin pattern (proven by TD.12 for checkpoint/visualization)
- Run full test suite after each story
- Maintain all public exports through `__init__.py`
- No changes to method signatures

**Rollback Plan:**
- Git revert to pre-refactoring commit
- Each story is independently revertable

## Definition of Done

- [ ] All 3 stories completed with acceptance criteria met
- [ ] Existing functionality verified through full test suite
- [ ] Integration points (YAMLEngine, CLI) working correctly
- [ ] Documentation updated (source-tree.md)
- [ ] No regression in existing features
- [ ] `stategraph.py` < 1500 lines

---

## Technical Notes

### Module Dependency Order

To avoid circular imports, modules should be loaded in this order:

```
parallel.py (types)
  ↓
parallel_execution.py (uses parallel.py)
  ↓
streaming_execution.py (uses parallel.py, may call parallel methods)
  ↓
scoped_execution.py (independent)
  ↓
session_state.py (independent)
  ↓
stategraph.py (inherits all mixins)
```

### Mixin Pattern Reference

Follow the established pattern from `checkpoint.py`:

```python
class ParallelExecutionMixin:
    """Mixin providing parallel execution capabilities."""

    def _execute_flow(self, current_node, state, config, fan_in_node):
        # Implementation using self.graph, self.logger, etc.
        ...
```

### Files Affected

- `python/src/the_edge_agent/stategraph.py` (modify)
- `python/src/the_edge_agent/parallel_execution.py` (create)
- `python/src/the_edge_agent/streaming_execution.py` (create)
- `python/src/the_edge_agent/scoped_execution.py` (create)
- `python/src/the_edge_agent/session_state.py` (create)
- `python/src/the_edge_agent/__init__.py` (may need updates)
- `docs/python/source-tree.md` (update)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-12 | 1.0 | Initial epic creation | PO Agent (Sarah) |
