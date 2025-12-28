# Story TEA-PY-008.2: Extract Node Factory Module

## Status
Done

## QA Results
- **Gate**: CONCERNS
- **Date**: 2025-12-27
- **Assessor**: Quinn (QA)
- **Test Results**: 1,746 passed, 0 failed
- **Functional ACs**: ALL PASS (30/30)
- **Line Count**: 839 lines (target 750, 12% over)
- **Gate File**: [TEA-PY-008.2-yaml-nodes-module.yml](../qa/gates/TEA-PY-008.2-yaml-nodes-module.yml)

**Concern**: Line count exceeds target. Justified by 7 node types + Lua/Prolog runtimes.

> **QA Validation**: Passed story-draft-checklist (2025-12-27)
> - Clarity Score: 9/10
> - Highly implementable with line-by-line code guidance
> - Minor: story references could include file paths

## Story

**As a** developer maintaining the YAMLEngine codebase,
**I want** node creation and run function factory logic extracted into a dedicated `yaml_nodes.py` module,
**so that** the ~700 lines of node factory code is isolated, testable, and maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml_engine.py`, `lua_runtime.py`, `prolog_runtime.py`, `actions/`
- **Technology**: Python 3.10+, Lua (lupa), Prolog (janus-swi)
- **Follows pattern**: YE.5 extraction with engine reference
- **Touch points**:
  - `load_from_dict()` - calls `_add_node_from_config()`
  - `_create_run_function()` - creates all run functions
  - Runtime modules for Lua/Prolog execution

### Methods to Extract (Lines 1275-1973, ~700 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `_add_node_from_config()` | 1275-1331 | Add node to graph from config |
| `_wrap_with_auto_trace()` | 1332-1401 | Auto-tracing wrapper |
| `_wrap_with_observability()` | 1403-1472 | Observability wrapper |
| `_create_run_function()` | 1474-1532 | Main factory dispatch |
| `_detect_lua_code()` | 1534-1566 | Lua code detection |
| `_detect_prolog_code()` | 1568-1586 | Prolog code detection |
| `_get_lua_runtime()` | 1588-1628 | Lua runtime accessor |
| `_get_prolog_runtime()` | 1630-1656 | Prolog runtime accessor |
| `_create_inline_function()` | 1658-1744 | Inline code execution |
| `_create_action_function()` | 1746-1785 | Action wrapper creation |
| `_create_steps_function()` | 1787-1814 | Multi-step execution |
| `_create_expression_function()` | 1816-1831 | Expression evaluation |
| `_create_while_loop_function()` | 1833-1973 | While loop implementation |

## Acceptance Criteria

### Module Creation
1. New file `python/src/the_edge_agent/yaml_nodes.py` created
2. Module has comprehensive docstring with examples
3. Module under 750 lines total

### NodeFactory Class
4. `NodeFactory` class created with engine reference pattern
5. Constructor accepts engine reference for runtime/registry access
6. `add_node_from_config(graph, node_config)` method implemented
7. `create_run_function(node_config)` method implemented
8. All node creation methods moved with preserved behavior

### Runtime Integration
9. `_detect_lua_code()` works identically
10. `_detect_prolog_code()` works identically
11. `_get_lua_runtime()` returns correct runtime (main thread caching, worker fresh)
12. `_get_prolog_runtime()` returns shared runtime with thread-local state
13. Parallel branch isolation preserved for Lua (TEA-PY-002/TEA-PY-006)

### Wrapper Functions
14. `_wrap_with_auto_trace()` produces identical tracing behavior
15. `_wrap_with_observability()` produces identical observability events
16. Wrappers correctly capture trace_context and observability_context

### Node Types Supported
17. Inline Python code execution works
18. Inline Lua code execution works (when lua_enabled)
19. Inline Prolog code execution works (when prolog_enabled)
20. Action function (`uses:`) works with registry
21. Multi-step execution (`steps:`) works
22. Expression function works
23. While-loop node type works with all validations

### YAMLEngine Integration
24. `YAMLEngine._node_factory` attribute added
25. `YAMLEngine._add_node_from_config()` delegates to factory
26. `YAMLEngine._create_run_function()` delegates to factory
27. Engine passes runtime configs (lua_enabled, prolog_enabled, timeouts)

### Backward Compatibility
28. All existing tests pass without modification
29. No public API changes to YAMLEngine
30. Node behavior identical before/after extraction

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_nodes.py module** (AC: 1-3)
  - [ ] Create `python/src/the_edge_agent/yaml_nodes.py`
  - [ ] Add module docstring with examples
  - [ ] Add required imports (threading, time, json, typing)
  - [ ] Import StateGraph from stategraph

- [ ] **Task 2: Implement NodeFactory class structure** (AC: 4-8)
  - [ ] Create `NodeFactory` class
  - [ ] Add constructor with engine reference
  - [ ] Store runtime config (lua_enabled, lua_timeout, prolog_enabled, etc.)
  - [ ] Store references to trace_context, observability_context, actions_registry

- [ ] **Task 3: Move runtime detection and accessors** (AC: 9-13)
  - [ ] Move `_detect_lua_code()` method
  - [ ] Move `_detect_prolog_code()` method
  - [ ] Move `_get_lua_runtime()` with thread isolation logic
  - [ ] Move `_get_prolog_runtime()` with lazy initialization
  - [ ] Store runtime instances as factory attributes

- [ ] **Task 4: Move wrapper functions** (AC: 14-16)
  - [ ] Move `_wrap_with_auto_trace()`
  - [ ] Move `_wrap_with_observability()`
  - [ ] Ensure closures capture correct context

- [ ] **Task 5: Move node creation methods** (AC: 17-23)
  - [ ] Move `create_run_function()` (main dispatch)
  - [ ] Move `_create_inline_function()` with Python/Lua/Prolog support
  - [ ] Move `_create_action_function()` with registry lookup
  - [ ] Move `_create_steps_function()`
  - [ ] Move `_create_expression_function()`
  - [ ] Move `_create_while_loop_function()` with all validations
  - [ ] Move `add_node_from_config()` (graph node addition)

- [ ] **Task 6: Update YAMLEngine integration** (AC: 24-27)
  - [ ] Import NodeFactory in yaml_engine.py
  - [ ] Create `_node_factory` in `__init__` with runtime configs
  - [ ] Update `_add_node_from_config()` to delegate
  - [ ] Update `_create_run_function()` to delegate

- [ ] **Task 7: Verify all node types work** (AC: 17-23)
  - [ ] Test inline Python nodes
  - [ ] Test inline Lua nodes (with lua_enabled)
  - [ ] Test inline Prolog nodes (with prolog_enabled)
  - [ ] Test action nodes with registry
  - [ ] Test steps nodes
  - [ ] Test expression nodes
  - [ ] Test while_loop nodes

- [ ] **Task 8: Verify backward compatibility** (AC: 28-30)
  - [ ] Run full test suite: `pytest tests/test_yaml_engine*.py`
  - [ ] Run Lua tests: `pytest tests/test_lua_isolation.py`
  - [ ] Run Prolog tests: `pytest tests/test_prolog_runtime.py`
  - [ ] Run while-loop tests: `pytest tests/test_yaml_engine_while_loop.py`

## Dev Notes

### NodeFactory Pattern

```python
# yaml_nodes.py
"""Node factory for YAMLEngine."""

import threading
import time
import json
from typing import Any, Callable, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine
    from .stategraph import StateGraph


class NodeFactory:
    """Factory for creating node run functions."""

    def __init__(self, engine: 'YAMLEngine'):
        """
        Initialize with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - _lua_enabled, _lua_timeout
                - _prolog_enabled, _prolog_timeout, _prolog_sandbox
                - _trace_context, _auto_trace
                - _observability_context, _enable_observability
                - actions_registry
                - _process_template() method
                - _evaluate_condition() method
        """
        self._engine = engine

        # Runtime config
        self._lua_enabled = engine._lua_enabled
        self._lua_timeout = engine._lua_timeout
        self._prolog_enabled = engine._prolog_enabled
        self._prolog_timeout = engine._prolog_timeout
        self._prolog_sandbox = engine._prolog_sandbox

        # Runtime instances (lazy-initialized)
        self._lua_runtime = None
        self._prolog_runtime = None

    def add_node_from_config(
        self, graph: 'StateGraph', node_config: Dict[str, Any]
    ) -> None:
        """Add a node to the graph from configuration."""
        ...

    def create_run_function(
        self, node_config: Dict[str, Any]
    ) -> Optional[Callable]:
        """Create a run function from node configuration."""
        ...
```

### Engine Integration

```python
# yaml_engine.py
from .yaml_nodes import NodeFactory

class YAMLEngine:
    def __init__(self, ...):
        # ... existing setup ...

        # Initialize node factory after actions_registry is set
        self._node_factory = NodeFactory(self)

    def _add_node_from_config(self, graph, node_config):
        return self._node_factory.add_node_from_config(graph, node_config)

    def _create_run_function(self, node_config):
        return self._node_factory.create_run_function(node_config)
```

### Thread Isolation for Lua (TEA-PY-006)

The `_get_lua_runtime()` method has critical thread isolation logic:
- Main thread: Uses cached `self._lua_runtime`
- Worker threads: Always creates fresh runtime

This MUST be preserved exactly to prevent parallel branch state leakage.

### Testing Requirements

- **Test files**: `tests/test_yaml_engine*.py`, `tests/test_lua_isolation.py`, `tests/test_prolog_runtime.py`
- **Critical tests**: `test_yaml_engine_while_loop.py` for while-loop
- **Run command**: `cd python && pytest -v`

## Definition of Done

- [ ] yaml_nodes.py created with NodeFactory class
- [ ] All 13 node creation methods moved and working
- [ ] YAMLEngine delegates to NodeFactory
- [ ] Lua/Prolog runtime isolation preserved
- [ ] While-loop validation preserved
- [ ] All 320+ tests pass without modification
- [ ] No circular imports
- [ ] Module under 750 lines

## Risk Assessment

- **Primary Risk**: Breaking Lua thread isolation (TEA-PY-006)
- **Mitigation**: Preserve exact `_get_lua_runtime()` logic, run isolation tests

- **Secondary Risk**: While-loop validation edge cases
- **Mitigation**: Run all while-loop tests, verify error messages match

- **Rollback**: Git revert single commit

## Dependency

- **Depends on**: TEA-PY-008.1 (TemplateProcessor for `_process_template` calls)
- **Must delegate to**: TemplateProcessor for template processing in node functions

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
