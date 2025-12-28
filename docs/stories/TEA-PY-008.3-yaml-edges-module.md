# Story TEA-PY-008.3: Extract Edge Factory Module

## Status
Done

## QA Results
- **Gate**: CONCERNS
- **Date**: 2025-12-27
- **Assessor**: Quinn (QA)
- **Test Results**: 1,746 passed, 0 failed
- **Functional ACs**: ALL PASS (34/34)
- **Line Count**: 429 lines (target 400, 7% over)
- **Gate File**: [TEA-PY-008.3-yaml-edges-module.yml](../qa/gates/TEA-PY-008.3-yaml-edges-module.yml)

**Concern**: Minor line count overrun. TEA-YAML-002 implicit chaining adds complexity.

> **QA Validation**: Passed story-draft-checklist (2025-12-27)
> - Clarity Score: 8/10
> - Excellent method table with exact line numbers (1975-2324)
> - Minor: "fan-in" term used without definition

## Story

**As a** developer maintaining the YAMLEngine codebase,
**I want** edge and goto processing logic extracted into a dedicated `yaml_edges.py` module,
**so that** the ~350 lines of graph navigation code is isolated and the TEA-YAML-002 implicit graph syntax is maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml_engine.py`, `stategraph.py`
- **Technology**: Python 3.10+, StateGraph edge APIs
- **Follows pattern**: YE.5 extraction with engine reference
- **Touch points**:
  - `load_from_dict()` - calls edge processing after node creation
  - `StateGraph.add_edge()`, `add_conditional_edges()`, `add_parallel_edge()`
  - `_evaluate_condition()` for condition evaluation

### Methods to Extract (Lines 1975-2324, ~350 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `_process_goto_and_implicit_edges()` | 1975-2063 | TEA-YAML-002 implicit chaining |
| `_process_node_goto()` | 2065-2162 | Per-node goto processing |
| `_evaluate_goto_condition()` | 2164-2218 | Goto condition evaluation |
| `_add_edge_from_config()` | 2220-2324 | Legacy edge configuration |

### Key Features to Preserve

1. **Precedence rules** (TEA-YAML-002):
   - goto property (highest)
   - edges section (legacy, deprecated)
   - implicit chaining (lowest)

2. **Implicit entry/exit**:
   - First node is automatic entry point
   - Last node is automatic finish point

3. **Deprecation warnings** for legacy `edges` section

## Acceptance Criteria

### Module Creation
1. New file `python/src/the_edge_agent/yaml_edges.py` created
2. Module has comprehensive docstring with examples
3. Module under 400 lines total

### EdgeFactory Class
4. `EdgeFactory` class created with engine reference pattern
5. Constructor accepts engine reference for template evaluation
6. `process_goto_and_implicit_edges(graph, nodes_list, edges_list)` implemented
7. `add_edge_from_config(graph, edge_config)` implemented

### Goto Processing (TEA-YAML-002)
8. String goto (unconditional) works: `goto: "target_node"`
9. List goto (conditional) works: `goto: [{if: expr, to: target}, ...]`
10. Fallback rule (no condition) works
11. Target validation at parse time (AC-6 from TEA-YAML-002)
12. `__end__` target works for finish points
13. Goto precedence over legacy edges enforced

### Implicit Chaining
14. Entry point set to first node (if no __start__ edge)
15. Implicit chaining to next node for nodes without goto/edges
16. Finish point set for last node (if no __end__ edge)
17. Nodes with legacy edges skip implicit chaining

### Condition Evaluation
18. `_evaluate_goto_condition(expr, state, result)` works
19. Access to `state`, `result`, `variables`, `secrets` in conditions
20. Jinja2 expression syntax supported
21. Template caching for condition expressions

### Legacy Edge Support
22. Normal edges work
23. Parallel edges work
24. Conditional edges with `condition:` work
25. `when:` clause syntactic sugar works
26. Entry edges from `__start__` work
27. Finish edges to `__end__` work

### YAMLEngine Integration
28. `YAMLEngine._edge_factory` attribute added
29. `YAMLEngine._process_goto_and_implicit_edges()` delegates
30. `YAMLEngine._add_edge_from_config()` delegates
31. `_nodes_with_goto` tracking preserved

### Backward Compatibility
32. All existing tests pass without modification
33. Deprecation warnings still logged for edges section
34. Edge behavior identical before/after extraction

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_edges.py module** (AC: 1-3)
  - [ ] Create `python/src/the_edge_agent/yaml_edges.py`
  - [ ] Add module docstring with examples
  - [ ] Add required imports (typing, logging)
  - [ ] Import StateGraph, START, END from stategraph

- [ ] **Task 2: Implement EdgeFactory class** (AC: 4-7)
  - [ ] Create `EdgeFactory` class
  - [ ] Add constructor with engine reference
  - [ ] Store references to variables, secrets, jinja_env, template_cache
  - [ ] Store reference to _evaluate_condition (via engine)

- [ ] **Task 3: Move goto processing** (AC: 8-13)
  - [ ] Move `_process_goto_and_implicit_edges()`
  - [ ] Move `_process_node_goto()`
  - [ ] Move `_evaluate_goto_condition()`
  - [ ] Preserve all validation and error messages
  - [ ] Preserve precedence logic

- [ ] **Task 4: Move legacy edge processing** (AC: 22-27)
  - [ ] Move `_add_edge_from_config()`
  - [ ] Preserve all edge types (normal, parallel, conditional)
  - [ ] Preserve when clause handling
  - [ ] Preserve entry/finish edge handling

- [ ] **Task 5: Preserve implicit chaining** (AC: 14-17)
  - [ ] Entry point auto-set to first node
  - [ ] Chain to next node for implicit nodes
  - [ ] Finish point auto-set for last node
  - [ ] Skip implicit for nodes with legacy edges

- [ ] **Task 6: Update YAMLEngine integration** (AC: 28-31)
  - [ ] Import EdgeFactory in yaml_engine.py
  - [ ] Create `_edge_factory` in `__init__`
  - [ ] Update `_process_goto_and_implicit_edges()` to delegate
  - [ ] Update `_add_edge_from_config()` to delegate
  - [ ] Preserve `_nodes_with_goto` attribute

- [ ] **Task 7: Verify backward compatibility** (AC: 32-34)
  - [ ] Run goto tests: `pytest tests/test_yaml_engine_goto.py`
  - [ ] Run edge tests: `pytest tests/test_yaml_engine_edges.py`
  - [ ] Verify deprecation warnings logged

## Dev Notes

### EdgeFactory Pattern

```python
# yaml_edges.py
"""Edge and goto processing for YAMLEngine."""

import logging
from typing import Any, Dict, List, Optional, Set, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine
    from .stategraph import StateGraph

from .stategraph import START, END

logger = logging.getLogger(__name__)


class EdgeFactory:
    """Factory for processing edges and goto navigation."""

    def __init__(self, engine: 'YAMLEngine'):
        """
        Initialize with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - _jinja_env for template processing
                - _template_cache for caching
                - variables, secrets for context
                - _evaluate_condition() method
        """
        self._engine = engine
        self._nodes_with_goto: Set[str] = set()

    def process_goto_and_implicit_edges(
        self,
        graph: 'StateGraph',
        nodes_list: List[Dict[str, Any]],
        edges_list: List[Dict[str, Any]],
    ) -> None:
        """
        Process goto properties and implicit chaining for nodes.

        TEA-YAML-002: Implements the new implicit graph navigation syntax.
        """
        ...

    def add_edge_from_config(
        self, graph: 'StateGraph', edge_config: Dict[str, Any]
    ) -> None:
        """Add an edge to the graph from configuration."""
        ...

    @property
    def nodes_with_goto(self) -> Set[str]:
        """Return set of node names that have goto definitions."""
        return self._nodes_with_goto
```

### Integration in YAMLEngine

```python
# yaml_engine.py
from .yaml_edges import EdgeFactory

class YAMLEngine:
    def __init__(self, ...):
        # ... after template processor setup ...
        self._edge_factory = EdgeFactory(self)

    def _process_goto_and_implicit_edges(self, graph, nodes_list, edges_list):
        self._edge_factory.process_goto_and_implicit_edges(
            graph, nodes_list, edges_list
        )
        # Copy nodes_with_goto for legacy edge precedence
        self._nodes_with_goto = self._edge_factory.nodes_with_goto

    def _add_edge_from_config(self, graph, edge_config):
        return self._edge_factory.add_edge_from_config(graph, edge_config)
```

### Condition Evaluation Delegation

The `_evaluate_goto_condition` method needs access to `_evaluate_condition` from the template processor:

```python
def _evaluate_goto_condition(
    self, expr: str, state: Dict[str, Any], result: Optional[Dict[str, Any]] = None
) -> bool:
    """Evaluate goto condition with state and result context."""
    # Use engine's template processing
    context = {
        "state": DotDict(state) if state else DotDict({}),
        "result": DotDict(result) if result else DotDict({}),
        "variables": DotDict(self._engine.variables),
        "secrets": DotDict(self._engine.secrets),
    }
    # ... rest of implementation
```

### Testing Requirements

- **Key test files**:
  - `tests/test_yaml_engine_goto.py` - 16 goto tests
  - `tests/test_yaml_engine_edges.py` - Edge tests
- **Run command**: `cd python && pytest tests/test_yaml_engine_goto.py tests/test_yaml_engine_edges.py -v`

## Definition of Done

- [ ] yaml_edges.py created with EdgeFactory class
- [ ] All goto and edge methods moved and working
- [ ] YAMLEngine delegates to EdgeFactory
- [ ] Precedence rules (goto > edges > implicit) preserved
- [ ] All 320+ tests pass without modification
- [ ] No circular imports
- [ ] Module under 400 lines

## Risk Assessment

- **Primary Risk**: Breaking goto precedence logic
- **Mitigation**: Preserve exact precedence code, run all goto tests

- **Secondary Risk**: Condition closure capture issues
- **Mitigation**: Use factory function pattern for closures (already in code)

- **Rollback**: Git revert single commit

## Dependency

- **Depends on**: TEA-PY-008.1 (TemplateProcessor for condition evaluation)
- **Uses**: `_evaluate_condition()` via engine reference

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
