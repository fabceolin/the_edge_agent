# BUG.003: Conditional Edge to __end__ Ignores Condition

**Status:** ready-for-dev
**Priority:** High
**Reported:** 2026-03-20
**Reporter:** Dev Agent (llm_prompt POC)
**Affected Version:** TEA current (edges section)

---

## Story

**As a** TEA YAML agent developer,
**I want** conditional edges targeting `__end__` to respect their condition expression,
**so that** I can use patterns like "go to __end__ only on error" without the agent always terminating.

---

## Problem

In `yaml_edges.py`, the `add_edge` method (around line 373) checks:

```python
if to_node == END or edge_type == "finish":
    graph.set_finish_point(from_node)
    return
```

This check runs **before** the conditional edge handling code (line 384+). When a conditional edge targets `__end__`:

```yaml
edges:
  - from: build_request
    to: __end__
    condition: "{{ state.status == 'error' }}"
```

The condition is completely ignored — `set_finish_point(from_node)` is called unconditionally, making `build_request` always terminate the graph regardless of state.

### Impact

Any YAML agent that tries to conditionally exit early to `__end__` will silently have that node become an unconditional finish point. All other conditional edges from the same node are also effectively dead code.

### Current workaround

Avoid conditional edges to `__end__`. Instead, use 2-way conditional branching between non-end nodes, and only use unconditional `to: __end__` or `goto: __end__` on terminal nodes.

---

## Acceptance Criteria

- [ ] **AC-1:** Conditional edges to `__end__` evaluate their condition before routing
- [ ] **AC-2:** Non-conditional edges to `__end__` continue to work as `set_finish_point`
- [ ] **AC-3:** The `goto: __end__` node-level property continues to work unchanged
- [ ] **AC-4:** Existing agents using non-conditional `to: __end__` are unaffected (backward compat)
- [ ] **AC-5:** Unit test covers conditional edge to `__end__` with true/false conditions

---

## Tasks / Subtasks

### Task 1: Fix edge processing order (AC-1, AC-2)

- [ ] 1.1 In `yaml_edges.py` `add_edge` method, move the `to_node == END` check **after** the condition check
- [ ] 1.2 When a condition is present AND `to_node == END`, create a conditional edge with `cond_map = {True: END}` instead of calling `set_finish_point`
- [ ] 1.3 When NO condition is present AND `to_node == END`, keep current `set_finish_point` behavior

### Task 2: Tests (AC-5)

- [ ] 2.1 Add test: conditional edge to `__end__` with condition=True → graph ends
- [ ] 2.2 Add test: conditional edge to `__end__` with condition=False → graph continues to next matching edge
- [ ] 2.3 Add test: 3-way branch from same node (to __end__, to A, to B) all conditional

---

## Dev Notes

### Root cause location

`python/src/the_edge_agent/yaml_edges.py`, `add_edge` method, approximately line 373.

### Fix approach

```python
# BEFORE (buggy): unconditional finish regardless of condition
if to_node == END or edge_type == "finish":
    graph.set_finish_point(from_node)
    return

# AFTER: only unconditional finish when no condition present
if (to_node == END or edge_type == "finish") and "condition" not in edge_config and "when" not in edge_config:
    graph.set_finish_point(from_node)
    return
```

Then the existing condition handling code at line 384+ will process the conditional `__end__` edge correctly since `add_conditional_edges` already supports `END` as a target.

### References

- [Source: python/src/the_edge_agent/yaml_edges.py#add_edge]
- [Source: python/src/the_edge_agent/stategraph.py#add_conditional_edges]
- Discovered during: `visionQuest/ai-workflow/agents/llm_prompt.yaml` POC
