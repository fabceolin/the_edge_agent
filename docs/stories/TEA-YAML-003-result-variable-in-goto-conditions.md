# Story: Result Variable in Goto Conditions

## Status
Backlog

### Priority
Low

### Parent Story
Descoped from TEA-YAML-002 (v1.3)

## Story
**As a** YAML agent developer,
**I want** access to a separate `result` variable in goto condition expressions,
**So that** I can distinguish between values returned by the current node versus pre-existing state values, enabling cleaner condition logic without naming conflicts.

## Background

In TEA-YAML-002, the original AC5 specified that goto conditions should have access to both `state` and `result` variables. During implementation, `result` was descoped because:

1. Node execution results are automatically merged into `state` before goto evaluation
2. The workaround `state.field_name` covers most use cases
3. Implementing a separate `result` variable requires executor-level changes in both Python and Rust

### Current Behavior (TEA-YAML-002)

```yaml
nodes:
  - name: check
    run: |
      return {"score": calculate_score()}
    goto:
      - if: "state.score > 80"  # Must use state.score, not result.score
        to: success
```

### Desired Behavior (This Story)

```yaml
nodes:
  - name: check
    run: |
      return {"score": calculate_score()}
    goto:
      - if: "result.score > 80"  # Direct access to node's return value
        to: success
      - if: "state.previous_score > result.score"  # Compare with pre-existing state
        to: regression_detected
```

## Acceptance Criteria

1. **Result Variable Available in Goto Conditions**
   - The `result` variable must be available in `if` expressions within goto rules
   - `result` contains the raw return value from the current node's execution
   - `result` is available BEFORE the merge into `state`

2. **State Variable Unchanged**
   - `state` continues to represent the full agent state (including merged results)
   - Backward compatibility: existing `state.field` access patterns continue to work

3. **Python Implementation**
   - Modify `_prepare_function_params` in `stategraph.py` to include `result`
   - Store node execution result before merge and pass to condition functions
   - Update `_evaluate_goto_condition` to use the passed result

4. **Rust Implementation**
   - Modify `determine_next_node` in `executor.rs` to accept result parameter
   - Pass result to `render_template` context
   - Ensure Tera templates can access `result` object

5. **Documentation**
   - Update YAML_REFERENCE.md with `result` variable usage
   - Add examples showing `result` vs `state` usage patterns
   - Update TEA-YAML-002 to remove descope note when complete

## Tasks / Subtasks

- [ ] **Python Executor Changes**
  - [ ] Store node result before merging into state
  - [ ] Add `result` to `available_params` in edge evaluation
  - [ ] Update `_evaluate_goto_condition` signature and implementation
  - [ ] Add tests for `result` variable access in goto conditions

- [ ] **Rust Executor Changes**
  - [ ] Store node result before merging into state
  - [ ] Pass result to `render_template` context in `determine_next_node`
  - [ ] Add tests for `result` variable access

- [ ] **Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md`
  - [ ] Add usage examples to goto documentation
  - [ ] Update TEA-YAML-002 to remove descope references

## Dev Notes

### Why This Was Descoped

The implementation requires changes to the executor's control flow:

1. Currently: `node runs` -> `result merges into state` -> `edges evaluated with state`
2. Required: `node runs` -> `store result` -> `edges evaluated with state AND result` -> `merge result into state`

This is a cross-cutting change affecting:
- `python/src/the_edge_agent/stategraph.py` (invoke, stream, _get_next_node)
- `rust/src/engine/executor.rs` (determine_next_node, run methods)

### Use Cases for Separate Result Variable

1. **Conflict avoidance**: When state already has a key that the node also returns
2. **Comparison logic**: `result.new_value > state.old_value`
3. **Cleaner semantics**: Explicit separation of "what this node returned" vs "accumulated state"

### Testing

- Python: Add tests to `tests/test_yaml_engine_goto.py`
- Rust: Add tests to `tests/test_yaml_goto.rs`
- Test both `result.field` and `state.field` access patterns
- Test conflict scenarios where state and result have same key

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-24 | 1.0 | Initial backlog story created (descoped from TEA-YAML-002 v1.3) | Sarah (PO) |
