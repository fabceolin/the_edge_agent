# Story YE.9: Deep Array Merge by Identifying Key

## Status

Ready for Review

## Story

**As a** YAML agent developer,
**I want** overlay YAML files to merge arrays by identifying key (e.g., `nodes` by `name`, `edges` by `from`+`to`, `goto` by `to`),
**so that** I can override individual nodes or edges in an overlay without duplicating the entire array.

## Motivation

YE.8 introduced YAML overlay merging with kubectl-style semantics where **arrays are replaced entirely**. While this works for settings and variables, it creates a significant limitation for `nodes`, `edges`, and `goto` arrays:

### Current Limitation

```yaml
# base.yaml - 5 nodes defined
nodes:
  - name: fetch_data
    run: |
      return {"data": fetch()}
  - name: call_llm
    uses: llm.call
    with:
      model: gpt-4o-mini
      prompt: "{{ state.data }}"
  - name: format_output
    run: |
      return {"result": format(state.response)}
  - name: validate
    run: |
      return {"valid": check(state.result)}
  - name: save
    uses: memory.store
    with:
      key: "result"
      value: "{{ state.result }}"

# overlay.yaml - User only wants to change LLM provider
nodes:
  - name: call_llm
    uses: llm.call
    with:
      model: claude-3-opus    # Changed
      prompt: "{{ state.data }}"

# CURRENT RESULT: Only 1 node remains (all others lost!)
# DESIRED RESULT: 5 nodes, with call_llm updated
```

### Workaround Today

Users must duplicate the entire `nodes` array in the overlay, leading to:
- **Configuration duplication** (defeats the purpose of overlays)
- **Drift risk** (changes to base nodes must be replicated in overlays)
- **Maintenance burden** (N overlays × M nodes to maintain)

### Proposed Solution

Implement **strategic merge patch for arrays** using identifying keys:

| Array | Merge Key | Rationale |
|-------|-----------|-----------|
| `nodes` | `name` | Uniquely identifies each node |
| `edges` | `from` + `to` | Uniquely identifies edge route |
| `goto` (on nodes) | `to` | Each routing destination appears once |

## Design Decisions

### DD-1: Array Merge Key Configuration

**Decision Date:** 2026-01-18

**Context:** Need to define which arrays use key-based merging and what keys identify elements.

**Design:** Hardcode merge keys for known TEA arrays:

```python
ARRAY_MERGE_KEYS = {
    "nodes": ["name"],
    "edges": ["from", "to"],
    # goto arrays inside nodes are handled specially
}

# For goto arrays (nested in nodes), merge by "to" field
GOTO_MERGE_KEY = ["to"]
```

**Rationale:**
- Simple, predictable behavior
- No configuration overhead for users
- Matches semantic meaning of these arrays
- Can be extended later with explicit config if needed

### DD-2: Merge Semantics for Array Elements

**Decision Date:** 2026-01-18

**Context:** When an overlay element matches a base element by key, how are they merged?

**Design:** Use recursive deep merge for matched elements:

```yaml
# base.yaml
nodes:
  - name: call_llm
    uses: llm.call
    with:
      model: gpt-4o-mini
      temperature: 0.7
      max_tokens: 1000

# overlay.yaml
nodes:
  - name: call_llm
    with:
      model: claude-3-opus    # Override
      temperature: 0.3        # Override
      # max_tokens preserved from base

# Result:
nodes:
  - name: call_llm
    uses: llm.call           # Preserved from base
    with:
      model: claude-3-opus   # From overlay
      temperature: 0.3       # From overlay
      max_tokens: 1000       # Preserved from base
```

**Rationale:**
- Minimizes overlay size (only specify what changes)
- Consistent with object merge semantics from YE.8
- Allows partial updates to complex node configurations

### DD-3: Non-Matching Elements

**Decision Date:** 2026-01-18

**Context:** What happens to overlay elements that don't match any base element?

**Design:** Append new elements to the array:

```yaml
# base.yaml
nodes:
  - name: step_a
  - name: step_b

# overlay.yaml
nodes:
  - name: step_c    # New element (no match in base)

# Result:
nodes:
  - name: step_a    # From base
  - name: step_b    # From base
  - name: step_c    # Appended from overlay
```

**Rationale:**
- Allows overlays to add new nodes/edges
- Natural extension of merge-by-key semantics
- Matches user expectations

### DD-4: Element Deletion

**Decision Date:** 2026-01-18

**Context:** How can users remove elements via overlay?

**Design:** Use explicit `__delete__: true` marker:

```yaml
# overlay.yaml - Remove a node
nodes:
  - name: deprecated_node
    __delete__: true

# Result: deprecated_node is removed from merged array
```

**Rationale:**
- Explicit intent (no accidental deletions)
- Consistent with kubectl's strategic merge patch
- Simple to implement and understand

### DD-5: Breaking Change Approach

**Decision Date:** 2026-01-18

**Context:** User confirmed breaking change is acceptable.

**Design:** Replace the current array replacement behavior with merge-by-key as the default (no backward compatibility flag).

**Migration:**
- Users who relied on full array replacement can use `__replace__: true` at array level
- Document migration path in release notes

```yaml
# Force old behavior (replace entire array)
nodes:
  __replace__: true
  items:
    - name: only_this_node
```

**Alternative (simpler):** If an overlay array contains no elements with matching keys, it replaces entirely (existing behavior preserved for that case).

### DD-6: Handling `goto` Arrays on Nodes

**Decision Date:** 2026-01-18

**Context:** `goto` is a property on individual nodes, not a top-level array.

**Design:** When merging a node that has a `goto` array, apply merge-by-key using `to` as the key:

```yaml
# base.yaml
nodes:
  - name: router
    goto:
      - if: "state.score > 0.9"
        to: high_confidence
      - if: "state.score > 0.5"
        to: medium_confidence
      - to: low_confidence

# overlay.yaml - Change condition for high_confidence route
nodes:
  - name: router
    goto:
      - if: "state.score > 0.95"    # Changed threshold
        to: high_confidence

# Result:
nodes:
  - name: router
    goto:
      - if: "state.score > 0.95"    # From overlay (matched by to)
        to: high_confidence
      - if: "state.score > 0.5"     # Preserved from base
        to: medium_confidence
      - to: low_confidence          # Preserved from base
```

**Rationale:**
- `to` field identifies the routing destination
- Each destination should appear once in a goto list
- Allows fine-grained control over routing rules

## Acceptance Criteria

1. `nodes` array merges by `name` key (matching nodes are deep-merged, non-matching are appended)
2. `edges` array merges by `from`+`to` composite key (matching edges are deep-merged, non-matching are appended)
3. `goto` arrays (inside nodes) merge by `to` key (matching entries are deep-merged, non-matching are appended)
4. Matched array elements use recursive deep merge (nested objects merged, not replaced)
5. Non-matched overlay elements are appended to the array
6. `__delete__: true` marker removes elements from the merged array
7. `--dump-merged` shows correctly merged arrays
8. Breaking change: arrays merge by key by default (no backward compat flag)
9. Both Python and Rust implementations produce identical merge results (parity)
10. Comprehensive unit tests cover all merge scenarios
11. Documentation updated in YAML_REFERENCE.md with new merge semantics
12. Migration guide for users relying on array replacement behavior

## Dependencies

**Blocked By:** YE.8 (YAML Overlay Merge Support) - Complete

**Blocks:** None

**Internal Dependencies (Python):**
- `python/src/the_edge_agent/schema/deep_merge.py` - Enhance with array key merge
- `python/src/the_edge_agent/cli.py` - No changes needed (uses deep_merge)
- `python/tests/test_cli_overlay.py` - Add array merge tests

**Internal Dependencies (Rust):**
- `rust/src/engine/deep_merge.rs` - Enhance with array key merge
- `rust/src/bin/tea.rs` - No changes needed (uses deep_merge)
- `rust/tests/test_cli_overlay.rs` - Add array merge tests

## Tasks / Subtasks

### Phase 1: Python Implementation (AC: 1-8, 10)

- [x] Task 1.1: Enhance `deep_merge.py` with array key merge (AC: 1, 2, 4, 5)
  - [x] Add `ARRAY_MERGE_KEYS` configuration dict
  - [x] Implement `merge_arrays_by_key(base, overlay, keys)` function
  - [x] Integrate into existing `deep_merge()` function
  - [x] Handle nested `goto` arrays on nodes (AC: 3)

- [x] Task 1.2: Implement element deletion (AC: 6)
  - [x] Check for `__delete__: true` marker in overlay elements
  - [x] Remove matching base elements when marker present
  - [x] Unit tests for deletion scenarios

- [x] Task 1.3: Write Python tests (AC: 10)
  - [x] Test nodes merge by name
  - [x] Test edges merge by from+to
  - [x] Test goto merge by to
  - [x] Test nested object merge within matched elements
  - [x] Test non-matching elements appended
  - [x] Test __delete__ marker removes elements
  - [x] Test --dump-merged shows merged arrays
  - [x] Test complex scenario: base has 5 nodes, overlay modifies 2, adds 1, deletes 1

### Phase 2: Rust Implementation (AC: 1-8, 10)

- [x] Task 2.1: Enhance `deep_merge.rs` with array key merge (AC: 1, 2, 4, 5)
  - [x] Add `ARRAY_MERGE_KEYS` configuration
  - [x] Implement `merge_arrays_by_key()` function
  - [x] Integrate into existing `deep_merge()` function
  - [x] Handle nested `goto` arrays on nodes (AC: 3)

- [x] Task 2.2: Implement element deletion (AC: 6)
  - [x] Check for `__delete__: true` marker in overlay elements
  - [x] Remove matching base elements when marker present
  - [x] Unit tests for deletion scenarios

- [x] Task 2.3: Write Rust tests (AC: 10)
  - [x] Test nodes merge by name
  - [x] Test edges merge by from+to
  - [x] Test goto merge by to
  - [x] Test nested object merge within matched elements
  - [x] Test non-matching elements appended
  - [x] Test __delete__ marker removes elements

### Phase 3: Parity Verification (AC: 9)

- [x] Task 3.1: Extend parity test suite
  - [x] Add array merge fixtures to `tests/fixtures/overlay/`
  - [x] Extend `test_overlay_parity.py` with array merge cases
  - [x] Verify identical behavior for edge cases

### Phase 4: Documentation (AC: 11, 12)

- [x] Task 4.1: Update YAML_REFERENCE.md
  - [x] Update "YAML Overlay Merging" section with new array semantics
  - [x] Add table of array merge keys
  - [x] Add examples showing node/edge/goto merging
  - [x] Document `__delete__` marker

- [x] Task 4.2: Create migration guide
  - [x] Document breaking change from YE.8 behavior
  - [x] Provide migration examples for users
  - [x] Update devops-yaml-overlay-agents.md article

## Dev Notes

### Algorithm Pseudocode

```python
def deep_merge(base, overlay, path=""):
    """Deep merge with array key merging."""
    if isinstance(base, dict) and isinstance(overlay, dict):
        result = base.copy()
        for key, overlay_value in overlay.items():
            if key in result:
                result[key] = deep_merge(result[key], overlay_value, f"{path}.{key}")
            else:
                result[key] = overlay_value
        return result

    elif isinstance(base, list) and isinstance(overlay, list):
        # Check if this array should merge by key
        array_name = path.split(".")[-1]
        if array_name in ARRAY_MERGE_KEYS:
            return merge_arrays_by_key(base, overlay, ARRAY_MERGE_KEYS[array_name])
        else:
            # Default: replace (existing YE.8 behavior for unknown arrays)
            return overlay

    else:
        # Scalars: overlay wins
        return overlay


def merge_arrays_by_key(base, overlay, keys):
    """Merge arrays by identifying key(s)."""
    result = []
    base_index = {}  # key_tuple -> index in result

    # Copy base elements to result
    for i, elem in enumerate(base):
        key_tuple = tuple(elem.get(k) for k in keys)
        base_index[key_tuple] = i
        result.append(elem.copy())

    # Process overlay elements
    for overlay_elem in overlay:
        key_tuple = tuple(overlay_elem.get(k) for k in keys)

        if overlay_elem.get("__delete__"):
            # Remove from result if exists
            if key_tuple in base_index:
                del result[base_index[key_tuple]]
                # Rebuild index (indices shifted)
                base_index = {tuple(e.get(k) for k in keys): i for i, e in enumerate(result)}
        elif key_tuple in base_index:
            # Merge with existing element
            idx = base_index[key_tuple]
            result[idx] = deep_merge(result[idx], overlay_elem)
        else:
            # Append new element
            result.append(overlay_elem)

    return result
```

### Testing Strategy

Test with realistic scenarios:

```yaml
# Fixture: base_multinode.yaml
name: test-agent
nodes:
  - name: fetch
    uses: http.get
    with:
      url: "{{ variables.api_url }}"
  - name: process
    uses: llm.call
    with:
      model: gpt-4o-mini
      prompt: "Process: {{ state.data }}"
  - name: validate
    run: |
      return {"valid": len(state.result) > 0}
  - name: save
    uses: memory.store
    with:
      key: "result"

edges:
  - from: __start__
    to: fetch
  - from: fetch
    to: process
  - from: process
    to: validate
    parallel: true
    fan_in: aggregate
  - from: validate
    to: save
  - from: save
    to: __end__
```

```yaml
# Fixture: overlay_modify_node.yaml
# Modifies process node to use different model
nodes:
  - name: process
    with:
      model: claude-3-opus
      temperature: 0.3
```

```yaml
# Fixture: overlay_add_delete.yaml
# Adds new node, deletes validate node
nodes:
  - name: log_request
    run: |
      print(f"Processing: {state.data}")
      return {}
  - name: validate
    __delete__: true
```

### Expected Test Results

| Test Case | Base Nodes | Overlay | Result Nodes |
|-----------|------------|---------|--------------|
| Modify one | 4 | 1 modified | 4 (one updated) |
| Add one | 4 | 1 new | 5 |
| Delete one | 4 | 1 deleted | 3 |
| Complex | 4 | 1 modified, 1 added, 1 deleted | 4 |

## Testing

### Test File Locations

- Python: `python/tests/test_cli_overlay.py` (extend existing)
- Rust: `rust/tests/test_cli_overlay.rs` (extend existing)
- Fixtures: `tests/fixtures/overlay/` (add new fixtures)
- Parity: `tests/test_overlay_parity.py` (extend)

### New Test Fixtures

```
tests/fixtures/overlay/
├── base_multinode.yaml          # Base with multiple nodes
├── overlay_modify_node.yaml     # Modify single node
├── overlay_add_node.yaml        # Add new node
├── overlay_delete_node.yaml     # Delete node with __delete__
├── overlay_modify_edge.yaml     # Modify single edge
├── overlay_modify_goto.yaml     # Modify goto routing
└── overlay_complex.yaml         # Multiple operations
```

### Test Matrix

| Scenario | Python | Rust | Parity |
|----------|--------|------|--------|
| Nodes merge by name | P0 | P0 | P0 |
| Edges merge by from+to | P0 | P0 | P0 |
| Goto merge by to | P1 | P1 | P1 |
| Nested object merge | P0 | P0 | P0 |
| Append non-matching | P0 | P0 | P0 |
| Delete with marker | P1 | P1 | P1 |
| Complex scenario | P1 | P1 | P1 |
| Empty overlay array | P2 | P2 | P2 |

**Total Tests:** ~20 Python + ~15 Rust + ~10 Parity = ~45 new tests

## Definition of Done

### Python Implementation
- [x] All Python-related acceptance criteria verified (AC 1-8, 10)
- [x] All Phase 1 tasks completed
- [x] Python tests pass (new + existing)
- [x] `deep_merge.py` enhanced with array key merge

### Rust Implementation
- [x] All Rust-related acceptance criteria verified (AC 1-8, 10)
- [x] All Phase 2 tasks completed
- [x] Rust tests pass (new + existing)
- [x] `deep_merge.rs` enhanced with array key merge

### Cross-Implementation
- [x] Parity tests pass (AC 9)
- [x] Both implementations produce identical `--dump-merged` output for array merges
- [x] Documentation updated (YAML_REFERENCE.md)
- [x] Migration guide created

## Rollback Procedure

If array merge functionality causes issues:

1. **Immediate Workaround:**
   - Users can use full array replacement by specifying all elements
   - Existing overlays continue to work (they just replace arrays)

2. **Code Rollback (Python):**
   - Revert `deep_merge.py` to YE.8 version (arrays replace)
   - Verification: `cd python && pytest tests/test_cli_overlay.py`

3. **Code Rollback (Rust):**
   - Revert `deep_merge.rs` to YE.8 version (arrays replace)
   - Verification: `cd rust && cargo test`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-18 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2026-01-18 | 0.2 | Added QA Results from test design assessment | Quinn (Test Architect) |
| 2026-01-18 | 1.0 | Implementation complete - Python, Rust, parity tests, documentation | James (Dev Agent) |

---

## QA Results

### Test Design Assessment

**Date:** 2026-01-18
**Reviewer:** Quinn (Test Architect)
**Assessment Document:** `docs/qa/assessments/YE.9-test-design-20260118.md`

#### Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 48 |
| Unit Tests | 24 (50%) |
| Integration Tests | 15 (31%) |
| E2E Tests | 9 (19%) |
| P0 (Critical) | 16 |
| P1 (High) | 20 |
| P2 (Medium) | 10 |
| P3 (Low) | 2 |

#### Risk Assessment

| Risk ID | Risk | Probability | Impact | Status |
|---------|------|-------------|--------|--------|
| R-001 | Incorrect merge key matching breaks existing overlays | Medium | High | Mitigated (8 tests) |
| R-002 | Breaking change causes user workflow failures | Medium | High | Mitigated (6 tests) |
| R-003 | Python/Rust implementation divergence | Low | Critical | Mitigated (9 tests) |
| R-004 | `__delete__` marker has unintended side effects | Medium | Medium | Mitigated (4 tests) |
| R-005 | Nested `goto` arrays not handled correctly | Medium | Medium | Mitigated (5 tests) |
| R-006 | Performance degradation on large arrays | Low | Medium | Mitigated (2 tests) |
| R-007 | Edge collision on `from`+`to` composite key | Low | High | Mitigated (3 tests) |

#### Coverage Analysis

- **All 12 Acceptance Criteria covered** with explicit test scenarios
- **Shift-left strategy applied**: 50% unit tests for fast feedback
- **Parity verification**: 9 dedicated E2E tests ensure Python/Rust identical behavior
- **No coverage gaps identified**

#### Key Test Focus Areas

1. **Core Merge Algorithm (P0)**: nodes by `name`, edges by `from`+`to`, goto by `to`
2. **Nested Deep Merge (P0)**: Recursive merge of matched elements
3. **Delete Marker (P0/P1)**: `__delete__: true` correctly removes elements
4. **Cross-Implementation Parity (P0)**: Python and Rust produce identical output
5. **Breaking Change Verification (P0)**: Default behavior is merge-by-key

#### Recommended Test Fixtures

| Fixture | Purpose |
|---------|---------|
| `base_multinode.yaml` | Base agent with 5+ nodes |
| `overlay_modify_single_node.yaml` | Modify one node by name |
| `overlay_modify_multiple_nodes.yaml` | Modify multiple nodes |
| `overlay_add_node.yaml` | Add new node |
| `overlay_delete_node.yaml` | Delete node with `__delete__` |
| `overlay_modify_edge.yaml` | Modify edge by from+to |
| `overlay_add_edge.yaml` | Add new edge |
| `overlay_delete_edge.yaml` | Delete edge |
| `overlay_modify_goto.yaml` | Modify goto routing |
| `overlay_nested_merge.yaml` | Deep nested object merge |
| `overlay_complex.yaml` | Multiple operations combined |
| `base_large_array.yaml` | 50+ nodes for scale testing |

#### Gate Status

**Test Design: PASS** - Ready for implementation

---

## Related

- **YE.8:** YAML Overlay Merge Support - Introduced overlay merging with array replacement
- **kubectl strategic merge patch:** Inspiration for array merge by key
- **Kustomize:** Similar array patching capabilities

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

- Python tests: 69 tests passed (38 existing + 31 new YE.9 tests)
- Rust tests: 18 tests passed (9 existing + 9 new YE.9 tests)
- Parity tests: 18 tests passed (9 existing + 9 new YE.9 parity tests)

### Completion Notes

Implemented deep array merge by identifying key for YAML overlays:

1. **Python Implementation** (`deep_merge.py`):
   - Added `ARRAY_MERGE_KEYS` dict mapping array names to their identifying keys
   - Added `GOTO_MERGE_KEY` for nested goto arrays
   - Added `DELETE_MARKER` constant (`__delete__`)
   - Implemented `_get_element_key()` helper function
   - Implemented `merge_arrays_by_key()` function
   - Added path tracking via `_deep_merge_impl()` for context-aware array handling

2. **Rust Implementation** (`deep_merge.rs`):
   - Added `get_array_merge_keys()` function (avoids lazy_static dependency)
   - Added `GOTO_MERGE_KEY` and `DELETE_MARKER` constants
   - Implemented `get_element_key()` helper function
   - Implemented `merge_arrays_by_key()` function
   - Added path tracking via `deep_merge_impl()` for context-aware array handling

3. **Breaking Change**: Arrays now merge by key by default instead of full replacement.

4. **Memory optimization**: Set Rust build to 8 parallel jobs via `.cargo/config.toml`.

### File List

**Python Source Files:**
- `python/src/the_edge_agent/schema/deep_merge.py` - Enhanced with array merge by key

**Rust Source Files:**
- `rust/src/engine/deep_merge.rs` - Enhanced with array merge by key

**Configuration Files:**
- `rust/.cargo/config.toml` - Created to limit parallel build jobs to 8

**Test Files:**
- `python/tests/test_deep_merge.py` - Added 31 new tests for YE.9
- `python/tests/test_cli_overlay.py` - Updated to expect YE.9 behavior
- `tests/test_overlay_parity.py` - Added 9 new parity tests

**Test Fixtures:**
- `tests/fixtures/overlay/base_multinode.yaml`
- `tests/fixtures/overlay/base_large_array.yaml`
- `tests/fixtures/overlay/base_with_goto.yaml`
- `tests/fixtures/overlay/overlay_modify_single_node.yaml`
- `tests/fixtures/overlay/overlay_modify_multiple_nodes.yaml`
- `tests/fixtures/overlay/overlay_add_node.yaml`
- `tests/fixtures/overlay/overlay_delete_node.yaml`
- `tests/fixtures/overlay/overlay_modify_edge.yaml`
- `tests/fixtures/overlay/overlay_add_edge.yaml`
- `tests/fixtures/overlay/overlay_delete_edge.yaml`
- `tests/fixtures/overlay/overlay_modify_goto.yaml`
- `tests/fixtures/overlay/overlay_nested_merge.yaml`
- `tests/fixtures/overlay/overlay_complex.yaml`
- `tests/fixtures/overlay/overlay_large_modify.yaml`

**Documentation Files:**
- `docs/shared/YAML_REFERENCE.md` - Added "Array Merge by Key (YE.9)" section
- `docs/articles/devops-yaml-overlay-agents.md` - Added migration guide section
