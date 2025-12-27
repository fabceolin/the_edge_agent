# TEA-RUST-042: Lua State Preservation in Rust Runtime

## Story Metadata

| Field | Value |
|-------|-------|
| **Story ID** | TEA-RUST-042 |
| **Epic** | TEA-RUST (Rust Runtime) |
| **Type** | Bug Fix |
| **Status** | Draft |
| **Priority** | P1 - Should Have |
| **Effort** | 2-3 hours |

## Story

**As a** TEA agent developer using Lua scripting in the Rust runtime,
**I want** state to be automatically preserved across Lua node executions,
**so that** I don't have to manually return all state fields in every Lua node (parity with Python runtime).

## Problem Statement

The Rust runtime's Lua executor replaces the entire state with what Lua nodes return, instead of merging the return values into existing state. This differs from:
- **Python runtime**: Uses `current_state.update(result)` to merge
- **Rust Prolog runtime**: Uses `collect_returns_from_context()` which starts with input state and adds return values

**Current behavior (Rust Lua):**
```yaml
# Node 1: state = {text: "hello", person: "bob"}
- name: step1
  language: lua
  run: return {processed = true}
# After: state = {processed: true}  # text and person LOST!
```

**Expected behavior (matching Python):**
```yaml
# After: state = {text: "hello", person: "bob", processed: true}  # MERGED
```

## Acceptance Criteria

1. **AC-1**: Lua node execution in Rust merges return values into existing state (not replaces)
2. **AC-2**: Original state fields are preserved unless explicitly overwritten by Lua return
3. **AC-3**: Behavior matches Python runtime's `state.update(result)` pattern
4. **AC-4**: Behavior matches Rust Prolog runtime's state preservation
5. **AC-5**: Existing tests continue to pass
6. **AC-6**: Add regression test for multi-node Lua state preservation

## Tasks / Subtasks

- [ ] **Task 1**: Modify Lua state handling in executor (AC: 1, 2)
  - [ ] In `executor.rs`, after calling `lua.execute_node_code()`, merge result into input state
  - [ ] Use pattern: `state.as_object_mut().extend(result.as_object())` or similar

- [ ] **Task 2**: Verify Prolog parity (AC: 4)
  - [ ] Confirm Prolog's `collect_returns_from_context()` pattern is correct reference

- [ ] **Task 3**: Add regression test (AC: 6)
  - [ ] Test multi-node Lua workflow that reads state set by previous nodes
  - [ ] Test state set in input is preserved after Lua node execution

- [ ] **Task 4**: Verify Python parity (AC: 3)
  - [ ] Run same YAML with both runtimes, verify identical state output

- [ ] **Task 5**: Run existing tests (AC: 5)
  - [ ] `cargo test --features prolog`

## Dev Notes

### Root Cause Location

**File**: `rust/src/engine/executor.rs`
- Lines 524-526: `state = new_state` replaces entire state

**File**: `rust/src/engine/lua_runtime.rs`
- Lines 415-441: `execute_node_code()` returns only Lua return value, not merged state

### Reference Implementation (Prolog - correct behavior)

**File**: `rust/src/engine/prolog_runtime.rs`
- Lines 884-895: `collect_returns_from_context()` starts with input state cache and adds return values:
```rust
let mut result = serde_json::Map::new();
// Start with input state
for (k, v) in cache.iter() {
    result.insert(k.clone(), v.clone());
}
// Then add return values...
```

### Suggested Fix

**Option A** (Recommended): Modify `lua_runtime.rs` to match Prolog pattern:
```rust
pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
    // Execute Lua code
    let result = /* current execution */;

    // Merge: start with input state, overlay with result
    let mut merged = state.clone();
    if let (Some(base), Some(updates)) = (merged.as_object_mut(), result.as_object()) {
        for (k, v) in updates {
            base.insert(k.clone(), v.clone());
        }
    }
    Ok(merged)
}
```

**Option B**: Modify `executor.rs` to merge after any node execution (but this may affect action handlers differently).

### Python Reference

**File**: `python/src/the_edge_agent/yaml_engine.py`
- Line 1688: `lua_runtime.execute_node_code(code, state_dict)` returns result
- Line 1809: `current_state.update(result)` merges result into existing state

### Testing

| Test | Location | Purpose |
|------|----------|---------|
| Unit test | `rust/tests/lua_state_preservation.rs` | Multi-node state preservation |
| Integration | Run `examples/prolog/neurosymbolic/hero-family-reasoning.yaml` | End-to-end verification |

**Testing Command:**
```bash
cd rust && LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux cargo test --features prolog
```

### Workaround (Current)

Until this is fixed, Lua nodes must explicitly return all state fields:
```lua
return {
  text = state.text,           -- Preserve original
  person = state.person,       -- Preserve original
  processed = true             -- Add new field
}
```

## Verification Checklist

- [ ] All existing Lua tests pass
- [ ] New regression test added and passes
- [ ] hero-family-reasoning.yaml works without explicit state preservation in Lua
- [ ] Python and Rust produce identical state output for same YAML

## Definition of Done

- [ ] Code changes merged to main branch
- [ ] All acceptance criteria verified
- [ ] Regression test added
- [ ] Documentation updated if needed

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-26 | 1.0 | Initial story creation | Sarah (PO Agent) |
