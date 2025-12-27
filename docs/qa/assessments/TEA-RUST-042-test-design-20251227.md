# Test Design: Story TEA-RUST-042

**Date:** 2025-12-27
**Designer:** Quinn (Test Architect)
**Story:** Lua State Preservation in Rust Runtime

## Test Strategy Overview

- **Total test scenarios:** 12
- **Unit tests:** 5 (42%)
- **Integration tests:** 5 (42%)
- **E2E tests:** 2 (16%)
- **Priority distribution:** P0: 4, P1: 6, P2: 2

## Problem Summary

The Rust runtime's Lua executor replaces the entire state with what Lua nodes return, instead of merging return values into existing state. This differs from Python runtime behavior (`state.update(result)`) and Rust Prolog runtime behavior (`collect_returns_from_context()`).

## Architecture Analysis: Python/Rust Parity

### Python Runtime Architecture (Reference)

The Python implementation uses a **two-layer approach**:

1. **`lua_runtime.py:372-426`** - `execute_node_code()` returns **only** what Lua returns:
   ```python
   def execute_node_code(self, code: str, state: Dict[str, Any]) -> Dict[str, Any]:
       # ... wraps code, executes, returns Lua result only
       return python_result  # No merge here
   ```

2. **`yaml_engine.py:1809`** - The engine does the merge:
   ```python
   if isinstance(result, dict):
       current_state.update(result)  # Merge happens here
   ```

### Rust Runtime Architecture (Current Bug)

The Rust implementation replaces state at `executor.rs:524-526`:
```rust
match self.execute_node(&current_node, &state) {
    Ok(new_state) => {
        state = new_state;  // Complete replacement, no merge!
    }
}
```

### Fix Options Analysis

| Option | Location | Behavior | Parity |
|--------|----------|----------|--------|
| **A (Recommended)** | `lua_runtime.rs::execute_node_code()` | Merge input + return | Matches Prolog pattern |
| **B** | `executor.rs` after node execution | Merge at executor level | Matches Python layer split |

**Recommendation:** Option A is cleaner as it keeps language-specific behavior encapsulated in the language runtime (matching Prolog's `collect_returns_from_context()` pattern).

### Critical Parity Test Point

For AC-3 (Python parity), the test must verify **end-to-end behavior** is identical, not internal implementation details. Both approaches result in the same final state.

## Test Scenarios by Acceptance Criteria

### AC-1: Lua node execution in Rust merges return values into existing state (not replaces)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-UNIT-001 | Unit | P0 | `test_lua_execute_node_code_merges_state` - Verify `execute_node_code()` returns merged state when Lua returns partial object | Core fix validation - this is the exact behavior being changed in `lua_runtime.rs:415-443` |
| 042-UNIT-002 | Unit | P0 | `test_lua_merge_with_empty_return` - Verify original state preserved when Lua returns empty table `{}` | Edge case: empty return should preserve all original state |
| 042-UNIT-003 | Unit | P1 | `test_lua_merge_with_nil_return` - Verify behavior when Lua returns `nil` | Edge case: `nil` return handling (should preserve state or return error) |

**Test Code Location:** `rust/tests/test_lua_state_preservation.rs`

**Scenario Details:**

```rust
// 042-UNIT-001: Core merge behavior
#[test]
fn test_lua_execute_node_code_merges_state() {
    let runtime = LuaRuntime::new().unwrap();
    let state = json!({"text": "hello", "person": "bob", "count": 5});

    // Lua returns only partial state
    let code = r#"return {processed = true}"#;

    let result = runtime.execute_node_code(code, &state).unwrap();

    // Original fields preserved
    assert_eq!(result["text"], json!("hello"));
    assert_eq!(result["person"], json!("bob"));
    assert_eq!(result["count"], json!(5));
    // New field added
    assert_eq!(result["processed"], json!(true));
}
```

---

### AC-2: Original state fields are preserved unless explicitly overwritten by Lua return

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-UNIT-004 | Unit | P0 | `test_lua_explicit_overwrite` - Verify Lua can overwrite existing field | Confirm merge is not append-only; explicit overwrites work |
| 042-UNIT-005 | Unit | P1 | `test_lua_nested_object_merge` - Verify nested objects are handled correctly | Nested object merging behavior must be defined (shallow vs deep merge) |

**Scenario Details:**

```rust
// 042-UNIT-004: Explicit overwrite
#[test]
fn test_lua_explicit_overwrite() {
    let runtime = LuaRuntime::new().unwrap();
    let state = json!({"count": 5, "name": "original"});

    // Lua explicitly returns count with new value
    let code = r#"return {count = 10}"#;

    let result = runtime.execute_node_code(code, &state).unwrap();

    // count should be overwritten
    assert_eq!(result["count"], json!(10));
    // name should be preserved
    assert_eq!(result["name"], json!("original"));
}
```

---

### AC-3: Behavior matches Python runtime's `state.update(result)` pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-INT-001 | Integration | P0 | `test_rust_python_parity_lua_state_merge` - Run same YAML with both runtimes, compare final state | Cross-runtime parity is critical for user expectations |
| 042-INT-002 | Integration | P1 | `test_multi_node_state_accumulation` - Multi-node workflow where each node adds to state | Real-world usage pattern |

**Test YAML for Parity Test:**

```yaml
# test_fixtures/lua_state_parity.yaml
name: lua-state-parity-test
state_schema:
  initial_value: str
  step1_added: bool
  step2_added: bool
  final_check: str

nodes:
  - name: step1
    language: lua
    run: |
      return {step1_added = true}

  - name: step2
    language: lua
    run: |
      return {step2_added = true}

  - name: verify
    language: lua
    run: |
      -- All fields should be present
      if state.initial_value and state.step1_added and state.step2_added then
        return {final_check = "all_present"}
      else
        return {final_check = "missing_fields"}
      end

edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: verify
  - from: verify
    to: __end__
```

**Test Location:** `rust/tests/test_cli_parity.rs` (add to existing parity tests)

**Python Parity Test Implementation (042-INT-001):**

This test should run both Python and Rust runtimes and compare final state:

```python
# python/tests/test_parity_lua_state.py
"""
TEA-RUST-042 Parity Test: Lua State Preservation

Verifies Python and Rust runtimes produce identical state
when running multi-node Lua workflows.
"""
import subprocess
import json
import pytest
from the_edge_agent.yaml_engine import YamlEngine

PARITY_YAML = """
name: lua-state-parity-test
state_schema:
  initial_value: str
  step1_added: bool
  step2_added: bool
  final_check: str

nodes:
  - name: step1
    language: lua
    run: |
      return {step1_added = true}

  - name: step2
    language: lua
    run: |
      return {step2_added = true}

  - name: verify
    language: lua
    run: |
      if state.initial_value and state.step1_added and state.step2_added then
        return {final_check = "all_present"}
      else
        return {final_check = "missing_fields"}
      end

edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: verify
  - from: verify
    to: __end__
"""

def test_python_lua_state_preservation():
    """Verify Python runtime preserves Lua state across nodes."""
    engine = YamlEngine.from_yaml(PARITY_YAML)
    graph = engine.build()

    initial_state = {"initial_value": "test_input"}

    final_state = None
    for event in graph.compile().invoke(initial_state):
        if hasattr(event, 'state'):
            final_state = event.state

    # Python should preserve all state fields
    assert final_state["initial_value"] == "test_input"
    assert final_state["step1_added"] == True
    assert final_state["step2_added"] == True
    assert final_state["final_check"] == "all_present"

def test_rust_python_parity_lua_state(tmp_path):
    """Compare Python and Rust outputs for same YAML workflow."""
    # Write YAML to temp file
    yaml_file = tmp_path / "parity_test.yaml"
    yaml_file.write_text(PARITY_YAML)

    # Run Python
    python_result = run_python_runtime(yaml_file, {"initial_value": "test"})

    # Run Rust
    rust_result = run_rust_runtime(yaml_file, {"initial_value": "test"})

    # Compare final states
    assert python_result["final_check"] == rust_result["final_check"]
    assert python_result["final_check"] == "all_present", \
        f"State lost! Python: {python_result}, Rust: {rust_result}"
```

---

### AC-4: Behavior matches Rust Prolog runtime's state preservation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-INT-003 | Integration | P1 | `test_lua_prolog_parity` - Run equivalent workflows in Lua and Prolog, verify same state behavior | Internal consistency within Rust runtime |

**Comparison Point:** Prolog's `collect_returns_from_context()` at `prolog_runtime.rs:884-895` starts with input state cache and adds return values.

---

### AC-5: Existing tests continue to pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-INT-004 | Integration | P1 | Run full `cargo test --features prolog` test suite | Regression prevention |
| 042-INT-005 | Integration | P1 | Verify `test_lua_runtime.rs` tests still pass | Existing Lua tests must not break |

**Execution:** `cd rust && LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux cargo test --features prolog`

---

### AC-6: Add regression test for multi-node Lua state preservation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 042-E2E-001 | E2E | P1 | `test_hero_family_reasoning_lua_state` - Run `hero-family-reasoning.yaml` end-to-end | Real-world workflow that depends on Lua state preservation |
| 042-E2E-002 | E2E | P2 | `test_interruptible_workflow_lua_state` - Workflow with interrupts preserving Lua state across checkpoint restore | State preservation across workflow lifecycle |

**Test Location:** `rust/tests/test_lua_state_preservation.rs` (new file)

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| State loss in production workflows | 042-UNIT-001, 042-INT-002, 042-E2E-001 | Core fix + multi-node + real-world test |
| Python parity regression | 042-INT-001 | Direct comparison with Python output |
| Prolog parity regression | 042-INT-003 | Internal consistency validation |
| Breaking existing Lua code | 042-INT-004, 042-INT-005 | Full regression suite |
| Edge case: nil/empty returns | 042-UNIT-002, 042-UNIT-003 | Explicit edge case tests |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core fix)
   - 042-UNIT-001: Core merge behavior
   - 042-UNIT-002: Empty return handling
   - 042-UNIT-004: Explicit overwrite

2. **P0 Integration tests**
   - 042-INT-001: Python parity

3. **P1 Unit tests**
   - 042-UNIT-003: Nil return handling
   - 042-UNIT-005: Nested object merge

4. **P1 Integration tests**
   - 042-INT-002: Multi-node accumulation
   - 042-INT-003: Prolog parity
   - 042-INT-004, 042-INT-005: Regression suite

5. **P1/P2 E2E tests**
   - 042-E2E-001: Real-world workflow
   - 042-E2E-002: Interrupt handling

---

## Test Implementation Recommendations

### New Test File: `rust/tests/test_lua_state_preservation.rs`

```rust
//! Regression tests for TEA-RUST-042: Lua State Preservation
//!
//! These tests verify that Lua node execution merges return values
//! into existing state rather than replacing the entire state.

use serde_json::json;
use the_edge_agent::engine::lua_runtime::LuaRuntime;

mod state_merge_tests {
    use super::*;

    #[test]
    fn test_lua_execute_node_code_merges_state() {
        // 042-UNIT-001
    }

    #[test]
    fn test_lua_merge_with_empty_return() {
        // 042-UNIT-002
    }

    #[test]
    fn test_lua_merge_with_nil_return() {
        // 042-UNIT-003
    }

    #[test]
    fn test_lua_explicit_overwrite() {
        // 042-UNIT-004
    }

    #[test]
    fn test_lua_nested_object_merge() {
        // 042-UNIT-005
    }
}

mod multi_node_tests {
    use super::*;

    #[test]
    fn test_multi_node_state_accumulation() {
        // 042-INT-002: Uses YamlEngine to run multi-node workflow
    }
}
```

### Test Fixture: `rust/tests/fixtures/lua_state_preservation.yaml`

```yaml
name: lua-state-preservation-regression
description: |
  TEA-RUST-042 regression test: Verifies state is preserved
  across multiple Lua node executions.

state_schema:
  input_text: str
  step1_result: bool
  step2_result: bool
  step3_result: str

nodes:
  - name: step1
    language: lua
    run: |
      -- Only return new field, don't touch input_text
      return {step1_result = true}

  - name: step2
    language: lua
    run: |
      -- Add another field without touching previous
      return {step2_result = true}

  - name: step3
    language: lua
    run: |
      -- Verify all previous state is accessible
      local all_present = state.input_text and state.step1_result and state.step2_result
      if all_present then
        return {step3_result = "all_preserved"}
      else
        return {step3_result = "state_lost"}
      end

edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: step3
  - from: step3
    to: __end__
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-042
  scenarios_total: 12
  by_level:
    unit: 5
    integration: 5
    e2e: 2
  by_priority:
    p0: 4
    p1: 6
    p2: 2
  coverage_gaps: []
  key_scenarios:
    - id: 042-UNIT-001
      criticality: P0
      description: Core merge behavior validation
    - id: 042-INT-001
      criticality: P0
      description: Python parity comparison
  designer: Quinn (Test Architect)
  date: 2025-12-27
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-RUST-042-test-design-20251227.md
P0 tests identified: 4
P1 tests identified: 6
P2 tests identified: 2
Total scenarios: 12
```
