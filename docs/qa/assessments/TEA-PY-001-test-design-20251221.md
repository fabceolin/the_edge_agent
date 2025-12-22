# Test Design: Story TEA-PY-001

**Date:** 2025-12-21
**Designer:** Quinn (Test Architect)
**Story:** Implement Lua Scripting Support in Python TEA

---

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 32 |
| **Unit tests** | 22 (69%) |
| **Integration tests** | 7 (22%) |
| **E2E tests** | 3 (9%) |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0** | 12 | Critical - must pass before merge |
| **P1** | 14 | High - core functionality validation |
| **P2** | 6 | Medium - edge cases and polish |

### Shift-Left Strategy

This test design follows a **shift-left approach** with 69% unit tests to enable fast feedback. The story involves a new module (`lua_runtime.py`) with clear boundaries, making it ideal for comprehensive unit testing.

---

## Test Scenarios by Acceptance Criteria

### AC-1: Explicit Lua `run: { type: lua, code: "..." }`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-UNIT-001 | Unit | P0 | `LuaRuntime.execute_node_code()` executes Lua and returns state updates | Core execution logic |
| TEA-PY-001-UNIT-002 | Unit | P1 | `execute_node_code()` with empty state returns empty dict | Boundary condition |
| TEA-PY-001-UNIT-003 | Unit | P1 | `execute_node_code()` with complex nested state works correctly | Data structure handling |
| TEA-PY-001-INT-001 | Integration | P0 | YAMLEngine parses `run: { type: lua, code: ... }` and executes via LuaRuntime | YAML→Lua integration |

### AC-2: Auto-detected Lua via `--lua` marker

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-UNIT-004 | Unit | P1 | Language detection identifies `--lua` comment at start of code | Heuristic validation |
| TEA-PY-001-INT-002 | Integration | P1 | YAMLEngine auto-detects Lua when code starts with `-- lua` marker | End-to-end detection |

### AC-3: `language: lua` global and per-node

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-INT-003 | Integration | P0 | YAMLEngine with global `language: lua` uses Lua for all nodes | Global config |
| TEA-PY-001-INT-004 | Integration | P0 | Node-level `language: lua` overrides global Python | Per-node override |
| TEA-PY-001-INT-005 | Integration | P1 | Mixed Python/Lua nodes in same workflow execute correctly | Multi-language support |

### AC-4: Timeout protection

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-UNIT-005 | Unit | P0 | Infinite loop (`while true do end`) raises `TimeoutError` | Security critical |
| TEA-PY-001-UNIT-006 | Unit | P0 | Timeout error message contains "Lua execution timeout" | Error contract |
| TEA-PY-001-UNIT-007 | Unit | P1 | Normal execution within timeout completes successfully | No false positives |
| TEA-PY-001-UNIT-008 | Unit | P1 | Configurable timeout (e.g., 0.1s) is respected | Configuration works |
| TEA-PY-001-UNIT-009 | Unit | P2 | Timeout reliability: 10 consecutive runs all timeout correctly | Stability |

### AC-5: Sandbox removes dangerous globals

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-UNIT-010 | Unit | P0 | `os` global returns `nil` after sandbox | Security critical |
| TEA-PY-001-UNIT-011 | Unit | P0 | `io` global returns `nil` after sandbox | Security critical |
| TEA-PY-001-UNIT-012 | Unit | P0 | `loadfile` global returns `nil` after sandbox | Security critical |
| TEA-PY-001-UNIT-013 | Unit | P0 | `dofile` global returns `nil` after sandbox | Security critical |
| TEA-PY-001-UNIT-014 | Unit | P0 | `debug` global returns `nil` after sandbox | Security critical |
| TEA-PY-001-UNIT-015 | Unit | P1 | Sandbox can be disabled via `sandbox=False` | Configuration flexibility |
| TEA-PY-001-UNIT-016 | Unit | P2 | Safe globals (`string`, `table`, `math`, `pairs`, `ipairs`) remain available | Usability |

### AC-6: Python `run:` backward compatibility

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-INT-006 | Integration | P0 | Existing Python `run:` block continues to work with `language: python` | Regression prevention |
| TEA-PY-001-E2E-001 | E2E | P1 | Existing YAML agents without `language:` key work unchanged | Backward compatibility |

### AC-7: Jinja2 condition evaluation unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-INT-007 | Integration | P1 | `when:` expressions with Jinja2 syntax still work with Lua nodes | Integration validation |

### AC-8: Cross-runtime parity (Python ↔ Rust)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-E2E-002 | E2E | P0 | Same Lua YAML produces identical output in Python and Rust TEA | Cross-runtime parity |
| TEA-PY-001-E2E-003 | E2E | P1 | Portable Lua syntax subset works identically in both runtimes | Compatibility validation |

### AC-9: JSON↔Lua conversion

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-PY-001-UNIT-017 | Unit | P0 | `json_to_lua(None)` returns Lua `nil` | Type mapping |
| TEA-PY-001-UNIT-018 | Unit | P0 | `json_to_lua(True/False)` returns Lua boolean | Type mapping |
| TEA-PY-001-UNIT-019 | Unit | P0 | `json_to_lua(42)` and `json_to_lua(3.14)` return Lua number | Type mapping |
| TEA-PY-001-UNIT-020 | Unit | P0 | `json_to_lua("hello")` returns Lua string | Type mapping |
| TEA-PY-001-UNIT-021 | Unit | P1 | `json_to_lua([1,2,3])` returns 1-indexed Lua table | Array conversion |
| TEA-PY-001-UNIT-022 | Unit | P1 | `json_to_lua({"a": 1})` returns Lua table with string keys | Object conversion |
| TEA-PY-001-UNIT-023 | Unit | P1 | `lua_to_json()` reverse conversion for all types | Bidirectional |
| TEA-PY-001-UNIT-024 | Unit | P2 | Deeply nested structures convert correctly | Edge case |
| TEA-PY-001-UNIT-025 | Unit | P2 | Empty array `[]` vs empty object `{}` distinction preserved | Ambiguity handling |

### AC-10, AC-11: Test coverage (meta-requirements)

These ACs are satisfied by implementing the test scenarios above.

### AC-12, AC-13: Documentation (not tested)

Documentation requirements verified via manual review.

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| **Infinite loop hangs system** | TEA-PY-001-UNIT-005, UNIT-006, UNIT-009 |
| **Sandbox bypass (code execution)** | TEA-PY-001-UNIT-010 through UNIT-014 |
| **Backward compatibility break** | TEA-PY-001-INT-006, E2E-001 |
| **Cross-runtime incompatibility** | TEA-PY-001-E2E-002, E2E-003 |
| **JSON conversion data loss** | TEA-PY-001-UNIT-017 through UNIT-025 |
| **Platform-specific timeout failure** | TEA-PY-001-UNIT-009 (reliability) |

---

## Platform-Specific Test Notes

### Timeout Mechanism

| Platform | Mechanism | Test Consideration |
|----------|-----------|-------------------|
| **POSIX (Linux/macOS)** | `signal.SIGALRM` | Standard timeout tests |
| **Windows** | `threading.Timer` | Separate test suite or skip |

**Recommendation:** Add `@pytest.mark.skipif(sys.platform == 'win32')` for SIGALRM-dependent tests if Windows support is deferred.

---

## Recommended Execution Order

1. **P0 Unit Tests** (fail fast on core issues)
   - Sandbox tests (UNIT-010 to UNIT-014)
   - Timeout tests (UNIT-005, UNIT-006)
   - JSON conversion (UNIT-017 to UNIT-020)
   - Core execution (UNIT-001)

2. **P0 Integration Tests**
   - YAML parsing (INT-001)
   - Language configuration (INT-003, INT-004)
   - Backward compatibility (INT-006)

3. **P0 E2E Tests**
   - Cross-runtime parity (E2E-002)

4. **P1 Tests** (in order by ID)

5. **P2 Tests** (as time permits)

---

## Test Data Requirements

### Fixtures Needed

```yaml
# fixtures/lua-agents/simple-lua-agent.yaml
name: simple-lua-test
language: lua
state_schema:
  count: int
  result: str
nodes:
  - name: increment
    run: |
      return { count = state.count + 1, result = "done" }
edges:
  - from: __start__
    to: increment
  - from: increment
    to: __end__
```

```yaml
# fixtures/lua-agents/mixed-language-agent.yaml
name: mixed-language-test
state_schema:
  value: int
nodes:
  - name: lua_step
    language: lua
    run: |
      return { value = state.value * 2 }
  - name: python_step
    language: python
    run: |
      return {"value": state["value"] + 10}
edges:
  - from: __start__
    to: lua_step
  - from: lua_step
    to: python_step
  - from: python_step
    to: __end__
```

### Mock Data for JSON Conversion

```python
# test_lua_runtime.py fixtures
JSON_TEST_CASES = [
    (None, "nil"),
    (True, "true"),
    (False, "false"),
    (42, "42"),
    (3.14, "3.14"),
    ("hello", '"hello"'),
    ([1, 2, 3], "{1, 2, 3}"),  # 1-indexed
    ({"a": 1, "b": 2}, '{a=1, b=2}'),
]
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-PY-001
  date: 2025-12-21
  designer: Quinn (Test Architect)
  scenarios_total: 32
  by_level:
    unit: 22
    integration: 7
    e2e: 3
  by_priority:
    p0: 12
    p1: 14
    p2: 6
  coverage_gaps: []
  risk_mitigations:
    - risk: "Infinite loop hangs system"
      tests: ["UNIT-005", "UNIT-006", "UNIT-009"]
    - risk: "Sandbox bypass"
      tests: ["UNIT-010", "UNIT-011", "UNIT-012", "UNIT-013", "UNIT-014"]
    - risk: "Backward compatibility break"
      tests: ["INT-006", "E2E-001"]
    - risk: "Cross-runtime incompatibility"
      tests: ["E2E-002", "E2E-003"]
  platform_notes: "Windows timeout tests may require separate implementation using threading.Timer"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (shift-left: 69% unit)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to specific tests
- [x] Platform-specific considerations documented

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-PY-001-test-design-20251221.md
P0 tests identified: 12
P1 tests identified: 14
P2 tests identified: 6
```

---

## Recommendations

### Must Implement (P0)

1. **Sandbox tests** - Security critical, must pass before any release
2. **Timeout tests** - Prevents DoS via infinite loops
3. **Core JSON conversion** - Data integrity across runtime boundary
4. **Cross-runtime E2E** - Primary story goal validation

### Should Implement (P1)

1. **Mixed language workflows** - Key differentiator feature
2. **Backward compatibility E2E** - Regression prevention
3. **All JSON edge cases** - Data fidelity

### Nice to Have (P2)

1. **Timeout reliability (10 runs)** - Confidence building
2. **Nested structure conversion** - Edge case coverage
3. **Empty array/object distinction** - Corner case

---

## Appendix: Rust Test Mapping

Tests mirror the Rust implementation in `rust/src/engine/lua_runtime.rs` for cross-runtime parity:

| Rust Test | Python Test |
|-----------|-------------|
| `test_create_runtime` | `test_create_runtime` |
| `test_json_to_lua_null` | `test_null_to_nil` |
| `test_json_to_lua_boolean` | `test_boolean_true`, `test_boolean_false` |
| `test_json_to_lua_integer` | `test_integer` |
| `test_json_to_lua_float` | `test_float` |
| `test_json_to_lua_string` | `test_string` |
| `test_json_to_lua_object` | `test_object_access` |
| `test_json_to_lua_nested_object` | `test_nested_object` |
| `test_json_to_lua_array` | `test_array_access` |
| `test_eval_condition_string` | `test_eval_condition_string` |
| `test_eval_condition_nil` | `test_eval_condition_nil` |
| `test_eval_condition_boolean` | `test_eval_condition_boolean_true/false` |
| `test_eval_condition_with_logic` | `test_eval_condition_with_logic` |
| `test_sandbox_removes_os` | `test_sandbox_removes_os` |
| `test_sandbox_removes_io` | `test_sandbox_removes_io` |
| `test_sandbox_removes_debug` | `test_sandbox_removes_debug` |
| `test_sandbox_allows_string_library` | `test_sandbox_allows_string` |
| `test_sandbox_allows_math_library` | `test_sandbox_allows_math` |
| `test_sandbox_allows_table_library` | `test_sandbox_allows_table` |
| `test_execute_node_code` | `test_execute_node_code_basic` |
| `test_return_table` | `test_return_array`, `test_return_object` |
| `test_ipairs_iteration` | `test_ipairs_iteration` |
| `test_timeout` | `test_timeout_triggers` |
