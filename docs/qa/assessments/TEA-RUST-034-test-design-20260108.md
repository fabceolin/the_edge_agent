# Test Design: Story TEA-RUST-034

**Date:** 2026-01-08
**Designer:** Quinn (Test Architect)
**Story:** External Imports Support (Rust Implementation)

## Test Strategy Overview

- **Total test scenarios:** 38
- **Unit tests:** 20 (53%)
- **Integration tests:** 14 (37%)
- **E2E tests:** 4 (10%)
- **Priority distribution:** P0: 12, P1: 16, P2: 8, P3: 2

## Risk Assessment Summary

| Risk Area | Impact | Probability | Priority |
|-----------|--------|-------------|----------|
| Path resolution errors | High | Medium | P0 |
| Lua runtime errors | High | Medium | P0 |
| Namespace collisions | Medium | Medium | P1 |
| Circular imports | Low | Low | P2 |
| Missing builtin sets | High | Low | P1 |

## Test Scenarios by Acceptance Criteria

### AC-1: Relative Path Loading

**Requirement:** `path: ./relative/path.lua` loads Lua file relative to YAML location

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-001 | Unit | P0 | Resolve `./actions/custom.lua` relative to YAML directory | Pure path resolution logic |
| TEA-RUST-034-UNIT-002 | Unit | P0 | Resolve `../shared/actions.lua` with parent directory | Path traversal handling |
| TEA-RUST-034-UNIT-003 | Unit | P1 | Resolve `actions/sub/deep.lua` without leading `./` | Implicit relative path |
| TEA-RUST-034-INT-001 | Integration | P0 | Load Lua file from relative path and execute action | File system + Lua runtime integration |

### AC-2: Absolute Path Loading

**Requirement:** `path: /absolute/path.lua` loads Lua file from absolute path

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-004 | Unit | P0 | Accept and use absolute path directly | Path resolution bypass logic |
| TEA-RUST-034-INT-002 | Integration | P1 | Load Lua file from absolute path | File system integration |

### AC-3: Lua Module Contract

**Requirement:** Lua module must export `register_actions(registry)` function

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-005 | Unit | P0 | Detect missing `register_actions` function | Contract validation logic |
| TEA-RUST-034-UNIT-006 | Unit | P1 | Handle `register_actions` that is not a function | Type validation |
| TEA-RUST-034-INT-003 | Integration | P0 | Call `register_actions` with registry proxy | Lua-Rust bridge integration |

### AC-4: Namespace Prefixing for Lua Actions

**Requirement:** Actions registered in Lua are available with namespace prefix

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-007 | Unit | P0 | Prefix action name with namespace | String manipulation logic |
| TEA-RUST-034-INT-004 | Integration | P0 | Register Lua action and invoke as `ns.action` | Full registration flow |

### AC-5: File Not Found Error

**Requirement:** Error if file not found with clear message including resolved path

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-008 | Unit | P0 | Generate error with full resolved path for missing file | Error message formatting |
| TEA-RUST-034-UNIT-009 | Unit | P1 | Include original path and resolved path in error | Debug info inclusion |
| TEA-RUST-034-INT-005 | Integration | P1 | Workflow fails with clear error when Lua file missing | End-to-end error flow |

### AC-6: Missing register_actions Error

**Requirement:** Error if Lua file doesn't have `register_actions` function

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-010 | Unit | P0 | Return error when `register_actions` not found in globals | Lua introspection logic |
| TEA-RUST-034-INT-006 | Integration | P1 | Workflow fails gracefully with invalid Lua module | Lua runtime error handling |

### AC-7: Circular Import Detection

**Requirement:** Circular imports are detected and skipped (with debug log)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-011 | Unit | P1 | Track loaded paths in HashSet | State management logic |
| TEA-RUST-034-UNIT-012 | Unit | P2 | Skip already-loaded path without error | Idempotency logic |
| TEA-RUST-034-INT-007 | Integration | P2 | Import same file twice, second is skipped | Full circular detection |

### AC-8 to AC-10: Built-in Action Sets

**Requirement:** `builtin: web/memory/llm` registers respective actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-013 | Unit | P0 | Map "web" to web module registration | Dispatch logic |
| TEA-RUST-034-UNIT-014 | Unit | P0 | Map "memory" to memory module registration | Dispatch logic |
| TEA-RUST-034-UNIT-015 | Unit | P1 | Map "llm" to LLM module registration | Dispatch logic |
| TEA-RUST-034-UNIT-016 | Unit | P1 | Map "json" to JSON module registration | Dispatch logic |
| TEA-RUST-034-INT-008 | Integration | P0 | Load builtin web and invoke fetch action | Module initialization + execution |
| TEA-RUST-034-INT-009 | Integration | P1 | Load builtin memory and invoke store action | Module initialization + execution |

### AC-11: Unknown Builtin Error

**Requirement:** Unknown builtin name produces clear error message

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-017 | Unit | P0 | Return error listing available builtins for unknown name | Error message quality |
| TEA-RUST-034-INT-010 | Integration | P1 | Workflow fails with helpful error for `builtin: invalid` | End-to-end error flow |

### AC-12: Builtin Namespace Prefixing

**Requirement:** Built-in actions respect namespace prefix

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-018 | Unit | P0 | Pass namespace to builtin registration | Parameter forwarding |
| TEA-RUST-034-INT-011 | Integration | P0 | Access builtin action as `http.fetch` after `namespace: http` | Namespace application |

### AC-13: Namespace Prefix Format

**Requirement:** `namespace: foo` prefixes all actions as `foo.action_name`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-019 | Unit | P1 | Format prefix as `{namespace}.{action}` | String formatting |

### AC-14: Empty Namespace Handling

**Requirement:** Empty/missing namespace registers actions at root level

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-UNIT-020 | Unit | P1 | Empty namespace results in no prefix | Conditional logic |
| TEA-RUST-034-INT-012 | Integration | P2 | Action accessible at root without namespace | Registration behavior |

### AC-15: Namespace Conflict Warning

**Requirement:** Namespace conflicts produce warning (last import wins)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-INT-013 | Integration | P2 | Log warning when overwriting existing action | Side effect verification |
| TEA-RUST-034-E2E-001 | E2E | P3 | Console shows warning during workflow load | User-facing warning |

### AC-16 to AC-19: Integration with Workflow

**Requirement:** Imports processed before execution, actions available in workflow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-INT-014 | Integration | P0 | Complete workflow with imported Lua action | Full workflow integration |
| TEA-RUST-034-E2E-002 | E2E | P0 | CLI `tea run` executes workflow with imports | User journey validation |
| TEA-RUST-034-E2E-003 | E2E | P1 | Workflow with mixed Lua and builtin imports | Complex scenario |

### AC-20 to AC-22: Error Collection and Reporting

**Requirement:** All errors collected, include source location, fail fast

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-034-INT-015 | Integration | P0 | Multiple import errors reported together | Error aggregation |
| TEA-RUST-034-INT-016 | Integration | P1 | Error messages include path/builtin source | Error attribution |
| TEA-RUST-034-E2E-004 | E2E | P2 | CLI shows all import errors in single output | User experience |

## Test Coverage Summary

### By Acceptance Criterion

| AC | Unit | Integration | E2E | Total |
|----|------|-------------|-----|-------|
| AC-1 | 3 | 1 | 0 | 4 |
| AC-2 | 1 | 1 | 0 | 2 |
| AC-3 | 2 | 1 | 0 | 3 |
| AC-4 | 1 | 1 | 0 | 2 |
| AC-5 | 2 | 1 | 0 | 3 |
| AC-6 | 1 | 1 | 0 | 2 |
| AC-7 | 2 | 1 | 0 | 3 |
| AC-8-10 | 4 | 2 | 0 | 6 |
| AC-11 | 1 | 1 | 0 | 2 |
| AC-12 | 1 | 1 | 0 | 2 |
| AC-13 | 1 | 0 | 0 | 1 |
| AC-14 | 1 | 1 | 0 | 2 |
| AC-15 | 0 | 1 | 1 | 2 |
| AC-16-19 | 0 | 1 | 2 | 3 |
| AC-20-22 | 0 | 2 | 1 | 3 |

### Coverage Gaps

- None identified. All 22 ACs have test coverage.

## Test Fixtures Required

### Lua Test Files

```
rust/tests/fixtures/
├── custom_actions.lua          # Valid module with register_actions
├── no_register.lua             # Missing register_actions function
├── invalid_register.lua        # register_actions is not a function
├── multiple_actions.lua        # Registers several actions
├── circular_a.lua              # References circular_b.lua
├── circular_b.lua              # References circular_a.lua (circular)
└── nested/
    └── deep_action.lua         # For nested path testing
```

### YAML Test Files

```
rust/tests/fixtures/yaml/
├── import_lua_basic.yaml       # Single Lua import
├── import_lua_namespace.yaml   # Lua import with namespace
├── import_builtin.yaml         # Builtin import
├── import_mixed.yaml           # Both Lua and builtin
├── import_error_missing.yaml   # Missing file
├── import_error_invalid.yaml   # Invalid Lua module
├── import_error_builtin.yaml   # Unknown builtin
└── import_circular.yaml        # Circular import scenario
```

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Path resolution (TEA-RUST-034-UNIT-001, 002, 004, 008)
   - Contract validation (TEA-RUST-034-UNIT-005, 010)
   - Namespace handling (TEA-RUST-034-UNIT-007, 018)
   - Builtin dispatch (TEA-RUST-034-UNIT-013, 014, 017)

2. **P0 Integration tests**
   - TEA-RUST-034-INT-001, 003, 004, 008, 011, 014, 015

3. **P0 E2E tests**
   - TEA-RUST-034-E2E-002

4. **P1 tests in order** (Unit → Integration → E2E)

5. **P2+ tests as time permits**

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Implementation Notes

### Mocking Strategy

- **Unit tests:** Mock file system reads, Lua runtime
- **Integration tests:** Use real file system with test fixtures, real Lua runtime
- **E2E tests:** Full `tea` CLI execution with subprocess

### Test Location Mapping

| Test Type | File Location |
|-----------|---------------|
| Unit | `rust/tests/unit/test_imports.rs` |
| Integration | `rust/tests/integration/test_yaml_imports.rs` |
| E2E | `rust/tests/e2e/test_cli_imports.rs` |
| Fixtures | `rust/tests/fixtures/` |

### Key Testing Concerns

1. **Path resolution cross-platform:** Ensure tests work on both Unix and Windows paths
2. **Lua runtime isolation:** Each test should have fresh Lua state
3. **Error message quality:** Assert on error message contents, not just error type
4. **Namespace collision:** Test both warning emission and last-wins behavior
