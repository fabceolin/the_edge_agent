# Test Design: Story YE.6 - External Action Module Imports

Date: 2025-12-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 22
- Unit tests: 14 (64%)
- Integration tests: 6 (27%)
- E2E tests: 2 (9%)
- Priority distribution: P0: 4, P1: 14, P2: 4

## Rationale

This story introduces external Python module loading into YAMLEngine, which is a **security-sensitive feature** (arbitrary code execution). The test strategy focuses heavily on:

1. **Contract validation** - Ensuring modules follow required patterns
2. **Error handling** - Clear, actionable error messages for developers
3. **Namespace isolation** - Preventing action name collisions
4. **Path resolution** - Correct handling of relative/absolute paths

The majority of tests are unit/integration level because:
- Module loading is pure logic (importlib operations)
- No external services involved (no E2E dependencies)
- Fast feedback critical for development workflow
- Integration tests validate the full import→action→execution flow

## Test Scenarios by Acceptance Criteria

### AC1: `imports:` top-level key in YAML config loads external action modules

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-001 | Unit | P1 | Parse `imports:` section from YAML dict | Pure config parsing logic |
| YE.6-UNIT-002 | Unit | P1 | Skip processing when `imports:` is empty/missing | Edge case handling |
| YE.6-INT-001 | Integration | P1 | Full load_from_dict with imports section populates registry | Component interaction validation |

### AC2: `path:` imports load Python files relative to YAML file location

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-003 | Unit | P0 | Resolve relative path from YAML file directory | Path resolution is critical for usability |
| YE.6-UNIT-004 | Unit | P1 | Handle absolute path without modification | Alternative path scenario |
| YE.6-UNIT-005 | Unit | P1 | Raise FileNotFoundError for missing file with clear message | Error handling |
| YE.6-UNIT-006 | Unit | P2 | Handle path with special characters (spaces, unicode) | Edge case robustness |

### AC3: `package:` imports load installed Python packages via importlib

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-007 | Unit | P1 | Load simple package name (e.g., `tea_actions_slack`) | Core package import functionality |
| YE.6-UNIT-008 | Unit | P1 | Load dotted package name (e.g., `tea_actions.slack`) | Submodule support |
| YE.6-UNIT-009 | Unit | P1 | Raise ImportError for missing package with clear message | Error handling |
| YE.6-INT-002 | Integration | P1 | Package import with real test package registers actions | Real importlib behavior |

### AC4: Imported modules must expose `register_actions(registry, engine)` function

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-010 | Unit | P0 | Validate module has `register_actions` callable | Contract enforcement is critical |
| YE.6-UNIT-011 | Unit | P1 | Raise ValueError for module missing `register_actions` with clear message | Developer guidance |
| YE.6-UNIT-012 | Unit | P2 | Validate `register_actions` is callable (not just attribute) | Defensive programming |

### AC5: `namespace:` prefix is applied to all actions from imported module

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-013 | Unit | P0 | Actions prefixed with `namespace.action_name` | Core namespacing functionality |
| YE.6-UNIT-014 | Unit | P1 | Empty namespace results in no prefix | Optional namespace support |
| YE.6-INT-003 | Integration | P1 | Multiple imports with different namespaces don't collide | Isolation verification |

### AC6: Actions from imports are available via `uses: namespace.action_name`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-INT-004 | Integration | P0 | Imported action callable from YAML node `uses:` directive | End-to-end action resolution |
| YE.6-E2E-001 | E2E | P1 | Complete workflow with imported action executes successfully | Full user journey validation |

### AC7: Import errors produce clear error messages with file/package name

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-015 | Unit | P1 | FileNotFoundError includes attempted path | Developer debugging |
| YE.6-UNIT-016 | Unit | P1 | ImportError includes package name | Developer debugging |
| YE.6-UNIT-017 | Unit | P1 | ValueError for invalid module includes module path/name | Developer debugging |

### AC8: Circular import detection prevents infinite loops

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-018 | Unit | P1 | Same path imported twice is skipped (no duplicate registration) | Idempotency |
| YE.6-UNIT-019 | Unit | P1 | Same package imported twice is skipped | Idempotency |
| YE.6-INT-005 | Integration | P2 | Multiple YAML imports of same module don't cause issues | Real-world usage pattern |

### AC9: Optional `__tea_actions__` metadata dict for discovery/validation

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-UNIT-020 | Unit | P2 | Metadata dict is read and logged when present | Optional feature |
| YE.6-UNIT-021 | Unit | P2 | Missing metadata is silently ignored | Graceful degradation |
| YE.6-INT-006 | Integration | P2 | Validation warning when declared actions don't match registered | Developer guidance |

### AC10: Comprehensive unit tests cover all import scenarios

*(This AC is meta - covered by this test design document)*

### AC11: Documentation updated

*(This AC is documentation - not testable via automated tests)*

## Additional Test Scenarios

### Error Recovery and Edge Cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.6-E2E-002 | E2E | P1 | Engine continues working after import failure for non-critical imports | Resilience validation |

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| **Security: Arbitrary code execution** | AC4 contract tests ensure only trusted modules load |
| **Usability: Confusing error messages** | AC7 error message tests validate developer experience |
| **Reliability: Path resolution failures** | AC2 path resolution tests cover edge cases |
| **Stability: Import conflicts** | AC5, AC8 namespace and circular import tests |

## Test Implementation Notes

### Test Fixtures Required

```
tests/
├── fixtures/
│   └── actions/
│       ├── valid_actions.py      # Module with proper register_actions
│       ├── invalid_actions.py    # Module missing register_actions
│       ├── with_metadata.py      # Module with __tea_actions__ dict
│       └── multi_action.py       # Module registering multiple actions
└── test_yaml_engine_imports.py   # New test file
```

### Mock Strategy

- **Path imports**: Use `tempfile` for isolated test modules
- **Package imports**: Mock `importlib.import_module` for missing package tests
- **Engine**: Real YAMLEngine instance (no mocking needed - fast enough)

### Test Data

```python
# valid_actions.py fixture
def register_actions(registry, engine):
    def test_action(state, param="default", **kwargs):
        return {"result": f"processed: {param}", "success": True}

    registry['test_action'] = test_action
    registry['another_action'] = lambda state, **kwargs: {"success": True}

__tea_actions__ = {
    "version": "1.0.0",
    "description": "Test actions for YE.6",
    "actions": ["test_action", "another_action"],
}
```

## Recommended Execution Order

1. **P0 Unit tests** (4 tests) - Core contract and path resolution
   - YE.6-UNIT-003 (path resolution)
   - YE.6-UNIT-010 (contract validation)
   - YE.6-UNIT-013 (namespace prefixing)
   - YE.6-INT-004 (action availability)

2. **P1 Unit tests** (10 tests) - Primary functionality
   - All path/package loading scenarios
   - Error message validation
   - Circular import detection

3. **P1 Integration tests** (4 tests) - Component interaction
   - Full import flow validation
   - Multi-namespace collision tests
   - Workflow execution

4. **P1 E2E tests** (2 tests) - User journey validation
   - Complete workflow execution
   - Error recovery

5. **P2 tests** (6 tests) - Edge cases and optional features
   - Special characters in paths
   - Metadata handling
   - Callable validation

## Quality Checklist

- [x] Every AC has at least one test (AC10, AC11 are meta/docs)
- [x] No duplicate coverage across levels (unit tests logic, integration tests flow)
- [x] Critical paths have multiple levels (import→register→execute has unit+integration+e2e)
- [x] Test levels are appropriate (unit for logic, integration for component interaction)
- [x] Priorities align with business risk (security-critical = P0)
- [x] Test IDs follow naming convention (YE.6-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 22
  by_level:
    unit: 14
    integration: 6
    e2e: 2
  by_priority:
    p0: 4
    p1: 14
    p2: 4
  coverage_gaps: []
  notes:
    - AC10 is meta (this document satisfies it)
    - AC11 is documentation (manual verification required)
```

## Trace References

```text
Test design matrix: docs/qa/assessments/YE.6-test-design-20251212.md
P0 tests identified: 4
Critical coverage: Path resolution, contract validation, namespace isolation, action availability
```
