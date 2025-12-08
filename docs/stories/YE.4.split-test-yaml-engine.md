# Story YE.4: Split test_yaml_engine.py into Smaller Files

## Status

Done

## Story

**As a** developer maintaining the test suite,
**I want** the monolithic `test_yaml_engine.py` file split into logical, smaller test modules,
**so that** tests are easier to navigate, maintain, and run selectively.

## Acceptance Criteria

1. The single `tests/test_yaml_engine.py` file (2686 lines, 24 test classes) is split into 7 separate test files based on functional boundaries
2. Each new test file contains logically related test classes
3. All imports are correctly configured in each new file (shared fixtures duplicated where needed)
4. All existing tests pass after the split (no test regressions)
5. Test discovery works correctly (`pytest tests/` finds all tests)
6. The original `test_yaml_engine.py` is removed after successful split
7. File naming follows pattern: `test_yaml_engine_<category>.py`

## Tasks / Subtasks

- [x] Task 1: Create `tests/test_yaml_engine_core.py` (AC: 1, 2, 3)
  - [x] Extract fixtures section (lines 24-58)
  - [x] Extract `TestYAMLEngineInit` class (lines 64-90)
  - [x] Extract `TestLoadFromFile` class (lines 96-131)
  - [x] Extract `TestLoadFromDict` class (lines 137-197)
  - [x] Extract `TestDotDict` class (lines 969-992)
  - [x] Extract `TestPackageIntegration` class (lines 901-917)
  - [x] Add required imports
  - [x] Verify file is syntactically valid

- [x] Task 2: Create `tests/test_yaml_engine_nodes.py` (AC: 1, 2, 3)
  - [x] Extract `TestInlineCode` class (lines 203-289)
  - [x] Extract `TestScriptStyle` class (lines 295-338)
  - [x] Extract `TestMultiStepNodes` class (lines 462-534)
  - [x] Extract `TestExpressionEvaluation` class (lines 540-609)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 3: Create `tests/test_yaml_engine_edges.py` (AC: 1, 2, 3)
  - [x] Extract `TestSimpleEdges` class (lines 615-669)
  - [x] Extract `TestConditionalEdges` class (lines 675-713)
  - [x] Extract `TestWhenClause` class (lines 719-758)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 4: Create `tests/test_yaml_engine_actions.py` (AC: 1, 2, 3)
  - [x] Extract `TestBuiltinActions` class (lines 344-456)
  - [x] Extract `TestBuiltinActionsRegistry` class (lines 820-895)
  - [x] Extract `TestTemplateProcessing` class (lines 764-814)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 5: Create `tests/test_yaml_engine_security.py` (AC: 1, 2, 3)
  - [x] Extract `TestSecurityBoundaries` class (lines 919-963)
  - [x] Extract `TestE2EExamples` class (lines 994-1080)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 6: Create `tests/test_yaml_engine_checkpoint.py` (AC: 1, 2, 3)
  - [x] Extract `TestCheckpointConfig` class (lines 1086-1196)
  - [x] Extract `TestCheckpointAPI` class (lines 1198-1357)
  - [x] Extract `TestCheckpointActions` class (lines 1359-1597)
  - [x] Extract `TestCheckpointTemplates` class (lines 1599-1690)
  - [x] Extract `TestCheckpointIntegration` class (lines 1692-1825)
  - [x] Extract `TestCheckpointErrors` class (lines 1827-1936)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 7: Create `tests/test_yaml_engine_data.py` (AC: 1, 2, 3)
  - [x] Extract `TestDataProcessingActions` class (lines 1942-2573)
  - [x] Extract `TestDataProcessingActionsIntegration` class (lines 2575-2679)
  - [x] Add required imports and fixtures
  - [x] Verify file is syntactically valid

- [x] Task 8: Validate all tests pass (AC: 4, 5)
  - [x] Run `pytest tests/test_yaml_engine_core.py -v`
  - [x] Run `pytest tests/test_yaml_engine_nodes.py -v`
  - [x] Run `pytest tests/test_yaml_engine_edges.py -v`
  - [x] Run `pytest tests/test_yaml_engine_actions.py -v`
  - [x] Run `pytest tests/test_yaml_engine_security.py -v`
  - [x] Run `pytest tests/test_yaml_engine_checkpoint.py -v`
  - [x] Run `pytest tests/test_yaml_engine_data.py -v`
  - [x] Run `pytest tests/` to verify full discovery

- [x] Task 9: Remove original file (AC: 6)
  - [x] Delete `tests/test_yaml_engine.py` after all tests pass
  - [x] Run final `pytest tests/` to confirm

## Dev Notes

### Existing Pattern Reference

This follows the exact pattern established in TD.11 which split `test_stategraph.py` into 5 modular files. The same approach applies here.

### Required Imports (Base Set)

```python
import pytest
import tempfile
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

import sys
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END, MemoryCheckpointer
from the_edge_agent.yaml_engine import DotDict
```

### Shared Fixtures

The following fixtures must be duplicated in each file that needs them:

```python
@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()

@pytest.fixture
def minimal_yaml_config():
    """Minimal valid YAML configuration."""
    return {
        'name': 'test-agent',
        'state_schema': {'value': 'int'},
        'nodes': [
            {'name': 'start', 'run': 'return {"value": 1}'}
        ],
        'edges': [
            {'from': '__start__', 'to': 'start'},
            {'from': 'start', 'to': '__end__'}
        ]
    }

@pytest.fixture
def temp_yaml_file(minimal_yaml_config):
    """Create a temporary YAML file."""
    import yaml
    with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
        yaml.dump(minimal_yaml_config, f)
        yield f.name
    os.unlink(f.name)
```

### File Size Estimates

| File | Est. Lines | Test Classes |
|------|------------|--------------|
| test_yaml_engine_core.py | ~250 | TestYAMLEngineInit, TestLoadFromFile, TestLoadFromDict, TestDotDict, TestPackageIntegration |
| test_yaml_engine_nodes.py | ~350 | TestInlineCode, TestScriptStyle, TestMultiStepNodes, TestExpressionEvaluation |
| test_yaml_engine_edges.py | ~150 | TestSimpleEdges, TestConditionalEdges, TestWhenClause |
| test_yaml_engine_actions.py | ~280 | TestBuiltinActions, TestBuiltinActionsRegistry, TestTemplateProcessing |
| test_yaml_engine_security.py | ~180 | TestSecurityBoundaries, TestE2EExamples |
| test_yaml_engine_checkpoint.py | ~860 | TestCheckpointConfig, TestCheckpointAPI, TestCheckpointActions, TestCheckpointTemplates, TestCheckpointIntegration, TestCheckpointErrors |
| test_yaml_engine_data.py | ~750 | TestDataProcessingActions, TestDataProcessingActionsIntegration |

### Testing

- **Test file location**: `tests/`
- **Test framework**: `pytest` with `unittest.mock`
- **Run command**: `pytest tests/ -v`

### Key Constraints

- Must not change any test logic, only move code between files
- All imports must be self-contained per file
- Fixtures must be duplicated in files that need them
- pytest must discover all tests automatically

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No issues encountered during implementation.

### Completion Notes List

- Split completed without any test failures
- All 220 tests pass (129 from new yaml_engine files + 91 from stategraph files)
- 9 tests skipped due to optional dependencies (jmespath, jsonschema) - same as before split
- pytest test discovery works correctly without any configuration changes
- GitHub Actions will automatically discover all test files

### File List

| File | Action | Description |
|------|--------|-------------|
| tests/test_yaml_engine_core.py | Created | Core tests: Init, LoadFromFile, LoadFromDict, DotDict, PackageIntegration (17 tests) |
| tests/test_yaml_engine_nodes.py | Created | Node tests: InlineCode, ScriptStyle, MultiStepNodes, ExpressionEvaluation (12 tests) |
| tests/test_yaml_engine_edges.py | Created | Edge tests: SimpleEdges, ConditionalEdges, WhenClause (6 tests) |
| tests/test_yaml_engine_actions.py | Created | Action tests: BuiltinActions, TemplateProcessing, BuiltinActionsRegistry (16 tests) |
| tests/test_yaml_engine_security.py | Created | Security tests: SecurityBoundaries, E2EExamples (6 tests) |
| tests/test_yaml_engine_checkpoint.py | Created | Checkpoint tests: Config, API, Actions, Templates, Integration, Errors (26 tests) |
| tests/test_yaml_engine_data.py | Created | Data processing tests: DataProcessingActions, Integration (55 tests) |
| tests/test_yaml_engine.py | Deleted | Original monolithic test file (2686 lines) |
