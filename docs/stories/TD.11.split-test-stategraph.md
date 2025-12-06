# Story TD.11: Split test_stategraph.py into Smaller Files

## Status

Ready for Review

## Story

**As a** developer maintaining the test suite,
**I want** the monolithic `test_stategraph.py` file split into logical, smaller test modules,
**so that** tests are easier to navigate, maintain, and run selectively.

## Acceptance Criteria

1. The single `tests/test_stategraph.py` file (2540 lines) is split into 5 separate test files based on test class boundaries
2. Each new test file contains one logical test class with its related tests
3. All imports are correctly configured in each new file
4. All existing tests pass after the split (no test regressions)
5. Test discovery works correctly (`pytest tests/` finds all tests)
6. The original `test_stategraph.py` is removed after successful split
7. File naming follows pattern: `test_stategraph_<category>.py`

## Tasks / Subtasks

- [x] Task 1: Create `tests/test_stategraph_core.py` (AC: 1, 2, 3)
  - [x] Extract `TestStateGraph` class (lines 11-1219)
  - [x] Add required imports (unittest, logging, pygraphviz, mock, parameterized, hypothesis, tea)
  - [x] Verify file is syntactically valid

- [x] Task 2: Create `tests/test_stategraph_parallel.py` (AC: 1, 2, 3)
  - [x] Extract `TestStateGraphFanOutFanIn` class (lines 1220-1671)
  - [x] Add required imports
  - [x] Verify file is syntactically valid

- [x] Task 3: Create `tests/test_stategraph_logging.py` (AC: 1, 2, 3)
  - [x] Extract `TestStateGraphLogging` class (lines 1672-1812)
  - [x] Add required imports
  - [x] Verify file is syntactically valid

- [x] Task 4: Create `tests/test_stategraph_stream.py` (AC: 1, 2, 3)
  - [x] Extract `TestStateGraphStreamParallel` class (lines 1813-2163)
  - [x] Add required imports
  - [x] Verify file is syntactically valid

- [x] Task 5: Create `tests/test_stategraph_checkpoint.py` (AC: 1, 2, 3)
  - [x] Extract `TestStateGraphCheckpoint` class (lines 2164-2540)
  - [x] Add required imports (including tempfile, os, pickle for checkpoint tests)
  - [x] Verify file is syntactically valid

- [x] Task 6: Validate all tests pass (AC: 4, 5)
  - [x] Run `pytest tests/test_stategraph_core.py -v`
  - [x] Run `pytest tests/test_stategraph_parallel.py -v`
  - [x] Run `pytest tests/test_stategraph_logging.py -v`
  - [x] Run `pytest tests/test_stategraph_stream.py -v`
  - [x] Run `pytest tests/test_stategraph_checkpoint.py -v`
  - [x] Run `pytest tests/` to verify full discovery

- [x] Task 7: Remove original file (AC: 6)
  - [x] Delete `tests/test_stategraph.py` after all tests pass
  - [x] Delete `tests/test_stategraph.py.orig` (backup file)
  - [x] Run final `pytest tests/` to confirm

## Dev Notes

### Existing Pattern Reference

The project already has a modular test structure with `test_yaml_engine.py` as a separate file. Follow the same import pattern.

### Required Imports (Base Set)

```python
import unittest
import logging
from unittest.mock import patch
from parameterized import parameterized
from hypothesis import given, strategies as st, settings
import the_edge_agent as tea
```

### Additional Imports by File

- **test_stategraph_core.py**: `import pygraphviz as pgv` (for graphviz tests)
- **test_stategraph_checkpoint.py**: `import tempfile`, `import os`, `import pickle`, `import time`
- **test_stategraph_parallel.py**: May need `import threading`, `import copy`
- **test_stategraph_logging.py**: Standard base imports sufficient

### File Size Estimates

| File | Est. Lines | Test Class |
|------|------------|------------|
| test_stategraph_core.py | ~1209 | TestStateGraph |
| test_stategraph_parallel.py | ~452 | TestStateGraphFanOutFanIn |
| test_stategraph_logging.py | ~141 | TestStateGraphLogging |
| test_stategraph_stream.py | ~351 | TestStateGraphStreamParallel |
| test_stategraph_checkpoint.py | ~377 | TestStateGraphCheckpoint |

### Testing

- **Test file location**: `tests/`
- **Test framework**: `unittest` with `pytest` runner
- **Test standards**: Parameterized tests, property-based testing with Hypothesis
- **Run command**: `pytest tests/ -v`

### Key Constraints

- Must not change any test logic, only move code between files
- All imports must be self-contained per file
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
- All 138 tests pass (81 from stategraph files + 57 from yaml_engine)
- pytest test discovery works correctly without any configuration changes
- GitHub Actions will automatically discover all test files

### File List

| File | Action | Description |
|------|--------|-------------|
| tests/test_stategraph_core.py | Created | TestStateGraph class (46 tests) |
| tests/test_stategraph_parallel.py | Created | TestStateGraphFanOutFanIn class (6 tests) |
| tests/test_stategraph_logging.py | Created | TestStateGraphLogging class (7 tests) |
| tests/test_stategraph_stream.py | Created | TestStateGraphStreamParallel class (8 tests) |
| tests/test_stategraph_checkpoint.py | Created | TestStateGraphCheckpoint class (14 tests) |
| tests/test_stategraph.py | Deleted | Original monolithic test file |
| tests/test_stategraph.py.orig | Deleted | Backup file from previous work |

