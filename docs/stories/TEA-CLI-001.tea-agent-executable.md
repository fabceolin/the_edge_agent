# Story: TEA-CLI-001 - tea-agent CLI Executable

## Status
Done

## User Story

**As a** developer using The Edge Agent,
**I want** a globally-accessible CLI command to run arbitrary YAML agents,
**so that** I can execute YAML-defined workflows without writing Python boilerplate.

## Story Context

### Existing System Integration:

- **Integrates with:** YAMLEngine (`src/the_edge_agent/yaml_engine.py`)
- **Technology:** Python setuptools console_scripts entry points
- **Follows pattern:** Standard Python CLI pattern (argparse + main function)
- **Touch points:**
  - `setup.py` - Add `console_scripts` entry point
  - New file `src/the_edge_agent/cli.py` - CLI implementation
  - Reuses logic from `examples/run_yaml_agent.py` (specifically `run_custom_yaml` function)

### Current State

The project has:
- ✅ YAMLEngine that loads and executes YAML agent configurations
- ✅ Example script `examples/run_yaml_agent.py` demonstrating usage
- ❌ No installed CLI executable (users must run `python examples/run_yaml_agent.py`)

### Scope Clarification: Custom Actions

**In Scope for This Story:**
- Custom actions defined via YAML `imports:` section (already supported by YAMLEngine)
- Built-in actions (`llm.call`, `http.get`, `file.read`, etc.)

**Out of Scope (Deferred to TEA-CLI-002):**
- CLI flags for loading external actions modules (e.g., `--actions-module`)
- Global actions configuration files
- CLI-level actions registry injection

**Rationale:**
This story focuses on core CLI functionality. Custom actions support via CLI flags is a separate enhancement tracked in TEA-CLI-002.

## Acceptance Criteria

### Functional Requirements:

1. **CLI Entry Point Created**
   - A new `tea-agent` command is available system-wide after `pip install -e .`
   - Command accepts a YAML file path as the first positional argument
   - Command accepts optional `--state` flag with JSON string for initial state
   - Command accepts optional `--state-file` flag to load initial state from JSON file

2. **YAML Agent Execution**
   - Loads the specified YAML file using YAMLEngine
   - Executes the agent with provided or empty initial state
   - Streams execution events to stdout with clear formatting
   - Displays final state as formatted JSON on completion

3. **Error Handling**
   - Shows helpful error message if YAML file doesn't exist
   - Shows helpful error message if YAML is malformed
   - Shows helpful error message if initial state JSON is invalid
   - Returns non-zero exit code on errors

### Integration Requirements:

4. **Existing YAMLEngine Integration**
   - Uses existing `YAMLEngine.load_from_file()` method (no modifications)
   - Uses existing `StateGraph.stream()` method for execution
   - No changes to YAMLEngine or StateGraph core logic

5. **CLI Pattern Consistency**
   - Follows standard argparse pattern
   - Includes `--help` flag with usage documentation
   - Includes `--version` flag showing package version

6. **Example Compatibility**
   - Works with existing `examples/yaml_customer_support_example.yaml`
   - Works with existing `examples/yaml_agent_example.yaml`
   - Existing example scripts remain functional

### Quality Requirements:

7. **Testing**
   - Unit tests for CLI argument parsing
   - Integration test executing a sample YAML agent
   - Test error handling for missing/invalid files

8. **Documentation**
   - Update README.md with CLI usage examples
   - Add docstrings to `cli.py` module
   - Include usage examples in `--help` output

9. **No Regression**
   - All existing tests pass
   - Example scripts continue to work
   - No changes to public API of StateGraph or YAMLEngine

## Technical Notes

### Integration Approach

The CLI will be a thin wrapper around existing functionality:

```python
# src/the_edge_agent/cli.py
import sys
import argparse
import json
from pathlib import Path
from the_edge_agent import YAMLEngine, __version__

def main():
    parser = argparse.ArgumentParser(
        description="Execute YAML-defined Edge Agent workflows"
    )
    parser.add_argument("yaml_file", help="Path to YAML agent configuration")
    parser.add_argument("--state", help="Initial state as JSON string")
    parser.add_argument("--state-file", help="Path to JSON file with initial state")
    parser.add_argument("--version", action="version", version=f"tea-agent {__version__}")

    args = parser.parse_args()

    # Load YAML, parse state, execute via YAMLEngine.load_from_file() + graph.stream()
    # Format and print events to stdout
```

### Existing Pattern Reference

The implementation should closely follow `run_custom_yaml()` from `examples/run_yaml_agent.py` (lines 129-174), adapting it for CLI usage with proper argument parsing and exit codes.

### Key Constraints

- **Zero changes** to YAMLEngine or StateGraph internals
- Must work with all existing YAML examples without modification
- Should be installable and runnable immediately after `pip install -e .`
- Must handle Ctrl+C gracefully (KeyboardInterrupt)

## Definition of Done

- [x] `tea-agent` command is registered in `setup.py` console_scripts
- [x] `src/the_edge_agent/cli.py` implements the CLI logic
- [x] `--help`, `--version`, `--state`, `--state-file` flags work correctly
- [x] Successfully executes `examples/yaml_customer_support_example.yaml`
- [x] Successfully executes `examples/yaml_agent_example.yaml`
- [x] Error handling for missing/invalid files is robust
- [x] Unit and integration tests pass
- [x] README.md updated with CLI usage section
- [x] All existing tests continue to pass
- [x] Code follows existing project patterns and PEP 8

## Risk and Compatibility Check

### Minimal Risk Assessment

**Primary Risk:** Entry point conflicts if `tea-agent` name collides with existing tools

**Mitigation:**
- Use descriptive name `tea-agent` (not generic `tea` or `agent`)
- Document the command name clearly in README
- Search PyPI for existing `tea-agent` packages before finalizing

**Rollback:** Simply remove the `console_scripts` entry from `setup.py` and delete `cli.py`

### Compatibility Verification

- [x] No breaking changes to existing APIs (YAMLEngine, StateGraph)
- [x] Database changes: N/A (no database involved)
- [x] UI changes: N/A (CLI only, follows terminal conventions)
- [x] Performance impact: Negligible (just argument parsing overhead)

## Validation Checklist

### Scope Validation

- [x] Story can be completed in one development session (~2-4 hours)
- [x] Integration approach is straightforward (add entry point + new module)
- [x] Follows existing patterns exactly (standard Python CLI with argparse)
- [x] No design or architecture work required

### Clarity Check

- [x] Story requirements are unambiguous
- [x] Integration points clearly specified (setup.py + new cli.py)
- [x] Success criteria are testable (command works, examples run, tests pass)
- [x] Rollback approach is simple (delete cli.py, remove entry point)

## Tasks / Subtasks

- [x] **Task 1: Create CLI module** (AC: 1, 2, 3)
  - [x] Create `src/the_edge_agent/cli.py`
  - [x] Implement argument parsing with argparse
  - [x] Implement YAML loading and state initialization logic
  - [x] Implement event streaming and output formatting
  - [x] Add error handling and exit codes

- [x] **Task 2: Register console script entry point** (AC: 1, 5)
  - [x] Add `console_scripts` section to `setup.py`
  - [x] Register `tea-agent = the_edge_agent.cli:main`
  - [x] Ensure `__version__` is in `__init__.py` `__all__` export list
        (Note: `__version__` variable exists at line 99 but not in `__all__` list)

- [x] **Task 3: Test CLI functionality** (AC: 2, 6, 7)
  - [x] Test `tea-agent examples/yaml_customer_support_example.yaml`
  - [x] Test `tea-agent examples/yaml_agent_example.yaml`
  - [x] Test `tea-agent --state '{"key": "value"}' examples/...`
  - [x] Test `tea-agent --state-file state.json examples/...`
  - [x] Test error handling for missing files
  - [x] Test error handling for invalid YAML
  - [x] Test error handling for invalid JSON state

- [x] **Task 4: Write automated tests** (AC: 7)
  - [x] Create `tests/test_cli.py`
  - [x] Test argument parsing logic
  - [x] Test integration with YAMLEngine
  - [x] Test error conditions

- [x] **Task 5: Update documentation** (AC: 8)
  - [x] Update README.md with "CLI Usage" section
  - [x] Add examples showing `tea-agent` command
  - [x] Document `--state` and `--state-file` flags
  - [x] Add docstrings to `cli.py`

- [x] **Task 6: Verify no regressions** (AC: 9)
  - [x] Run full test suite: `pytest`
  - [x] Verify example scripts still work
  - [x] Verify existing import paths unchanged

## Dev Notes

### Relevant Source Tree

```
src/the_edge_agent/
├── __init__.py              # Public API - exports StateGraph, YAMLEngine, __version__
├── stategraph.py            # Core StateGraph implementation (no changes)
├── yaml_engine.py           # YAMLEngine for YAML loading (no changes)
└── cli.py                   # NEW: CLI implementation

examples/
├── run_yaml_agent.py        # Reference implementation (no changes)
├── yaml_customer_support_example.yaml
└── yaml_agent_example.yaml

setup.py                     # MODIFIED: Add console_scripts entry point

tests/
└── test_cli.py              # NEW: CLI tests
```

### Key Implementation Notes

1. **Import `__version__`:** Ensure `__init__.py` exports `__version__` for `--version` flag
2. **State merging:** If both `--state` and `--state-file` are provided, merge them (or show error)
3. **Event formatting:** Use existing pattern from `run_yaml_agent.py` for consistent output
4. **Path resolution:** Use `pathlib.Path` for cross-platform compatibility
5. **JSON parsing:** Use `json.loads()` with proper error handling

### Testing Standards

#### Test File Location
- `tests/test_cli.py` (new file)

#### Test Standards
- Use `unittest` framework (consistent with existing test suite)
- Mock file I/O where appropriate
- Test both success and failure paths

#### Testing Frameworks
- `unittest` for test structure
- `unittest.mock` for mocking file system and subprocess calls
- `tempfile` for creating temporary YAML files in tests

#### Specific Testing Requirements
- Test CLI with sample YAML configs (use temporary files)
- Test argument parsing edge cases
- Verify exit codes (0 for success, non-zero for errors)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-17 | 1.0 | Initial story creation | Sarah (PO Agent) |
| 2025-12-17 | 1.1 | Added scope clarification - custom actions deferred to TEA-CLI-002 | Sarah (PO Agent) |
| 2025-12-17 | 1.2 | Validation complete - clarified __version__ export requirement - Status: Approved | Sarah (PO Agent) |

## Dev Agent Record

### Agent Model Used
Claude Sonnet 4.5 (claude-sonnet-4-5-20250929)

### Debug Log References
No debug log entries required - implementation completed without issues.

### Completion Notes List
- CLI module created with comprehensive argument parsing (--state, --state-file, --version, --help)
- Entry point registered in setup.py as `tea-agent` command
- `__version__` exported in `__init__.py` for --version flag
- 18 new unit tests added covering all CLI functionality
- All 714 existing tests pass - zero regressions
- README.md updated with CLI Usage section and examples
- Tested successfully with both yaml_agent_example.yaml and yaml_customer_support_example.yaml
- Error handling verified for missing files, invalid YAML, and invalid JSON
- State merging works correctly when both --state and --state-file are provided

### File List
**Created:**
- `src/the_edge_agent/cli.py` - Main CLI module with argument parsing and execution logic
- `tests/test_cli.py` - Comprehensive unit tests (18 test cases)

**Modified:**
- `setup.py` - Added `entry_points` section with `tea-agent` console script
- `src/the_edge_agent/__init__.py` - Added `__version__` to `__all__` exports
- `README.md` - Added CLI Usage section with examples and documentation

## QA Results

### Quality Gate: PASS

**Reviewer:** Quinn (Test Architect)
**Date:** 2026-01-08
**Gate File:** `docs/qa/gates/TEA-CLI-001-tea-agent-executable.yml`

#### Gate Decision Summary

**Status:** ✅ **PASS** - All acceptance criteria met with comprehensive test coverage and zero regressions.

**Key Findings:**
- ✅ All 9 acceptance criteria fully verified and passing
- ✅ 18 new comprehensive unit/integration tests added
- ✅ Zero regressions - all 714 existing tests pass
- ✅ Clean integration with no modifications to YAMLEngine or StateGraph core
- ✅ Excellent error handling with helpful messages
- ✅ Well-documented with clear README examples

#### Acceptance Criteria Verification

| AC | Description | Status | Evidence |
|----|-------------|--------|----------|
| 1 | CLI Entry Point Created | ✅ PASS | tea-agent command with --state, --state-file, --version, --help flags |
| 2 | YAML Agent Execution | ✅ PASS | Loads YAML, executes via StateGraph.stream(), displays events/final state |
| 3 | Error Handling | ✅ PASS | Tests verify FileNotFoundError, YAML/JSON errors with non-zero exit codes |
| 4 | YAMLEngine Integration | ✅ PASS | Uses existing load_from_file() and stream() methods without modifications |
| 5 | CLI Pattern Consistency | ✅ PASS | Standard argparse pattern with --help/--version, __version__ exported |
| 6 | Example Compatibility | ✅ PASS | Works with yaml_customer_support_example.yaml and yaml_agent_example.yaml |
| 7 | Testing | ✅ PASS | 18 unit tests covering argument parsing, integration, error handling |
| 8 | Documentation | ✅ PASS | README.md updated with CLI Usage section, docstrings in cli.py |
| 9 | No Regression | ✅ PASS | All 714 existing tests pass, no changes to public APIs |

#### Risk Assessment

**Total Risks:** 1 (Low)

| Risk | Severity | Mitigation | Status |
|------|----------|------------|--------|
| Entry point name collision (tea-agent) | Low | Descriptive naming, PyPI search performed | ✅ MITIGATED |

#### Non-Functional Requirements

| NFR | Status | Notes |
|-----|--------|-------|
| Security | ✅ PASS | No security concerns - CLI only handles local file paths |
| Performance | ✅ PASS | Minimal overhead from argparse |
| Reliability | ✅ PASS | Comprehensive error handling for all edge cases |
| Maintainability | ✅ PASS | Clean code following project patterns, well documented |
| Usability | ✅ PASS | Intuitive CLI with --help and --version flags |

#### Implementation Quality Highlights

**Strengths:**
- Clean, straightforward implementation following standard Python CLI patterns
- No technical debt introduced - uses existing YAMLEngine functionality
- Excellent test coverage with 18 new tests
- Good error messages for common failure scenarios
- README documentation clear and helpful

**Future Enhancements (Non-Blocking):**
- Consider adding --verbose flag for detailed execution logs
- Add shell completion support (argcomplete) for better UX

#### Final Recommendation

**Decision:** ✅ **APPROVED FOR MERGE**

Story TEA-CLI-001 is production-ready with no blocking issues. Implementation demonstrates exemplary quality with comprehensive testing, clean integration, and thorough documentation.
