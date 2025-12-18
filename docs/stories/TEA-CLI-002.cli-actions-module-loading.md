# Story: TEA-CLI-002 - CLI Actions Module Loading

## Status
Ready for Review

## Dependencies
- **Requires:** TEA-CLI-001 (tea-agent CLI Executable) - MUST be completed first

## User Story

**As a** developer using The Edge Agent CLI,
**I want** to load custom actions modules via CLI flags,
**so that** I can reuse action libraries across multiple YAML agents without duplicating `imports:` in every file.

## Story Context

### Existing System Integration:

- **Integrates with:**
  - YAMLEngine `actions_registry` parameter (`src/the_edge_agent/yaml_engine.py`)
  - CLI module `src/the_edge_agent/cli.py` (created in TEA-CLI-001)
- **Technology:**
  - Python `importlib` for dynamic module loading
  - YAMLEngine constructor parameter for actions registry injection
- **Follows pattern:**
  - Similar to pytest's `--import-mode` or Django's module loading
  - Module contract: `register_actions(registry, engine)` function
- **Touch points:**
  - `src/the_edge_agent/cli.py` - Add argument parsing for actions flags
  - `src/the_edge_agent/cli.py` - Add module loading logic

### Current State

After TEA-CLI-001 completion:
- ✅ `tea-agent` CLI command exists and works
- ✅ YAML agents can define custom actions via `imports:` section
- ❌ No way to load global/shared actions at CLI invocation time
- ❌ Developers must duplicate `imports:` across YAML files for shared actions

### Use Cases

**Use Case 1: Shared Actions Library**
```bash
# Load company-wide actions module
tea-agent agent.yaml --actions-module my_company.tea_actions
```

**Use Case 2: Local Development Actions**
```bash
# Load actions from local file during development
tea-agent agent.yaml --actions-file ./dev_actions.py
```

**Use Case 3: Testing Override**
```bash
# Override specific actions with test mocks
tea-agent agent.yaml --actions-file ./test_mocks.py
```

## Acceptance Criteria

### Functional Requirements:

1. **CLI Flag: --actions-module**
   - Accepts a Python module path (e.g., `my_package.actions`)
   - Loads module using `importlib.import_module()`
   - Calls `register_actions(registry, engine)` function if present
   - Registers actions into YAMLEngine's actions_registry
   - Shows clear error if module not found or invalid

2. **CLI Flag: --actions-file**
   - Accepts a file path (e.g., `./my_actions.py` or `/abs/path/actions.py`)
   - Loads Python file dynamically using `importlib` machinery
   - Calls `register_actions(registry, engine)` function if present
   - Supports both relative and absolute paths
   - Shows clear error if file not found or invalid

3. **Module Contract Validation**
   - If module/file lacks `register_actions()` function, show helpful error
   - If `register_actions()` raises exception, show error with traceback
   - Validates that registered actions are callables

4. **Actions Registry Merging**
   - CLI-loaded actions are available to YAML agent
   - YAML `imports:` actions take precedence over CLI actions (explicit override)
   - Built-in actions remain available (lowest priority)
   - Later CLI flags override earlier CLI flags (explicit via ordering)
   - Log warning when actions are overridden (for debugging and visibility)

5. **Multiple Actions Sources**
   - Support loading multiple modules: `--actions-module pkg1.actions --actions-module pkg2.actions`
   - Load order determines priority (later overrides earlier)
   - YAML imports always have highest priority

### Integration Requirements:

6. **YAMLEngine Integration**
   - Uses existing `YAMLEngine(actions_registry={...})` constructor parameter
   - No modifications to YAMLEngine internals required
   - Follows existing module loading contract from YAML `imports:` feature

7. **Error Handling**
   - ImportError: Show helpful message suggesting `pip install <module>`
   - FileNotFoundError: Show helpful message with absolute path attempted
   - AttributeError: Show message about missing `register_actions()` function
   - All errors include example of correct module structure

8. **Backward Compatibility**
   - CLI works without any actions flags (uses only YAML imports + built-ins)
   - Existing YAML files with `imports:` continue to work unchanged
   - No breaking changes to TEA-CLI-001 functionality

### Quality Requirements:

9. **Testing**
   - Unit tests for module/file loading logic
   - Unit tests for actions registry merging
   - Integration test with sample actions module
   - Test error handling for all failure modes

10. **Documentation**
    - Update README.md with `--actions-module` and `--actions-file` examples
    - Document the `register_actions(registry, engine)` contract
    - Provide example actions module structure
    - Update `--help` output with new flags

11. **No Regression**
    - All existing CLI tests pass
    - TEA-CLI-001 functionality unchanged when flags not used
    - No changes to YAMLEngine or StateGraph

## Technical Notes

### Integration Approach

Extend the CLI to load actions before creating YAMLEngine:

```python
# In cli.py main()

def load_actions_from_module(module_path: str) -> dict:
    """Load actions from a Python module."""
    import importlib

    module = importlib.import_module(module_path)

    if not hasattr(module, 'register_actions'):
        raise AttributeError(
            f"Module '{module_path}' must define register_actions(registry, engine) function"
        )

    registry = {}
    module.register_actions(registry, engine=None)  # engine passed later
    return registry

def load_actions_from_file(file_path: str) -> dict:
    """Load actions from a Python file."""
    import importlib.util
    from pathlib import Path

    path = Path(file_path).resolve()
    if not path.exists():
        raise FileNotFoundError(f"Actions file not found: {path}")

    spec = importlib.util.spec_from_file_location("custom_actions", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    if not hasattr(module, 'register_actions'):
        raise AttributeError(
            f"File '{file_path}' must define register_actions(registry, engine) function"
        )

    registry = {}
    module.register_actions(registry, engine=None)
    return registry

# In main():
combined_registry = {}

# Load from --actions-module flags
for module_path in args.actions_modules:
    combined_registry.update(load_actions_from_module(module_path))

# Load from --actions-file flags
for file_path in args.actions_files:
    combined_registry.update(load_actions_from_file(file_path))

# Create engine with combined registry
engine = YAMLEngine(actions_registry=combined_registry)
```

### Module Contract (from CLAUDE.md)

External action modules MUST follow this contract:

```python
# my_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""

    def my_custom_action(state, param1, param2=None, **kwargs):
        return {"result": "value", "success": True}

    registry['my_custom_action'] = my_custom_action

# Optional metadata
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My custom actions",
    "actions": ["my_custom_action"],
}
```

### Existing Pattern Reference

This follows the same module loading pattern used by YAMLEngine for YAML `imports:`:
- See `yaml_engine.py` methods: `_load_module_from_path()` (line 1090) and `_load_module_from_package()` (line 1137)
- Reuses the same `register_actions()` contract (validated at lines 1108, 1157)
- Uses standard `importlib` for module/file loading

### Key Constraints

- **Zero changes** to YAMLEngine internals (only uses public constructor parameter)
- **Backward compatible** with TEA-CLI-001 (flags are optional)
- **Same contract** as YAML `imports:` for consistency
- Must handle both installed packages and local files

## Definition of Done

- [x] `--actions-module` flag accepts module path and loads actions
- [x] `--actions-file` flag accepts file path and loads actions
- [x] Multiple flags can be specified (merged in order)
- [x] Module contract validation with helpful error messages
- [x] Actions registry merging works correctly (CLI → YAML imports precedence)
- [x] Error handling for ImportError, FileNotFoundError, AttributeError
- [x] Unit tests for module/file loading logic
- [x] Integration tests with sample actions module
- [x] README.md updated with examples and module contract documentation
- [x] `--help` output includes new flags
- [x] All existing tests continue to pass
- [x] No breaking changes to TEA-CLI-001 behavior

## Risk and Compatibility Check

### Minimal Risk Assessment

**Primary Risk:** Dynamic module loading could execute untrusted code

**Mitigation:**
- Document security warning: only load trusted modules
- No automatic module discovery (explicit flags only)
- Show warning if loading from relative file paths
- Recommend using installed packages for production

**Rollback:** Remove the argument parsing for these flags (backward compatible removal)

### Compatibility Verification

- [x] No breaking changes to existing CLI usage (flags are optional)
- [x] No changes to YAMLEngine or StateGraph internals
- [x] YAML `imports:` continue to work and take precedence
- [x] Performance impact: Negligible (module loading is one-time at startup)

## Validation Checklist

### Scope Validation

- [x] Story can be completed in one development session (~2-3 hours)
- [x] Integration approach is straightforward (argparse + importlib)
- [x] Follows existing module loading pattern from YAML imports
- [x] No design or architecture work required

### Clarity Check

- [x] Story requirements are unambiguous
- [x] Integration points clearly specified (cli.py only)
- [x] Success criteria are testable
- [x] Rollback approach is simple (remove flags)

## Tasks / Subtasks

- [x] **Task 1: Add CLI argument parsing** (AC: 1, 2, 5)
  - [x] Add `--actions-module` argument (action='append' for multiple)
  - [x] Add `--actions-file` argument (action='append' for multiple)
  - [x] Update `--help` documentation

- [x] **Task 2: Implement module loading functions** (AC: 1, 2, 3)
  - [x] Create `load_actions_from_module(module_path)` function
  - [x] Create `load_actions_from_file(file_path)` function
  - [x] Add contract validation (check for `register_actions()`)
  - [x] Add error handling with helpful messages

- [x] **Task 3: Implement registry merging** (AC: 4, 5)
  - [x] Merge CLI actions in order specified
  - [x] Pass merged registry to YAMLEngine constructor
  - [x] Ensure YAML imports override CLI actions

- [x] **Task 4: Error handling** (AC: 7)
  - [x] Handle ImportError with package installation hint
  - [x] Handle FileNotFoundError with absolute path display
  - [x] Handle AttributeError with contract example
  - [x] Include traceback for debugging

- [x] **Task 5: Testing** (AC: 9)
  - [x] Create sample actions module for testing
  - [x] Test module loading (`--actions-module`)
  - [x] Test file loading (`--actions-file`)
  - [x] Test multiple actions sources
  - [x] Test registry merging precedence
  - [x] Test all error conditions
  - [x] Test backward compatibility (no flags)

- [x] **Task 6: Documentation** (AC: 10)
  - [x] Update README.md with actions module examples
  - [x] Document `register_actions()` contract
  - [x] Provide example actions module structure
  - [x] Add security warning about untrusted code
  - [x] Update CLI `--help` text

- [x] **Task 7: Regression testing** (AC: 11)
  - [x] Run full test suite
  - [x] Verify TEA-CLI-001 functionality unchanged
  - [x] Test existing YAML examples with new flags

## Dev Notes

### Relevant Source Tree

```
src/the_edge_agent/
├── __init__.py              # No changes
├── stategraph.py            # No changes
├── yaml_engine.py           # No changes (uses existing actions_registry param)
└── cli.py                   # MODIFIED: Add actions module loading

tests/
├── test_cli.py              # MODIFIED: Add actions module loading tests
└── fixtures/
    └── sample_actions.py    # NEW: Sample actions module for testing

README.md                    # MODIFIED: Add actions module documentation
```

### Key Implementation Notes

1. **Path Resolution:** Use `pathlib.Path.resolve()` for absolute paths
2. **Module Caching:** Python's import system caches modules automatically
3. **Error Messages:** Include example of correct module structure in errors
4. **Security:** Document that only trusted modules should be loaded
5. **Testing:** Create reusable sample actions module in tests/fixtures/

### Example Actions Module for Testing

```python
# tests/fixtures/sample_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register sample test actions."""

    def test_action(state, **kwargs):
        return {"test": "success"}

    def another_action(state, value, **kwargs):
        return {"result": value * 2}

    registry['test_action'] = test_action
    registry['another_action'] = another_action

__tea_actions__ = {
    "version": "1.0.0",
    "description": "Sample test actions",
    "actions": ["test_action", "another_action"],
}
```

### Testing Standards

#### Test File Location
- `tests/test_cli.py` (extend existing file from TEA-CLI-001)
- `tests/fixtures/sample_actions.py` (new test fixture)

#### Test Standards
- Use `unittest` framework (consistent with existing tests)
- Use `tempfile` for creating temporary action files
- Mock `importlib` where appropriate for error testing

#### Testing Frameworks
- `unittest` for test structure
- `unittest.mock` for mocking imports
- `tempfile` for temporary files

#### Specific Testing Requirements
- Test loading from installed package (use fixtures as mock package)
- Test loading from local file (use tempfile)
- Test multiple sources with correct precedence
- Test all error paths with helpful messages
- Verify no regression when flags not used

### Security Considerations

**Warning in Documentation:**
```markdown
⚠️ **Security Warning**: The `--actions-module` and `--actions-file` flags
execute Python code from the specified modules. Only load actions from trusted
sources. For production use, prefer installed packages over local files.
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-17 | 1.0 | Initial story creation for CLI actions module loading | Sarah (PO Agent) |
| 2025-12-17 | 1.1 | Validation complete - fixed method references, clarified collision behavior - Status: Approved | Sarah (PO Agent) |

## Dev Agent Record

### Agent Model Used
Claude Sonnet 4.5 (claude-sonnet-4-5-20250929)

### Debug Log References
None - Implementation completed without blockers or debugging sessions.

### Completion Notes List
- All 7 tasks completed successfully
- Implemented `--actions-module` and `--actions-file` CLI flags with full argparse integration
- Created `load_actions_from_module()` and `load_actions_from_file()` functions with comprehensive error handling
- Implemented `load_cli_actions()` for merging multiple action sources with proper precedence (later overrides earlier)
- Added detailed error messages with contract examples for ImportError, FileNotFoundError, and AttributeError
- Created sample test fixture `tests/fixtures/sample_actions.py` following the actions module contract
- Wrote 42 comprehensive unit tests covering all functionality and error paths
- All tests pass (42/42) including new tests and existing regression tests (123 core tests pass)
- Updated README.md with CLI examples, custom actions module documentation, and security warning
- CLI `--help` output updated automatically via epilog examples
- No breaking changes to TEA-CLI-001 functionality - fully backward compatible
- Actions loading priority correctly implemented: Built-in → CLI modules → CLI files → YAML imports

### File List
**Modified:**
- `src/the_edge_agent/cli.py` - Added CLI argument parsing, module/file loading functions, registry merging
- `tests/test_cli.py` - Extended with comprehensive tests for actions loading
- `README.md` - Added CLI examples and custom actions module documentation

**Created:**
- `tests/fixtures/__init__.py` - Package init for test fixtures
- `tests/fixtures/sample_actions.py` - Sample actions module for testing

## QA Results

*This section will be populated by QA Agent after story completion.*
