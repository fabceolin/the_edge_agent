# Test Design: TEA-PY-008.4 - Extract Import Loader Module

## Story Reference
**Story ID**: TEA-PY-008.4
**Story Title**: Extract Import Loader Module
**Test Design Date**: 2025-12-27
**Test Architect**: Quinn

## Executive Summary

This test design covers the extraction of 215 lines of import loading logic from `yaml_engine.py` into a new `yaml_imports.py` module. The refactoring introduces the `ImportLoader` class while maintaining 100% backward compatibility with existing import behavior.

**Key Testing Focus Areas**:
1. YE.6 external imports functionality (path, package, namespace)
2. Circular import prevention mechanism
3. Namespace handling and action registration
4. Module contract validation (`register_actions` function)
5. Metadata logging (`__tea_actions__` attribute)
6. Backward compatibility with existing tests

**Risk Areas**:
- Breaking relative path resolution during extraction
- Losing circular import prevention state
- Changing error messages that downstream code depends on
- Introducing circular imports between yaml_engine.py and yaml_imports.py

---

## Test Strategy

### Testing Pyramid Distribution

| Level | Count | Percentage | Rationale |
|-------|-------|------------|-----------|
| Unit | 18 | 43% | Focus on ImportLoader class methods in isolation |
| Integration | 20 | 48% | Verify ImportLoader with YAMLEngine and file system |
| E2E | 4 | 9% | Validate full workflows with imported actions |
| **Total** | **42** | **100%** | Comprehensive coverage of all 28 ACs |

### Test Execution Order

1. **Phase 1: Module Structure** (Unit tests for AC 1-7)
2. **Phase 2: Core Import Logic** (Unit + Integration for AC 8-21)
3. **Phase 3: YAMLEngine Integration** (Integration tests for AC 22-25)
4. **Phase 4: Backward Compatibility** (All existing tests for AC 26-28)

### Coverage Targets

- **Line Coverage**: >95% for yaml_imports.py
- **Branch Coverage**: >90% for all conditional paths
- **AC Coverage**: 100% (all 28 acceptance criteria)

---

## Test Scenarios

### 1. Module Creation & Structure (AC 1-3)

#### TEA-PY-008.4-unit-001
- **AC**: 1, 2
- **Priority**: P3
- **Description**: Verify yaml_imports.py module exists and is importable
- **Preconditions**: Module created at `python/src/the_edge_agent/yaml_imports.py`
- **Test Steps**:
  1. Import yaml_imports module
  2. Verify module docstring exists and is comprehensive (>50 chars)
  3. Verify docstring includes usage examples
- **Expected Results**:
  - Module imports without errors
  - Docstring contains "ImportLoader" and "YAMLEngine"
  - Docstring includes example code snippets
- **Test Data**: None
- **Dependencies**: None

#### TEA-PY-008.4-unit-002
- **AC**: 3
- **Priority**: P2
- **Description**: Verify module is under 250 lines
- **Preconditions**: Module implementation complete
- **Test Steps**:
  1. Read yaml_imports.py file
  2. Count total lines (including comments and docstrings)
  3. Assert line count <= 250
- **Expected Results**: Line count is 250 or fewer
- **Test Data**: None
- **Dependencies**: None

---

### 2. ImportLoader Class Structure (AC 4-7)

#### TEA-PY-008.4-unit-003
- **AC**: 4, 5
- **Priority**: P0
- **Description**: Verify ImportLoader class constructor accepts engine reference
- **Preconditions**: None
- **Test Steps**:
  1. Create mock YAMLEngine instance
  2. Instantiate ImportLoader with engine reference
  3. Verify `_engine` attribute is set
  4. Verify `_loaded_modules` is initialized as empty set
- **Expected Results**:
  - ImportLoader instance created successfully
  - `_engine` references passed engine
  - `_loaded_modules` is empty set
- **Test Data**: Mock YAMLEngine with actions_registry dict
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-unit-004
- **AC**: 6
- **Priority**: P0
- **Description**: Verify load_imports method signature and basic execution
- **Preconditions**: ImportLoader instantiated
- **Test Steps**:
  1. Create ImportLoader with mock engine
  2. Call `load_imports([], None)`
  3. Verify no errors with empty imports list
  4. Call `load_imports([], "/some/dir")`
  5. Verify yaml_dir parameter is accepted
- **Expected Results**:
  - Method accepts imports list and yaml_dir
  - No errors with empty imports
  - No errors with yaml_dir path
- **Test Data**: Empty list, various yaml_dir paths
- **Dependencies**: None

#### TEA-PY-008.4-unit-005
- **AC**: 7
- **Priority**: P0
- **Description**: Verify circular import prevention tracking
- **Preconditions**: ImportLoader instantiated
- **Test Steps**:
  1. Create ImportLoader
  2. Verify `_loaded_modules` is initially empty
  3. Mock `_load_from_path` to add module to `_loaded_modules`
  4. Verify module path is tracked after first load
  5. Verify second load of same path is skipped
- **Expected Results**:
  - `_loaded_modules` starts empty
  - Module paths added to set after loading
  - Duplicate loads detected and skipped
- **Test Data**: Duplicate module paths
- **Dependencies**: unittest.mock

---

### 3. Path Import Support (AC 8-12)

#### TEA-PY-008.4-integration-001
- **AC**: 8, 9
- **Priority**: P0
- **Description**: Verify relative path resolution from YAML directory
- **Preconditions**: Test fixture at `tests/fixtures/actions/valid_actions.py`
- **Test Steps**:
  1. Create ImportLoader with real YAMLEngine
  2. Call `_load_from_path('./valid_actions.py', 'rel', yaml_dir=fixtures_dir)`
  3. Verify path is resolved relative to yaml_dir
  4. Verify actions are registered with namespace
- **Expected Results**:
  - Relative path resolved correctly
  - Actions registered as `rel.transform` and `rel.echo`
  - No FileNotFoundError
- **Test Data**: `tests/fixtures/actions/valid_actions.py`
- **Dependencies**: File system, test fixtures

#### TEA-PY-008.4-integration-002
- **AC**: 9
- **Priority**: P0
- **Description**: Verify absolute path handling
- **Preconditions**: Test fixture with absolute path
- **Test Steps**:
  1. Create ImportLoader
  2. Get absolute path to valid_actions.py
  3. Call `_load_from_path(absolute_path, 'abs', yaml_dir='/other/dir')`
  4. Verify yaml_dir is ignored for absolute paths
  5. Verify actions are registered
- **Expected Results**:
  - Absolute path loaded successfully
  - yaml_dir parameter ignored
  - Actions registered as `abs.transform` and `abs.echo`
- **Test Data**: Absolute path to test fixture
- **Dependencies**: File system, os.path.abspath

#### TEA-PY-008.4-integration-003
- **AC**: 10
- **Priority**: P0
- **Description**: Verify FileNotFoundError for missing files
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Call `_load_from_path('/nonexistent/actions.py', 'test', None)`
  3. Catch FileNotFoundError exception
  4. Verify error message includes path
  5. Verify error message says "Action module not found"
- **Expected Results**:
  - FileNotFoundError raised
  - Error message includes "/nonexistent/actions.py"
  - Error message is user-friendly
- **Test Data**: Non-existent file path
- **Dependencies**: None

#### TEA-PY-008.4-integration-004
- **AC**: 11
- **Priority**: P0
- **Description**: Verify module loaded with unique name
- **Preconditions**: Test fixture available
- **Test Steps**:
  1. Create ImportLoader
  2. Load same physical file with different namespaces
  3. Verify each load uses unique module name in sys.modules
  4. Verify no module name collisions
- **Expected Results**:
  - Each load creates unique module entry
  - Module names include timestamp or UUID
  - No ImportError due to name collision
- **Test Data**: Same fixture file, multiple namespaces
- **Dependencies**: sys.modules inspection

#### TEA-PY-008.4-integration-005
- **AC**: 12
- **Priority**: P0
- **Description**: Verify register_actions function validation
- **Preconditions**: Test fixture without register_actions
- **Test Steps**:
  1. Create ImportLoader
  2. Load `tests/fixtures/actions/invalid_actions.py`
  3. Catch ValueError exception
  4. Verify error message mentions "register_actions"
  5. Verify error message includes module path
- **Expected Results**:
  - ValueError raised
  - Error message: "missing required 'register_actions'"
  - Error message includes source path
- **Test Data**: `tests/fixtures/actions/invalid_actions.py`
- **Dependencies**: Test fixture without register_actions

---

### 4. Package Import Support (AC 13-15)

#### TEA-PY-008.4-integration-006
- **AC**: 13
- **Priority**: P0
- **Description**: Verify installed package loading via importlib
- **Preconditions**: Mock package with register_actions
- **Test Steps**:
  1. Create ImportLoader
  2. Mock importlib.import_module to return valid module
  3. Call `_load_from_package('tea_test_pkg', 'pkg')`
  4. Verify importlib.import_module called with package name
  5. Verify actions registered with namespace
- **Expected Results**:
  - importlib.import_module called correctly
  - Mock package loaded
  - Actions registered as `pkg.action_name`
- **Test Data**: Mock package module
- **Dependencies**: unittest.mock, importlib

#### TEA-PY-008.4-integration-007
- **AC**: 14
- **Priority**: P0
- **Description**: Verify ImportError with helpful message for missing packages
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Call `_load_from_package('nonexistent_tea_package_xyz', 'test')`
  3. Catch ImportError exception
  4. Verify error message includes package name
  5. Verify error message suggests "pip install"
- **Expected Results**:
  - ImportError raised
  - Error message: "Failed to import package 'nonexistent_tea_package_xyz'"
  - Error message includes "pip install" suggestion
- **Test Data**: Non-existent package name
- **Dependencies**: None

#### TEA-PY-008.4-integration-008
- **AC**: 15
- **Priority**: P0
- **Description**: Verify register_actions validation for packages
- **Preconditions**: Built-in module without register_actions
- **Test Steps**:
  1. Create ImportLoader
  2. Call `_load_from_package('json', 'test')` (json is built-in)
  3. Catch ValueError exception
  4. Verify error message mentions "register_actions"
- **Expected Results**:
  - ValueError raised
  - Error message: "missing required 'register_actions'"
  - Error message includes package name
- **Test Data**: Built-in 'json' module
- **Dependencies**: None

---

### 5. Namespace Handling (AC 16-18)

#### TEA-PY-008.4-unit-006
- **AC**: 16
- **Priority**: P0
- **Description**: Verify namespace prefix applied to actions
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader with mock engine
  2. Create local registry with actions: {'action1': fn1, 'action2': fn2}
  3. Call `_merge_registry_with_namespace(local_registry, 'custom', 'test.py')`
  4. Verify engine.actions_registry contains 'custom.action1' and 'custom.action2'
  5. Verify original names not in registry
- **Expected Results**:
  - Actions prefixed with "custom."
  - Registry contains 'custom.action1' and 'custom.action2'
  - Original names ('action1', 'action2') not added
- **Test Data**: Mock registry with 2 actions
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-unit-007
- **AC**: 17
- **Priority**: P0
- **Description**: Verify empty namespace registers at root level
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader with mock engine
  2. Create local registry with actions: {'my_action': fn}
  3. Call `_merge_registry_with_namespace(local_registry, '', 'test.py')`
  4. Verify engine.actions_registry contains 'my_action' (no prefix)
- **Expected Results**:
  - Actions registered without namespace prefix
  - Registry contains 'my_action' (not '.my_action')
- **Test Data**: Mock registry, empty string namespace
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-integration-009
- **AC**: 18
- **Priority**: P1
- **Description**: Verify override warnings logged for duplicate actions
- **Preconditions**: Mock logger
- **Test Steps**:
  1. Create ImportLoader
  2. Add action 'ns.action1' to engine registry
  3. Mock logger.warning
  4. Create local registry with 'action1'
  5. Call `_merge_registry_with_namespace(local_registry, 'ns', 'new.py')`
  6. Verify logger.warning called with override message
  7. Verify message includes action name and both sources
- **Expected Results**:
  - Warning logged for duplicate action
  - Warning message includes "ns.action1"
  - Warning mentions both original and new source
- **Test Data**: Duplicate action names
- **Dependencies**: unittest.mock for logger

#### TEA-PY-008.4-integration-010
- **AC**: 16, 17
- **Priority**: P0
- **Description**: Verify multiple namespaces don't collide
- **Preconditions**: Test fixtures with same action names
- **Test Steps**:
  1. Create ImportLoader with real engine
  2. Load valid_actions.py with namespace 'first'
  3. Load valid_actions.py again with namespace 'second' (should be skipped)
  4. Load multi_action.py with namespace 'third'
  5. Verify all namespaces have correct prefixes
  6. Verify 'first.transform' and 'third.action_one' both exist
- **Expected Results**:
  - Each namespace isolated
  - No action name collisions
  - Second load of same file skipped (circular import prevention)
- **Test Data**: Multiple test fixtures
- **Dependencies**: File system, test fixtures

---

### 6. Metadata Logging (AC 19-21)

#### TEA-PY-008.4-integration-011
- **AC**: 19, 20
- **Priority**: P2
- **Description**: Verify __tea_actions__ metadata logged at INFO level
- **Preconditions**: Test fixture with __tea_actions__ attribute
- **Test Steps**:
  1. Create ImportLoader
  2. Mock logger at module level
  3. Load `tests/fixtures/actions/with_metadata.py`
  4. Verify logger.info called
  5. Verify logged message includes version, description, actions list
  6. Verify namespace included in log
- **Expected Results**:
  - logger.info called (not warning or debug)
  - Log message includes "version: 1.2.3"
  - Log message includes "namespace: meta"
  - Log message includes actions list
- **Test Data**: `tests/fixtures/actions/with_metadata.py`
- **Dependencies**: unittest.mock for logger

#### TEA-PY-008.4-integration-012
- **AC**: 21
- **Priority**: P2
- **Description**: Verify graceful handling of missing metadata
- **Preconditions**: Test fixture without __tea_actions__
- **Test Steps**:
  1. Create ImportLoader
  2. Mock logger
  3. Load `tests/fixtures/actions/valid_actions.py` (no metadata)
  4. Verify actions registered successfully
  5. Verify logger.info NOT called for metadata
  6. Verify no exceptions raised
- **Expected Results**:
  - Actions registered normally
  - No errors or warnings about missing metadata
  - Module loads successfully
- **Test Data**: `tests/fixtures/actions/valid_actions.py`
- **Dependencies**: None

#### TEA-PY-008.4-unit-008
- **AC**: 20, 21
- **Priority**: P2
- **Description**: Verify partial metadata logging
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Create mock module with partial __tea_actions__: {'version': '1.0'}
  3. Mock logger
  4. Call `_log_module_metadata(module, 'test.py', 'ns')`
  5. Verify version logged
  6. Verify missing description doesn't cause error
  7. Verify missing actions list doesn't cause error
- **Expected Results**:
  - Metadata logged with available fields
  - Missing fields handled gracefully
  - No KeyError or AttributeError
- **Test Data**: Mock module with partial metadata dict
- **Dependencies**: unittest.mock

---

### 7. YAMLEngine Integration (AC 22-25)

#### TEA-PY-008.4-integration-013
- **AC**: 22
- **Priority**: P0
- **Description**: Verify YAMLEngine creates _import_loader attribute
- **Preconditions**: yaml_engine.py imports ImportLoader
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Verify `_import_loader` attribute exists
  3. Verify `_import_loader` is instance of ImportLoader
  4. Verify `_import_loader._engine` references the engine
- **Expected Results**:
  - `_import_loader` attribute set in __init__
  - Instance is ImportLoader class
  - Circular reference established (loader -> engine)
- **Test Data**: None
- **Dependencies**: None

#### TEA-PY-008.4-integration-014
- **AC**: 23
- **Priority**: P0
- **Description**: Verify YAMLEngine._load_imports delegates to loader
- **Preconditions**: YAMLEngine with ImportLoader
- **Test Steps**:
  1. Create YAMLEngine
  2. Mock `_import_loader.load_imports` method
  3. Call `engine._load_imports(imports_list, yaml_dir)`
  4. Verify `_import_loader.load_imports` called with same arguments
  5. Verify no other import logic in YAMLEngine._load_imports
- **Expected Results**:
  - YAMLEngine._load_imports is thin wrapper
  - All arguments passed through to loader
  - Return value passed back to caller
- **Test Data**: Mock imports list and yaml_dir
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-integration-015
- **AC**: 24
- **Priority**: P0
- **Description**: Verify _loaded_modules tracked on loader instance
- **Preconditions**: ImportLoader integrated with YAMLEngine
- **Test Steps**:
  1. Create YAMLEngine
  2. Verify `_loaded_modules` NOT in engine.__dict__
  3. Verify `_import_loader._loaded_modules` exists
  4. Load a module via engine
  5. Verify module tracked in `_import_loader._loaded_modules`
  6. Verify engine doesn't have own _loaded_modules
- **Expected Results**:
  - State moved to ImportLoader
  - YAMLEngine doesn't duplicate state
  - Circular import tracking works through loader
- **Test Data**: Test fixture module
- **Dependencies**: File system

#### TEA-PY-008.4-integration-016
- **AC**: 25
- **Priority**: P0
- **Description**: Verify actions_registry passed to loader for population
- **Preconditions**: YAMLEngine with actions_registry
- **Test Steps**:
  1. Create YAMLEngine
  2. Verify `_import_loader._engine.actions_registry` is same object as `engine.actions_registry`
  3. Load module with actions
  4. Verify actions appear in engine.actions_registry
  5. Verify no separate registry in loader
- **Expected Results**:
  - Single registry shared between engine and loader
  - Actions registered in engine's registry
  - Reference equality confirmed (same object)
- **Test Data**: Test fixture with actions
- **Dependencies**: None

---

### 8. Backward Compatibility (AC 26-28)

#### TEA-PY-008.4-e2e-001
- **AC**: 26
- **Priority**: P0
- **Description**: Verify all existing import tests pass without modification
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run `pytest tests/test_yaml_engine_imports.py -v`
  2. Verify all 40+ tests pass
  3. Verify no test modifications required
  4. Verify no test skips or xfails
- **Expected Results**:
  - All tests pass (100% success rate)
  - No test code changes needed
  - Same test count as before refactoring
- **Test Data**: Existing test suite
- **Dependencies**: pytest, existing test fixtures
- **Notes**: This is the primary backward compatibility gate

#### TEA-PY-008.4-integration-017
- **AC**: 27
- **Priority**: P0
- **Description**: Verify import behavior identical before/after extraction
- **Preconditions**: Test fixtures available
- **Test Steps**:
  1. Load YAML with path import
  2. Verify actions registered in same location
  3. Load YAML with package import (mocked)
  4. Verify actions registered in same location
  5. Load YAML with multiple imports
  6. Verify load order and namespace behavior unchanged
- **Expected Results**:
  - Actions registered in actions_registry
  - Namespace prefixes applied identically
  - Load order deterministic
  - No behavioral changes detected
- **Test Data**: Multiple test fixtures
- **Dependencies**: File system

#### TEA-PY-008.4-integration-018
- **AC**: 28
- **Priority**: P0
- **Description**: Verify error messages unchanged
- **Preconditions**: Test fixtures for error cases
- **Test Steps**:
  1. Test FileNotFoundError message (missing path)
  2. Test ImportError message (missing package)
  3. Test ValueError message (no register_actions)
  4. Test ValueError message (invalid import type)
  5. Compare each error message to pre-refactoring version
- **Expected Results**:
  - All error messages character-for-character identical
  - Error types unchanged (FileNotFoundError, ImportError, ValueError)
  - Error message formatting preserved
- **Test Data**: Various invalid import configurations
- **Dependencies**: Stored error message baselines

---

### 9. Circular Import Prevention (Special Focus: YE.6)

#### TEA-PY-008.4-integration-019
- **AC**: 7, 24
- **Priority**: P0
- **Description**: Verify circular import prevention with path imports
- **Preconditions**: Test fixture
- **Test Steps**:
  1. Create ImportLoader
  2. Load valid_actions.py with namespace 'first'
  3. Verify module path added to _loaded_modules
  4. Attempt to load same path with namespace 'second'
  5. Verify second load silently skipped
  6. Verify 'second.transform' NOT in registry
  7. Verify only 'first.transform' exists
- **Expected Results**:
  - First load succeeds
  - Module path tracked
  - Second load detected and skipped
  - No duplicate actions
- **Test Data**: Same fixture file loaded twice
- **Dependencies**: File system

#### TEA-PY-008.4-integration-020
- **AC**: 7, 24
- **Priority**: P0
- **Description**: Verify circular import prevention with package imports
- **Preconditions**: Mock package
- **Test Steps**:
  1. Create ImportLoader
  2. Mock importlib.import_module
  3. Load package 'test_pkg' with namespace 'first'
  4. Verify package name added to _loaded_modules
  5. Attempt to load 'test_pkg' with namespace 'second'
  6. Verify second load skipped
  7. Verify only 'first.action' exists in registry
- **Expected Results**:
  - First package load succeeds
  - Package name tracked
  - Second load skipped
  - No duplicate registration
- **Test Data**: Mock package
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-unit-009
- **AC**: 7
- **Priority**: P1
- **Description**: Verify _loaded_modules state isolation between engine instances
- **Preconditions**: None
- **Test Steps**:
  1. Create first YAMLEngine instance
  2. Load module A with first engine
  3. Create second YAMLEngine instance
  4. Verify second engine can load module A (not blocked by first)
  5. Verify each loader has independent _loaded_modules set
- **Expected Results**:
  - Each ImportLoader has isolated state
  - No cross-contamination between engines
  - Module can be loaded in multiple engine instances
- **Test Data**: Test fixture
- **Dependencies**: None

---

### 10. Edge Cases & Error Handling

#### TEA-PY-008.4-integration-021
- **AC**: 8, 9
- **Priority**: P1
- **Description**: Verify path resolution with special characters
- **Preconditions**: Fixture file with spaces/special chars in path
- **Test Steps**:
  1. Create fixture in directory with spaces: "test fixtures/actions/valid.py"
  2. Create ImportLoader
  3. Load using relative path from yaml_dir
  4. Verify path resolved correctly
  5. Verify actions registered
- **Expected Results**:
  - Paths with spaces handled correctly
  - No path resolution errors
  - Actions registered successfully
- **Test Data**: Path with spaces and special characters
- **Dependencies**: File system with space-containing directory

#### TEA-PY-008.4-integration-022
- **AC**: 11
- **Priority**: P1
- **Description**: Verify module loaded with sys.modules cleanup
- **Preconditions**: Test fixture
- **Test Steps**:
  1. Record initial sys.modules keys
  2. Create ImportLoader and load module
  3. Verify new module in sys.modules
  4. Verify module name is unique (contains UUID or timestamp)
  5. Delete ImportLoader
  6. Verify module still in sys.modules (intentional - no cleanup)
- **Expected Results**:
  - Module added to sys.modules
  - Unique module name generated
  - Module persists after loader deletion
- **Test Data**: Test fixture
- **Dependencies**: sys.modules inspection

#### TEA-PY-008.4-integration-023
- **AC**: 12, 15
- **Priority**: P1
- **Description**: Verify register_actions signature validation
- **Preconditions**: Fixture with wrong signature
- **Test Steps**:
  1. Create fixture with `register_actions(registry)` (missing engine param)
  2. Create ImportLoader
  3. Load fixture
  4. Verify ValueError raised about signature
  5. Verify error message helpful
- **Expected Results**:
  - ValueError raised for incorrect signature
  - Error message mentions expected signature
  - Error message includes source path
- **Test Data**: Fixture with wrong register_actions signature
- **Dependencies**: Custom test fixture
- **Notes**: May require inspect module to validate signature

#### TEA-PY-008.4-unit-010
- **AC**: 16, 17, 18
- **Priority**: P1
- **Description**: Verify namespace edge cases
- **Preconditions**: None
- **Test Steps**:
  1. Test namespace with dots: "my.custom.namespace"
  2. Test namespace with underscores: "my_namespace"
  3. Test namespace with numbers: "ns123"
  4. Test namespace with hyphen: "my-namespace"
  5. Verify all create valid action names
- **Expected Results**:
  - All namespace formats accepted
  - Actions prefixed correctly
  - No namespace validation errors
- **Test Data**: Various namespace strings
- **Dependencies**: None

#### TEA-PY-008.4-integration-024
- **AC**: 6
- **Priority**: P2
- **Description**: Verify load_imports with mixed import types
- **Preconditions**: Test fixtures and mock package
- **Test Steps**:
  1. Create imports list with both path and package imports
  2. Create ImportLoader
  3. Load mixed imports: [{'path': '...'}, {'package': '...'}]
  4. Verify both types loaded
  5. Verify correct dispatcher used for each
- **Expected Results**:
  - Path imports call _load_from_path
  - Package imports call _load_from_package
  - Both registered in same registry
- **Test Data**: Mixed import configuration
- **Dependencies**: Mock package, test fixture

#### TEA-PY-008.4-integration-025
- **AC**: 6
- **Priority**: P1
- **Description**: Verify invalid import type error
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Create import config with neither 'path' nor 'package': {'url': '...'}
  3. Call load_imports with invalid config
  4. Verify ValueError raised
  5. Verify error message says "must specify 'path' or 'package'"
- **Expected Results**:
  - ValueError raised
  - Clear error message
  - Error message mentions both required keys
- **Test Data**: Invalid import configuration
- **Dependencies**: None

---

### 11. Full Workflow Integration (E2E)

#### TEA-PY-008.4-e2e-002
- **AC**: 27
- **Priority**: P0
- **Description**: End-to-end workflow with imported action execution
- **Preconditions**: Test fixture with valid_actions.py
- **Test Steps**:
  1. Create YAML config with imports and nodes using imported actions
  2. Create YAMLEngine and load config
  3. Verify imports processed
  4. Execute workflow with invoke()
  5. Verify imported action executed
  6. Verify state mutations from action
  7. Verify final state includes action results
- **Expected Results**:
  - YAML loaded successfully
  - Workflow executes without errors
  - Imported action runs and transforms state
  - Final state contains expected values
- **Test Data**: YAML config with imports + nodes
- **Dependencies**: Full YAMLEngine, test fixtures

#### TEA-PY-008.4-e2e-003
- **AC**: 8, 27
- **Priority**: P0
- **Description**: End-to-end with load_from_file and relative imports
- **Preconditions**: Temporary YAML file in fixtures directory
- **Test Steps**:
  1. Create temporary YAML file with relative import: './valid_actions.py'
  2. Write YAML to fixtures directory
  3. Call YAMLEngine.load_from_file(yaml_path)
  4. Verify yaml_dir auto-detected from file path
  5. Verify relative import resolved
  6. Execute workflow
  7. Cleanup temporary YAML file
- **Expected Results**:
  - YAML file loaded
  - yaml_dir derived from file path
  - Relative import resolved correctly
  - Workflow executes successfully
- **Test Data**: Temporary YAML file, test fixture
- **Dependencies**: File system, tempfile module

#### TEA-PY-008.4-e2e-004
- **AC**: 26, 27
- **Priority**: P0
- **Description**: Full test suite regression - 320+ tests
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run full test suite: `pytest tests/`
  2. Verify all 320+ tests pass
  3. Verify no new failures introduced
  4. Verify no new warnings
  5. Check test execution time (should be similar to pre-refactoring)
- **Expected Results**:
  - All tests pass (100%)
  - No new failures
  - Performance unchanged (<5% variance)
  - No new deprecation warnings
- **Test Data**: Full test suite
- **Dependencies**: pytest, all test fixtures
- **Notes**: Critical gate for refactoring success

---

### 12. Code Quality & Documentation

#### TEA-PY-008.4-unit-011
- **AC**: 2
- **Priority**: P3
- **Description**: Verify ImportLoader class has comprehensive docstring
- **Preconditions**: Module complete
- **Test Steps**:
  1. Inspect ImportLoader.__doc__
  2. Verify docstring >100 characters
  3. Verify docstring mentions "external action modules"
  4. Verify docstring has example usage
  5. Verify all public methods have docstrings
- **Expected Results**:
  - Class docstring comprehensive
  - All methods documented
  - Docstrings follow Google/NumPy style
- **Test Data**: None
- **Dependencies**: None

#### TEA-PY-008.4-unit-012
- **AC**: 2, 3
- **Priority**: P3
- **Description**: Verify module-level imports and dependencies
- **Preconditions**: Module complete
- **Test Steps**:
  1. Import yaml_imports module
  2. Verify dependencies: os, importlib, logging, typing
  3. Verify TYPE_CHECKING used for YAMLEngine import
  4. Verify no circular imports at module level
  5. Run with Python -c "import the_edge_agent.yaml_imports"
- **Expected Results**:
  - All imports succeed
  - No circular import errors
  - TYPE_CHECKING prevents runtime circular import
  - Module importable standalone
- **Test Data**: None
- **Dependencies**: None

#### TEA-PY-008.4-unit-013
- **AC**: 4, 5
- **Priority**: P2
- **Description**: Verify engine reference pattern is thread-safe
- **Preconditions**: None
- **Test Steps**:
  1. Create YAMLEngine
  2. Verify _import_loader._engine is weak reference or normal reference
  3. If normal reference, verify no memory leak with circular ref
  4. Create multiple loaders with same engine
  5. Delete engine, verify loaders don't prevent garbage collection
- **Expected Results**:
  - Engine reference works correctly
  - No memory leaks from circular references
  - Garbage collection not prevented
- **Test Data**: None
- **Dependencies**: gc module, weakref (if used)
- **Notes**: May need to check if weak references needed

---

### 13. Logging & Observability

#### TEA-PY-008.4-unit-014
- **AC**: 19, 20, 21
- **Priority**: P2
- **Description**: Verify logging configuration and levels
- **Preconditions**: None
- **Test Steps**:
  1. Verify module creates logger: `logger = logging.getLogger(__name__)`
  2. Mock logger at module level
  3. Test each logging call (info, warning, debug)
  4. Verify log levels used correctly:
     - INFO for metadata
     - WARNING for overrides
     - DEBUG for loading details (if any)
- **Expected Results**:
  - Logger created with module name
  - Appropriate log levels used
  - Log messages include context (namespace, source)
- **Test Data**: Various module loads
- **Dependencies**: unittest.mock

#### TEA-PY-008.4-integration-026
- **AC**: 18
- **Priority**: P2
- **Description**: Verify override warning includes full context
- **Preconditions**: Test fixture
- **Test Steps**:
  1. Create ImportLoader
  2. Load module with action 'ns.my_action' from 'first.py'
  3. Mock logger
  4. Load different module with action 'my_action' into namespace 'ns'
  5. Capture warning message
  6. Verify warning includes:
     - Action name: 'ns.my_action'
     - Original source: 'first.py'
     - New source: 'second.py'
- **Expected Results**:
  - Warning logged at WARNING level
  - All context included in message
  - Clear indication of override occurring
- **Test Data**: Two fixtures with same action name
- **Dependencies**: unittest.mock for logger

---

### 14. Security & Validation

#### TEA-PY-008.4-unit-015
- **AC**: 12, 15
- **Priority**: P1
- **Description**: Verify register_actions is callable
- **Preconditions**: None
- **Test Steps**:
  1. Create mock module with register_actions attribute that is not callable
  2. Create ImportLoader
  3. Attempt to load module
  4. Verify ValueError raised
  5. Verify error message says "must be callable" or similar
- **Expected Results**:
  - Non-callable register_actions rejected
  - Clear error message
  - TypeError or ValueError raised
- **Test Data**: Mock module with register_actions = "not a function"
- **Dependencies**: None

#### TEA-PY-008.4-unit-016
- **AC**: 8, 10
- **Priority**: P1
- **Description**: Verify path traversal prevention
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Attempt to load path with traversal: '../../../etc/passwd'
  3. If path doesn't exist, FileNotFoundError expected
  4. If path exists, verify it's not loaded as Python module
  5. Verify no security bypass
- **Expected Results**:
  - Path traversal allowed (it's valid use case for relative imports)
  - But non-Python files don't load
  - FileNotFoundError for missing files
- **Test Data**: Path traversal strings
- **Dependencies**: None
- **Notes**: Path traversal is valid for relative imports, but files must be valid Python modules

#### TEA-PY-008.4-unit-017
- **AC**: 12, 15
- **Priority**: P1
- **Description**: Verify register_actions exception handling
- **Preconditions**: None
- **Test Steps**:
  1. Create mock module where register_actions raises exception
  2. Create ImportLoader
  3. Attempt to load module
  4. Verify original exception propagates or is wrapped
  5. Verify error message includes source and action that failed
- **Expected Results**:
  - Exception from register_actions not silently caught
  - Error context preserved
  - Traceback helpful for debugging
- **Test Data**: Mock module with failing register_actions
- **Dependencies**: unittest.mock

---

### 15. Performance & Resource Management

#### TEA-PY-008.4-unit-018
- **AC**: 7, 24
- **Priority**: P2
- **Description**: Verify _loaded_modules memory efficiency
- **Preconditions**: None
- **Test Steps**:
  1. Create ImportLoader
  2. Load 100 unique modules (mocked)
  3. Measure _loaded_modules size
  4. Verify it's a set (O(1) lookup)
  5. Verify memory usage reasonable (<10KB for 100 entries)
- **Expected Results**:
  - Set data structure used
  - Fast lookup for circular import check
  - Reasonable memory usage
- **Test Data**: 100 mock module paths
- **Dependencies**: sys.getsizeof for memory check
- **Notes**: Set is important for O(1) duplicate detection

---

## Test Environment Requirements

### Software Dependencies
- **Python**: 3.10+
- **pytest**: Latest version
- **unittest.mock**: Standard library
- **Coverage**: pytest-cov plugin

### Test Fixtures Required
- `tests/fixtures/actions/valid_actions.py` - Valid module with register_actions
- `tests/fixtures/actions/invalid_actions.py` - Module without register_actions
- `tests/fixtures/actions/with_metadata.py` - Module with __tea_actions__ metadata
- `tests/fixtures/actions/multi_action.py` - Module with multiple actions

### Mock Objects
- Mock YAMLEngine with actions_registry
- Mock importlib.import_module for package tests
- Mock logging.Logger for log verification

---

## Test Data Requirements

### Valid Import Configurations
```python
# Path import
{'path': './actions/custom.py', 'namespace': 'custom'}

# Absolute path import
{'path': '/abs/path/to/actions.py', 'namespace': 'abs'}

# Package import
{'package': 'tea_custom_actions', 'namespace': 'custom'}

# Empty namespace
{'path': './actions.py', 'namespace': ''}
```

### Invalid Import Configurations
```python
# Missing both path and package
{'namespace': 'invalid'}

# Invalid key
{'url': 'https://example.com/actions.py', 'namespace': 'invalid'}
```

### Test Fixture Structures

**valid_actions.py**:
```python
def register_actions(registry, engine):
    registry['transform'] = lambda state, **kw: {'transformed': f"[{kw['data']}]"}
    registry['echo'] = lambda state, **kw: {'echoed': kw['message']}
```

**with_metadata.py**:
```python
__tea_actions__ = {
    "version": "1.2.3",
    "description": "Test actions with metadata",
    "actions": ["action1", "action2"]
}

def register_actions(registry, engine):
    registry['action1'] = lambda state, **kw: {}
    registry['action2'] = lambda state, **kw: {}
```

---

## Risk Mitigation

### High-Risk Areas

#### Risk 1: Breaking Relative Path Resolution
- **Impact**: P0 - Critical functionality broken
- **Tests**: TEA-PY-008.4-integration-001, TEA-PY-008.4-e2e-003
- **Mitigation**:
  - Verify exact path resolution logic preserved
  - Test with both relative and absolute paths
  - Test with load_from_file auto-detection

#### Risk 2: Circular Import Between Modules
- **Impact**: P0 - Module import failure
- **Tests**: TEA-PY-008.4-unit-012
- **Mitigation**:
  - Use TYPE_CHECKING for YAMLEngine import
  - Test standalone module import
  - Verify no runtime circular imports

#### Risk 3: Losing Loaded Modules State
- **Impact**: P1 - Duplicate imports not prevented
- **Tests**: TEA-PY-008.4-integration-019, TEA-PY-008.4-integration-020
- **Mitigation**:
  - Verify state tracked on ImportLoader instance
  - Test circular import prevention still works
  - Verify state isolation between engines

#### Risk 4: Changing Error Messages
- **Impact**: P1 - Breaking downstream code that parses errors
- **Tests**: TEA-PY-008.4-integration-018
- **Mitigation**:
  - Compare all error messages character-by-character
  - Store baseline error messages
  - Verify error types unchanged

---

## Acceptance Criteria Coverage Matrix

| AC | Description | Test IDs | Priority | Status |
|----|-------------|----------|----------|--------|
| 1 | Module file created | unit-001 | P3 | Pending |
| 2 | Comprehensive docstring | unit-001, unit-011 | P3 | Pending |
| 3 | Module under 250 lines | unit-002 | P2 | Pending |
| 4 | ImportLoader class created | unit-003 | P0 | Pending |
| 5 | Constructor accepts engine | unit-003 | P0 | Pending |
| 6 | load_imports method implemented | unit-004, integration-024, integration-025 | P0 | Pending |
| 7 | Circular import tracking | unit-005, integration-019, integration-020, unit-009 | P0 | Pending |
| 8 | Relative path resolution | integration-001, integration-021, unit-016 | P0 | Pending |
| 9 | Absolute paths work | integration-002 | P0 | Pending |
| 10 | FileNotFoundError for missing | integration-003 | P0 | Pending |
| 11 | Unique module name | integration-004, integration-022 | P0 | Pending |
| 12 | register_actions validated | integration-005, integration-023, unit-015, unit-017 | P0 | Pending |
| 13 | Package loading via importlib | integration-006 | P0 | Pending |
| 14 | ImportError with helpful message | integration-007 | P0 | Pending |
| 15 | Package register_actions validated | integration-008, unit-015 | P0 | Pending |
| 16 | Namespace prefix applied | unit-006, integration-010 | P0 | Pending |
| 17 | Empty namespace at root | unit-007, integration-010 | P0 | Pending |
| 18 | Override warnings logged | integration-009, integration-026 | P1 | Pending |
| 19 | Metadata logged at INFO | integration-011, unit-014 | P2 | Pending |
| 20 | Version/description/actions logged | integration-011, unit-008 | P2 | Pending |
| 21 | Graceful missing metadata | integration-012, unit-008 | P2 | Pending |
| 22 | _import_loader attribute added | integration-013 | P0 | Pending |
| 23 | _load_imports delegates | integration-014 | P0 | Pending |
| 24 | _loaded_modules on loader | integration-015, integration-019, integration-020, unit-018 | P0 | Pending |
| 25 | actions_registry passed | integration-016 | P0 | Pending |
| 26 | All existing tests pass | e2e-001, e2e-004 | P0 | Pending |
| 27 | Behavior identical | integration-017, e2e-002, e2e-003 | P0 | Pending |
| 28 | Error messages unchanged | integration-018 | P0 | Pending |

**Total ACs**: 28
**Total Test Scenarios**: 42
**Coverage**: 100% (all ACs covered by at least one test)

---

## Test Execution Plan

### Phase 1: Unit Tests (AC 1-7)
**Duration**: 30 minutes
**Tests**: unit-001 through unit-018
**Goal**: Verify ImportLoader class structure and isolated methods

**Success Criteria**:
- All 18 unit tests pass
- No import errors
- Module under 250 lines
- Class structure validated

### Phase 2: Integration Tests (AC 8-25)
**Duration**: 1 hour
**Tests**: integration-001 through integration-026
**Goal**: Verify ImportLoader with YAMLEngine and file system

**Success Criteria**:
- All 20 integration tests pass
- Path and package imports work
- Namespace handling correct
- YAMLEngine integration validated

### Phase 3: E2E Tests (AC 26-28)
**Duration**: 45 minutes
**Tests**: e2e-001 through e2e-004
**Goal**: Verify full workflows and backward compatibility

**Success Criteria**:
- All 4 E2E tests pass
- Existing test suite passes (320+ tests)
- Full workflows execute correctly
- No regressions detected

### Phase 4: Regression & Performance
**Duration**: 30 minutes
**Tests**: Full test suite, performance baseline
**Goal**: Confirm no regressions and acceptable performance

**Success Criteria**:
- All 320+ tests pass
- Test execution time within 5% of baseline
- No new warnings or deprecations
- Memory usage stable

---

## Test Reporting

### Test Results Format

```
Test Execution Report: TEA-PY-008.4
Date: YYYY-MM-DD
Tester: [Name]

Summary:
- Total Tests: 42
- Passed: X
- Failed: Y
- Skipped: Z
- Success Rate: XX%

Failures:
[Test ID] - [Test Name]
  - Error: [Error message]
  - Root Cause: [Analysis]
  - Action: [Fix required]

Coverage:
- Line Coverage: XX%
- Branch Coverage: XX%
- AC Coverage: XX% (X/28)

Recommendation: [PASS / FAIL / CONDITIONAL PASS]
```

### Exit Criteria

**Must Pass (Blockers)**:
- All P0 tests pass (29 tests)
- AC coverage 100% (28/28)
- Existing test suite passes (e2e-001, e2e-004)
- No circular imports (unit-012)

**Should Pass (Important)**:
- All P1 tests pass (7 tests)
- Line coverage >95%
- Branch coverage >90%

**Nice to Pass**:
- All P2 tests pass (4 tests)
- All P3 tests pass (2 tests)
- Documentation quality checks

---

## Special Testing Considerations

### YE.6 External Imports Feature

The YE.6 feature has 6 key capabilities that must be preserved:

1. **Path imports**: Tested by integration-001, integration-002, integration-003
2. **Package imports**: Tested by integration-006, integration-007, integration-008
3. **Namespacing**: Tested by unit-006, unit-007, integration-009, integration-010
4. **Circular import prevention**: Tested by integration-019, integration-020, unit-009
5. **Contract validation**: Tested by integration-005, integration-008, unit-015
6. **Metadata logging**: Tested by integration-011, integration-012, unit-008

**Critical Path**: integration-001 → integration-006 → unit-006 → integration-019 → integration-013 → e2e-001

### Circular Import Prevention

This is a critical feature that must be thoroughly tested:

**Test Coverage**:
- unit-005: Tracks _loaded_modules state
- integration-019: Path imports circular prevention
- integration-020: Package imports circular prevention
- unit-009: State isolation between engines

**Success Criteria**:
- First load succeeds, second load silently skipped
- No duplicate actions in registry
- No infinite import loops

### Namespace Handling

Namespace handling has several edge cases:

**Test Coverage**:
- unit-006: Prefix applied
- unit-007: Empty namespace
- integration-009: Override warnings
- integration-010: Multiple namespaces
- unit-010: Namespace edge cases (dots, hyphens, etc.)

**Success Criteria**:
- Actions prefixed correctly: `namespace.action_name`
- Empty namespace registers at root: `action_name`
- Conflicts logged with clear warnings

---

## Conclusion

This test design provides comprehensive coverage of the import loader module extraction with 42 test scenarios across 3 levels (unit, integration, E2E). All 28 acceptance criteria are covered with appropriate priority levels.

**Key Success Factors**:
1. Backward compatibility (AC 26-28) is the highest priority
2. Circular import prevention must be thoroughly validated
3. Error messages must remain unchanged
4. Full existing test suite must pass without modification

**Recommended Execution Order**:
1. Phase 1: Unit tests (validate structure)
2. Phase 2: Integration tests (validate behavior)
3. Phase 3: E2E tests (validate backward compatibility)
4. Phase 4: Full regression (confirm no side effects)

**Estimated Total Execution Time**: 2.5 hours (excluding any fixes)

---

## Appendix A: Test Fixture Reference

### Required Fixtures

| Fixture | Path | Purpose | Contents |
|---------|------|---------|----------|
| valid_actions.py | tests/fixtures/actions/ | Valid module | register_actions with 2 actions |
| invalid_actions.py | tests/fixtures/actions/ | Missing contract | No register_actions function |
| with_metadata.py | tests/fixtures/actions/ | Metadata logging | __tea_actions__ dict + register_actions |
| multi_action.py | tests/fixtures/actions/ | Multiple actions | register_actions with 3+ actions |

### Fixture Validation

Before test execution, verify:
```bash
cd python
ls -l tests/fixtures/actions/
# Should show: valid_actions.py, invalid_actions.py, with_metadata.py, multi_action.py

# Verify valid_actions.py has register_actions
grep -q "def register_actions" tests/fixtures/actions/valid_actions.py
echo $?  # Should be 0

# Verify with_metadata.py has __tea_actions__
grep -q "__tea_actions__" tests/fixtures/actions/with_metadata.py
echo $?  # Should be 0
```

---

## Appendix B: Mock Object Patterns

### Mock YAMLEngine
```python
from unittest.mock import MagicMock

def create_mock_engine():
    engine = MagicMock()
    engine.actions_registry = {}
    return engine
```

### Mock Module with register_actions
```python
def create_mock_module_with_actions():
    module = MagicMock()
    module.register_actions = MagicMock(
        side_effect=lambda reg, eng: reg.update({
            'action1': lambda **kw: {},
            'action2': lambda **kw: {}
        })
    )
    return module
```

### Mock Logger
```python
from unittest.mock import patch

@patch('the_edge_agent.yaml_imports.logger')
def test_logging(mock_logger):
    # Test code here
    mock_logger.info.assert_called()
    mock_logger.warning.assert_called_with(...)
```

---

**Document Version**: 1.0
**Last Updated**: 2025-12-27
**Status**: Ready for Review
**Approver**: Dev Team Lead
