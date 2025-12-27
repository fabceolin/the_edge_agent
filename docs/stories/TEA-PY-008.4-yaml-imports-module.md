# Story TEA-PY-008.4: Extract Import Loader Module

## Status
Ready for Dev

> **QA Validation**: Passed story-draft-checklist (2025-12-27)
> - Clarity Score: 9/10
> - Line references verified against actual codebase
> - Minor: YE.5 reference could use formal document link

## Story

**As a** developer maintaining the YAMLEngine codebase,
**I want** external action import logic extracted into a dedicated `yaml_imports.py` module,
**so that** the ~215 lines of module loading code is isolated and the YE.6 external imports feature is maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml_engine.py`, `actions/` registry pattern
- **Technology**: Python 3.10+, importlib dynamic loading
- **Follows pattern**: YE.5 actions registration pattern
- **Touch points**:
  - `load_from_dict()` - calls `_load_imports()` for imports section
  - `actions_registry` - populated with namespaced actions
  - `_loaded_modules` - tracks circular import prevention

### Methods to Extract (Lines 2523-2737, ~215 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `_load_imports()` | 2523-2572 | Main import dispatcher |
| `_load_from_path()` | 2574-2638 | Load from local file |
| `_load_from_package()` | 2640-2687 | Load from installed package |
| `_log_module_metadata()` | 2689-2712 | Log __tea_actions__ metadata |
| `_merge_registry_with_namespace()` | 2714-2737 | Merge with namespace prefix |

### Key Features (YE.6)

1. **Path imports**: Load from local Python files relative to YAML
2. **Package imports**: Load from installed pip packages
3. **Namespacing**: Prefix actions with namespace (e.g., `custom.my_action`)
4. **Circular import prevention**: Track loaded modules
5. **Contract validation**: Require `register_actions(registry, engine)` function
6. **Metadata logging**: Optional `__tea_actions__` for version/description

## Acceptance Criteria

### Module Creation
1. New file `python/src/the_edge_agent/yaml_imports.py` created
2. Module has comprehensive docstring with examples
3. Module under 250 lines total

### ImportLoader Class
4. `ImportLoader` class created with engine reference pattern
5. Constructor accepts engine reference for registry access
6. `load_imports(imports, yaml_dir)` method implemented
7. Tracks loaded modules to prevent circular imports

### Path Import Support
8. Relative paths resolved from YAML file location
9. Absolute paths work unchanged
10. FileNotFoundError raised for missing files
11. Module loaded dynamically with unique name
12. `register_actions(registry, engine)` validated

### Package Import Support
13. Installed packages loaded via `importlib.import_module`
14. ImportError with helpful message for missing packages
15. `register_actions(registry, engine)` validated

### Namespace Handling
16. Actions prefixed with namespace: `namespace.action_name`
17. Empty namespace registers at root level
18. Override warnings logged for duplicate actions

### Metadata Logging
19. `__tea_actions__` metadata logged at INFO level
20. Version, description, actions list logged if present
21. Graceful handling of missing metadata

### YAMLEngine Integration
22. `YAMLEngine._import_loader` attribute added
23. `YAMLEngine._load_imports()` delegates to loader
24. `_loaded_modules` tracked on loader instance
25. `actions_registry` passed to loader for population

### Backward Compatibility
26. All existing tests pass without modification
27. Import behavior identical before/after extraction
28. Error messages unchanged

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_imports.py module** (AC: 1-3)
  - [ ] Create `python/src/the_edge_agent/yaml_imports.py`
  - [ ] Add module docstring with examples
  - [ ] Add required imports (os, importlib, logging, typing)

- [ ] **Task 2: Implement ImportLoader class** (AC: 4-7)
  - [ ] Create `ImportLoader` class
  - [ ] Add constructor with engine reference
  - [ ] Store actions_registry reference
  - [ ] Initialize `_loaded_modules` set
  - [ ] Implement `load_imports()` dispatcher

- [ ] **Task 3: Move path import logic** (AC: 8-12)
  - [ ] Move `_load_from_path()` method
  - [ ] Preserve relative path resolution
  - [ ] Preserve circular import check
  - [ ] Preserve contract validation
  - [ ] Preserve dynamic module loading

- [ ] **Task 4: Move package import logic** (AC: 13-15)
  - [ ] Move `_load_from_package()` method
  - [ ] Preserve circular import check
  - [ ] Preserve contract validation
  - [ ] Preserve helpful error messages

- [ ] **Task 5: Move namespace handling** (AC: 16-18)
  - [ ] Move `_merge_registry_with_namespace()` method
  - [ ] Preserve namespace prefix logic
  - [ ] Preserve override warning

- [ ] **Task 6: Move metadata logging** (AC: 19-21)
  - [ ] Move `_log_module_metadata()` method
  - [ ] Preserve all logging behavior

- [ ] **Task 7: Update YAMLEngine integration** (AC: 22-25)
  - [ ] Import ImportLoader in yaml_engine.py
  - [ ] Create `_import_loader` in `__init__`
  - [ ] Update `_load_imports()` to delegate
  - [ ] Remove `_loaded_modules` from engine (now on loader)

- [ ] **Task 8: Verify backward compatibility** (AC: 26-28)
  - [ ] Run import tests: `pytest tests/test_yaml_engine_imports.py`
  - [ ] Verify error messages match
  - [ ] Verify logging output matches

## Dev Notes

### ImportLoader Pattern

```python
# yaml_imports.py
"""External action module loading for YAMLEngine."""

import os
import importlib
import importlib.util
import logging
from typing import Any, Callable, Dict, List, Optional, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine

logger = logging.getLogger(__name__)


class ImportLoader:
    """Loader for external action modules from paths and packages."""

    def __init__(self, engine: 'YAMLEngine'):
        """
        Initialize with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - actions_registry for action registration
        """
        self._engine = engine
        self._loaded_modules: Set[str] = set()

    def load_imports(
        self,
        imports: List[Dict[str, Any]],
        yaml_dir: Optional[str] = None,
    ) -> None:
        """
        Load external action modules from the imports section.

        Args:
            imports: List of import configurations from YAML
            yaml_dir: Directory of the YAML file for relative path resolution
        """
        ...

    def _load_from_path(
        self, path: str, namespace: str, yaml_dir: Optional[str] = None
    ) -> None:
        """Load actions from a local Python file."""
        ...

    def _load_from_package(self, package: str, namespace: str) -> None:
        """Load actions from an installed Python package."""
        ...

    def _log_module_metadata(
        self, module: Any, source: str, namespace: str
    ) -> None:
        """Log optional __tea_actions__ metadata."""
        ...

    def _merge_registry_with_namespace(
        self,
        local_registry: Dict[str, Callable],
        namespace: str,
        source: str,
    ) -> None:
        """Merge actions into main registry with namespace prefix."""
        ...
```

### Integration in YAMLEngine

```python
# yaml_engine.py
from .yaml_imports import ImportLoader

class YAMLEngine:
    def __init__(self, ...):
        # ... after actions_registry is set ...
        self._import_loader = ImportLoader(self)

    def _load_imports(self, imports, yaml_dir=None):
        return self._import_loader.load_imports(imports, yaml_dir)

    # Remove self._loaded_modules (now on loader)
```

### Contract Validation

Imported modules MUST have:
```python
def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""
    registry['my_action'] = lambda state, **kw: {...}
```

Optional metadata:
```python
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Custom actions for Slack integration",
    "actions": ["send_message", "list_channels"]
}
```

### Testing Requirements

- **Test file**: `tests/test_yaml_engine_imports.py`
- **Test fixtures**: `tests/fixtures/actions/` directory
- **Run command**: `cd python && pytest tests/test_yaml_engine_imports.py -v`

## Definition of Done

- [ ] yaml_imports.py created with ImportLoader class
- [ ] All import methods moved and working
- [ ] YAMLEngine delegates to ImportLoader
- [ ] Circular import prevention preserved
- [ ] All 320+ tests pass without modification
- [ ] No circular imports in codebase
- [ ] Module under 250 lines

## Risk Assessment

- **Primary Risk**: Breaking relative path resolution
- **Mitigation**: Preserve exact path resolution logic, test with fixtures

- **Secondary Risk**: Losing loaded_modules state
- **Mitigation**: State is now on ImportLoader instance, passed via engine

- **Rollback**: Git revert single commit

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
