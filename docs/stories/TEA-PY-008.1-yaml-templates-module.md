# Story TEA-PY-008.1: Extract Template Processing Module

## Status
Ready for Dev

> **QA Validation**: Passed story-draft-checklist (2025-12-27)
> - Clarity Score: 8/10
> - Complete code skeleton with method signatures provided
> - Minor: domain terms (object passthrough, StrictUndefined) could use brief explanations

**SM Validation:** 2025-12-27 - Story draft checklist PASSED (5/5 categories)

## Story

**As a** developer maintaining the YAMLEngine codebase,
**I want** template processing logic extracted into a dedicated `yaml_templates.py` module,
**so that** the core engine focuses on orchestration while template logic is isolated and testable.

## Context

### Existing System Integration

- **Integrates with**: `yaml_engine.py` YAMLEngine class
- **Technology**: Python 3.10+, Jinja2 templates
- **Follows pattern**: YE.5 extraction pattern with engine reference
- **Touch points**:
  - `load_from_dict()` - template context setup
  - `_create_inline_function()` - code template processing
  - `_create_action_function()` - parameter processing
  - `_add_edge_from_config()` - condition evaluation

### Methods to Extract (Lines 2326-2521, ~196 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `_process_template()` | 2326-2420 | Jinja2 template rendering |
| `_process_params()` | 2422-2449 | Recursive parameter processing |
| `_evaluate_condition()` | 2451-2497 | Condition expression evaluation |
| `_convert_simple_expression()` | 2499-2521 | Legacy expression conversion |

### Additional Context to Move

- Template cache (`self._template_cache`)
- Jinja2 environment initialization (stays shared, passed to module)

## Acceptance Criteria

### Module Creation
1. New file `python/src/the_edge_agent/yaml_templates.py` created
2. Module has comprehensive docstring with usage examples
3. Module under 300 lines total

### TemplateProcessor Class
4. `TemplateProcessor` class created with engine reference pattern
5. Constructor accepts `jinja_env`, `variables`, `secrets` references
6. `process_template(text, state, checkpoint_context)` method implemented
7. `process_params(params, state)` method implemented
8. `evaluate_condition(expr, state)` method implemented
9. `_convert_simple_expression(expr)` preserved for backward compatibility

### Template Features Preserved
10. `{{ state.key }}` variable access works identically
11. `{{ variables.key }}` and `{{ secrets.key }}` access works
12. `{{ checkpoint.dir }}` and `{{ checkpoint.last }}` work
13. Jinja2 filters (`| tojson`, `| upper`, `| fromjson`) work
14. Single expression object passthrough (AC: 5 from TEA-YAML-001) preserved
15. Template caching performance optimization (AC: 8) preserved
16. StrictUndefined error messages (AC: 7) preserved

### YAMLEngine Integration
17. `YAMLEngine._template_processor` attribute added
18. `YAMLEngine._process_template()` delegates to processor
19. `YAMLEngine._process_params()` delegates to processor
20. `YAMLEngine._evaluate_condition()` delegates to processor
21. Jinja2 environment stays in YAMLEngine (shared initialization)

### Backward Compatibility
22. All existing tests pass without modification
23. No public API changes to YAMLEngine
24. Template behavior identical before/after extraction

### Quality Requirements
25. Type hints on all methods
26. No circular imports
27. Comprehensive docstrings

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_templates.py module** (AC: 1-3)
  - [ ] Create `python/src/the_edge_agent/yaml_templates.py`
  - [ ] Add module docstring with examples
  - [ ] Add required imports (re, json, typing)
  - [ ] Import DotDict from yaml_engine (or move DotDict to this module)

- [ ] **Task 2: Implement TemplateProcessor class** (AC: 4-9)
  - [ ] Create `TemplateProcessor` class
  - [ ] Add `__init__(self, jinja_env, variables, secrets)` constructor
  - [ ] Move `_process_template()` logic to `process_template()`
  - [ ] Move `_process_params()` logic to `process_params()`
  - [ ] Move `_evaluate_condition()` logic to `evaluate_condition()`
  - [ ] Move `_convert_simple_expression()` as private method
  - [ ] Add template cache as instance attribute

- [ ] **Task 3: Preserve template features** (AC: 10-16)
  - [ ] Test state/variables/secrets access
  - [ ] Test checkpoint context access
  - [ ] Test Jinja2 filters work
  - [ ] Test object passthrough for single expressions
  - [ ] Verify template caching works
  - [ ] Verify StrictUndefined error messages

- [ ] **Task 4: Update YAMLEngine integration** (AC: 17-21)
  - [ ] Import TemplateProcessor in yaml_engine.py
  - [ ] Create `_template_processor` in `__init__`
  - [ ] Update `_process_template()` to delegate
  - [ ] Update `_process_params()` to delegate
  - [ ] Update `_evaluate_condition()` to delegate
  - [ ] Pass checkpoint context to processor calls

- [ ] **Task 5: Verify backward compatibility** (AC: 22-24)
  - [ ] Run full test suite: `pytest tests/test_yaml_engine*.py`
  - [ ] Verify template-specific tests pass
  - [ ] Run import verification script

- [ ] **Task 6: Code quality check** (AC: 25-27)
  - [ ] Add type hints to all methods
  - [ ] Verify no circular imports
  - [ ] Add comprehensive docstrings

## Dev Notes

### TemplateProcessor Pattern

```python
# yaml_templates.py
"""Template processing for YAMLEngine using Jinja2."""

import re
import json
from typing import Any, Dict, Optional
from jinja2 import Environment

class DotDict(dict):
    """Dictionary with attribute-style access (moved from yaml_engine)."""
    def __getattr__(self, key):
        try:
            value = self[key]
            if isinstance(value, dict) and not isinstance(value, DotDict):
                return DotDict(value)
            return value
        except KeyError:
            raise AttributeError(f"'{type(self).__name__}' has no attribute '{key}'")

    def __setattr__(self, key, value):
        self[key] = value


class TemplateProcessor:
    """Process Jinja2 templates for YAML configurations."""

    def __init__(
        self,
        jinja_env: Environment,
        variables: Dict[str, Any],
        secrets: Dict[str, Any],
    ):
        self._jinja_env = jinja_env
        self._variables = variables
        self._secrets = secrets
        self._template_cache: Dict[str, Any] = {}

    def process_template(
        self,
        text: str,
        state: Dict[str, Any],
        checkpoint_dir: Optional[str] = None,
        last_checkpoint: Optional[str] = None,
    ) -> Any:
        """Process template variables in text using Jinja2."""
        # Implementation moved from YAMLEngine._process_template
        ...

    def process_params(
        self,
        params: Dict[str, Any],
        state: Dict[str, Any],
        checkpoint_dir: Optional[str] = None,
        last_checkpoint: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Recursively process parameters, replacing template variables."""
        ...

    def evaluate_condition(self, expr: str, state: Dict[str, Any]) -> bool:
        """Evaluate a condition expression using Jinja2."""
        ...
```

### Integration in YAMLEngine

```python
# yaml_engine.py
from .yaml_templates import TemplateProcessor, DotDict

class YAMLEngine:
    def __init__(self, ...):
        # ... existing Jinja2 setup ...
        self._template_processor = TemplateProcessor(
            jinja_env=self._jinja_env,
            variables=self.variables,
            secrets=self.secrets,
        )

    def _process_template(self, text: str, state: Dict[str, Any]) -> Any:
        return self._template_processor.process_template(
            text, state,
            checkpoint_dir=self._checkpoint_dir,
            last_checkpoint=self._last_checkpoint_path,
        )
```

### Testing

- **Test file locations**: `tests/test_yaml_engine*.py`
- **Key test files**: `test_yaml_engine_core.py`, `test_yaml_engine_edges.py`
- **Run command**: `cd python && pytest tests/test_yaml_engine*.py -v`

### DotDict Location Decision

DotDict is used by both yaml_engine.py and the template processor. Options:
1. **Move to yaml_templates.py** (recommended) - Template processing owns it
2. Keep in yaml_engine.py and import - Creates import dependency
3. Create separate utils.py - Overhead for 15 lines

## Testing Requirements

- Test file location: `python/tests/test_yaml_engine*.py`
- Test standards: pytest, no mocking of template behavior
- No test modifications allowed (pure refactoring)

## Definition of Done

- [ ] yaml_templates.py created with TemplateProcessor class
- [ ] All template methods moved and working
- [ ] YAMLEngine delegates to TemplateProcessor
- [ ] All 320+ tests pass without modification
- [ ] No circular imports
- [ ] Module under 300 lines

## Risk Assessment

- **Primary Risk**: Template caching may have state leakage between tests
- **Mitigation**: Cache is per-processor instance, not class-level
- **Rollback**: Git revert single commit

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
