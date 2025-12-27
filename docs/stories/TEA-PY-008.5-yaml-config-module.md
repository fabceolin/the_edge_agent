# Story TEA-PY-008.5: Extract Configuration Module

## Status
Ready for Dev

> **QA Validation**: Passed story-draft-checklist (2025-12-27)
> - Clarity Score: 8/10
> - ~130 lines of reference implementation code provided
> - Minor: TEA-PY-008.1 dependency relationship could be clarified

## Story

**As a** developer maintaining the YAMLEngine codebase,
**I want** configuration management, backend properties, and lifecycle methods extracted into a dedicated `yaml_config.py` module,
**so that** the ~370 lines of infrastructure configuration code is isolated and the TEA-BUILTIN-005.3/TEA-BUILTIN-006 features are maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml_engine.py`, `memory/` backends, `exporters.py`
- **Technology**: Python 3.10+, Firebase, Opik, DuckDB
- **Follows pattern**: YE.5 extraction with engine reference
- **Touch points**:
  - `load_from_dict()` - calls `_configure_memory_infrastructure()`
  - `__init__` - calls `_resolve_opik_config()`
  - Memory backend factories from `memory/`
  - OpikExporter from `exporters.py`

### Methods to Extract (Lines 539-908, ~370 lines)

| Method/Property | Lines | Purpose |
|----------------|-------|---------|
| `metadata_store` property | 539-547 | MetadataStore accessor |
| `blob_storage` property | 550-558 | BlobStorage accessor |
| `query_engine` property | 561-569 | QueryEngine accessor |
| `vector_index` property | 572-580 | VectorIndex accessor |
| `embedding_fn` property | 583-591 | Embedding function accessor |
| `opik_llm_tracing` property | 594-604 | Opik LLM tracing flag |
| `opik_config` property | 607-629 | Resolved Opik config |
| `observability_context` property | 632-655 | ObservabilityContext accessor |
| `_resolve_opik_config()` | 657-720 | Opik configuration resolution |
| `_add_opik_exporter_from_config()` | 722-752 | Opik exporter setup |
| `_configure_memory_infrastructure()` | 754-829 | Firebase/memory setup |
| `close()` | 831-873 | Resource cleanup |
| `__del__()` | 875-880 | Destructor |
| `get_memory_state()` | 882-895 | Checkpoint memory state |
| `restore_memory_state()` | 897-908 | State restoration |

### Key Features to Preserve

1. **Opik Configuration (TEA-BUILTIN-005.3)**:
   - Precedence: constructor > env vars > YAML > defaults
   - Native LLM tracing support
   - Trace export to Opik

2. **Memory Infrastructure (TEA-BUILTIN-006)**:
   - Firestore MetadataStore
   - GCS BlobStorage
   - DuckDB QueryEngine and VectorIndex

3. **Lifecycle Management**:
   - Proper resource cleanup in `close()`
   - Safe destructor with exception handling

## Acceptance Criteria

### Module Creation
1. New file `python/src/the_edge_agent/yaml_config.py` created
2. Module has comprehensive docstring with examples
3. Module under 400 lines total

### EngineConfig Class
4. `EngineConfig` class created with engine reference pattern
5. Constructor accepts engine reference for backend access
6. `resolve_opik_config(yaml_settings)` method implemented
7. `add_opik_exporter_from_config()` method implemented
8. `configure_memory_infrastructure(config)` method implemented

### Backend Properties
9. Accessor methods for metadata_store, blob_storage work
10. Accessor methods for query_engine, vector_index work
11. Accessor for embedding_fn works
12. Accessor for opik_llm_tracing, opik_config work
13. Accessor for observability_context works

### Lifecycle Management
14. `close()` method cleans up all backends
15. `close()` is safe to call multiple times
16. `__del__()` calls `close()` safely
17. Memory backends set to None after closing

### Memory State
18. `get_memory_state()` returns serializable state
19. `restore_memory_state(state)` restores from checkpoint
20. Memory backend delegation preserved

### YAMLEngine Integration
21. `YAMLEngine._engine_config` attribute added
22. `YAMLEngine.close()` delegates to config
23. All property accessors delegate to EngineConfig
24. `_resolve_opik_config()` delegates to config
25. `_configure_memory_infrastructure()` delegates to config

### Backward Compatibility
26. All existing tests pass without modification
27. Property access patterns preserved on YAMLEngine
28. Configuration behavior identical before/after extraction

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_config.py module** (AC: 1-3)
  - [ ] Create `python/src/the_edge_agent/yaml_config.py`
  - [ ] Add module docstring with examples
  - [ ] Add required imports (os, logging, typing)
  - [ ] Import memory factories and exporters

- [ ] **Task 2: Implement EngineConfig class** (AC: 4-8)
  - [ ] Create `EngineConfig` class
  - [ ] Add constructor with engine reference
  - [ ] Store constructor Opik params for precedence
  - [ ] Move `_resolve_opik_config()` method
  - [ ] Move `_add_opik_exporter_from_config()` method
  - [ ] Move `_configure_memory_infrastructure()` method

- [ ] **Task 3: Move backend property accessors** (AC: 9-13)
  - [ ] Move `metadata_store` property
  - [ ] Move `blob_storage` property
  - [ ] Move `query_engine` property
  - [ ] Move `vector_index` property
  - [ ] Move `embedding_fn` property
  - [ ] Move `opik_llm_tracing` property
  - [ ] Move `opik_config` property
  - [ ] Move `observability_context` property

- [ ] **Task 4: Move lifecycle methods** (AC: 14-17)
  - [ ] Move `close()` method
  - [ ] Preserve idempotent close behavior
  - [ ] Move `__del__()` destructor
  - [ ] Preserve exception handling in destructor

- [ ] **Task 5: Move memory state methods** (AC: 18-20)
  - [ ] Move `get_memory_state()` method
  - [ ] Move `restore_memory_state()` method
  - [ ] Preserve memory backend delegation

- [ ] **Task 6: Update YAMLEngine integration** (AC: 21-25)
  - [ ] Import EngineConfig in yaml_engine.py
  - [ ] Create `_engine_config` in `__init__`
  - [ ] Delegate property accessors to config
  - [ ] Delegate lifecycle methods to config
  - [ ] Delegate memory state methods to config

- [ ] **Task 7: Verify backward compatibility** (AC: 26-28)
  - [ ] Run memory tests: `pytest tests/test_yaml_engine_memory.py`
  - [ ] Run config tests: `pytest tests/test_opik*.py`
  - [ ] Verify property access works unchanged

## Dev Notes

### EngineConfig Pattern

```python
# yaml_config.py
"""Configuration management for YAMLEngine."""

import os
import logging
from typing import Any, Callable, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine
    from .observability import ObservabilityContext

from .memory import (
    create_metadata_store,
    create_blob_storage,
    create_query_engine,
    create_vector_index,
)

logger = logging.getLogger(__name__)


class EngineConfig:
    """Configuration and lifecycle management for YAMLEngine."""

    def __init__(self, engine: 'YAMLEngine'):
        """
        Initialize with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - Backend storage references
                - Constructor Opik parameters
                - Memory backend reference
                - Trace context for exporters
        """
        self._engine = engine
        self._opik_config: Dict[str, Any] = {}

    def resolve_opik_config(
        self, yaml_settings: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Resolve Opik configuration with proper precedence hierarchy.

        Precedence (highest to lowest):
        1. Constructor parameters
        2. Environment variables
        3. YAML settings
        4. Defaults
        """
        ...

    def add_opik_exporter_from_config(self) -> None:
        """Add an OpikExporter to the trace context using resolved config."""
        ...

    def configure_memory_infrastructure(self, config: Dict[str, Any]) -> None:
        """
        Configure Firebase Agent Memory Infrastructure from YAML settings.

        TEA-BUILTIN-006: Allows runtime configuration of memory backends via YAML.
        """
        ...

    @property
    def metadata_store(self) -> Optional[Any]:
        """Get the metadata store instance."""
        return self._engine._metadata_store

    @property
    def blob_storage(self) -> Optional[Any]:
        """Get the blob storage instance."""
        return self._engine._blob_storage

    # ... other properties ...

    def close(self) -> None:
        """
        Close all backends and release resources.

        Should be called when the engine is no longer needed.
        Safe to call multiple times.
        """
        ...

    def get_memory_state(self) -> Dict[str, Any]:
        """Get serializable memory state for checkpoint persistence."""
        return self._engine._memory_backend.get_state()

    def restore_memory_state(self, state: Dict[str, Any]) -> None:
        """Restore memory state from checkpoint."""
        self._engine._memory_backend.set_state(state)
```

### Integration in YAMLEngine

```python
# yaml_engine.py
from .yaml_config import EngineConfig

class YAMLEngine:
    def __init__(self, ...):
        # ... existing setup ...

        # Initialize config manager after backends are set
        self._engine_config = EngineConfig(self)
        self._opik_config = self._engine_config.resolve_opik_config()

    @property
    def metadata_store(self):
        return self._engine_config.metadata_store

    @property
    def blob_storage(self):
        return self._engine_config.blob_storage

    # ... other delegated properties ...

    def close(self):
        return self._engine_config.close()

    def get_memory_state(self):
        return self._engine_config.get_memory_state()

    def restore_memory_state(self, state):
        return self._engine_config.restore_memory_state(state)
```

### Opik Configuration Precedence

```python
# Precedence order (TEA-BUILTIN-005.3):
# 1. Constructor: YAMLEngine(opik_project_name="my-project")
# 2. Environment: OPIK_PROJECT_NAME=my-project
# 3. YAML: settings.opik.project_name: my-project
# 4. Default: "the-edge-agent"
```

### Testing Requirements

- **Test files**: `tests/test_yaml_engine*.py`, `tests/test_opik*.py`
- **Memory tests**: `tests/test_ltm_backend.py`
- **Run command**: `cd python && pytest tests/test_yaml_engine*.py tests/test_opik*.py -v`

## Definition of Done

- [ ] yaml_config.py created with EngineConfig class
- [ ] All configuration methods moved and working
- [ ] All property accessors delegating correctly
- [ ] YAMLEngine delegates lifecycle to EngineConfig
- [ ] Opik precedence preserved (constructor > env > YAML > defaults)
- [ ] All 320+ tests pass without modification
- [ ] No circular imports
- [ ] Module under 400 lines

## Risk Assessment

- **Primary Risk**: Breaking Opik configuration precedence
- **Mitigation**: Preserve exact precedence logic, test with different config sources

- **Secondary Risk**: Resource leak if close() not called
- **Mitigation**: `__del__` safety preserved, all close paths tested

- **Rollback**: Git revert single commit

## Dependency

- **Depends on**: TEA-PY-008.1 (TemplateProcessor provides evaluation)
- **Independent of**: TEA-PY-008.2, TEA-PY-008.3, TEA-PY-008.4

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
