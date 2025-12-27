# Test Design: TEA-PY-008.5 - Extract Configuration Module

## Document Information

| Field | Value |
|-------|-------|
| Story ID | TEA-PY-008.5 |
| Story Title | Extract Configuration Module |
| Test Designer | Quinn (Test Architect) |
| Date | 2025-12-27 |
| Status | Draft |

## Story Summary

Extract ~370 lines of configuration management, backend properties, and lifecycle methods from `yaml_engine.py` into a dedicated `yaml_config.py` module with an `EngineConfig` class. This refactoring must preserve:

1. **TEA-BUILTIN-005.3**: Opik configuration precedence (constructor > env > YAML > defaults)
2. **TEA-BUILTIN-006**: Memory infrastructure (Firestore, GCS, DuckDB)
3. Lifecycle management (resource cleanup, safe destruction)
4. Full backward compatibility with existing tests

## Test Strategy

### Scope

| In Scope | Out of Scope |
|----------|--------------|
| Module creation and structure | Performance optimization |
| EngineConfig class implementation | New feature development |
| Backend property delegation | Refactoring beyond extraction |
| Opik configuration precedence | Opik SDK internals |
| Memory infrastructure integration | Firebase/GCS internals |
| Lifecycle management (close, __del__) | Operating system resource management |
| Memory state serialization | Checkpoint backend implementation |
| YAMLEngine integration | YAML parsing logic |
| Backward compatibility | New API design |

### Test Levels

| Level | Focus | Coverage Target |
|-------|-------|-----------------|
| **Unit** | Individual methods in EngineConfig | 100% of EngineConfig class |
| **Integration** | EngineConfig ↔ YAMLEngine interaction | All delegation paths |
| **E2E** | Complete workflows with configuration | Critical user scenarios |

### Risk-Based Prioritization

| Priority | Description | Risk Level |
|----------|-------------|------------|
| **P0** | Critical functionality that must work | Breaking changes, data loss |
| **P1** | Important features with high usage | Degraded user experience |
| **P2** | Edge cases and error handling | Minor issues |
| **P3** | Nice-to-have validations | Cosmetic issues |

### Special Focus Areas

Based on story context, these areas require extra attention:

1. **Opik Configuration Precedence** (TEA-BUILTIN-005.3)
   - Test all four levels: constructor, env, YAML, defaults
   - Verify proper override behavior
   - Validate edge cases (empty values, None vs missing)

2. **Memory Infrastructure** (TEA-BUILTIN-006)
   - Test all backend types: MetadataStore, BlobStorage, QueryEngine, VectorIndex
   - Verify proper initialization and cleanup
   - Test state serialization/restoration

3. **Lifecycle Management**
   - Test idempotent close() behavior
   - Verify safe __del__() with exception handling
   - Test resource cleanup in all scenarios

## Test Scenarios

### Module Creation (AC 1-3)

#### TEA-PY-008.5-UNIT-001: Module file exists
- **Priority**: P0
- **AC Coverage**: AC 1
- **Objective**: Verify yaml_config.py is created in correct location
- **Preconditions**: Story implementation complete
- **Test Steps**:
  1. Check file exists at `python/src/the_edge_agent/yaml_config.py`
  2. Verify file is importable
- **Expected Results**:
  - File exists
  - No import errors
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-002: Module has comprehensive docstring
- **Priority**: P1
- **AC Coverage**: AC 2
- **Objective**: Verify module-level documentation quality
- **Preconditions**: yaml_config.py created
- **Test Steps**:
  1. Read module docstring
  2. Verify contains purpose description
  3. Verify contains usage examples
  4. Verify contains EngineConfig class reference
- **Expected Results**:
  - Docstring exists and is non-empty
  - Contains code examples showing EngineConfig usage
  - Documents key features (Opik, memory, lifecycle)
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-003: Module line count under 400
- **Priority**: P2
- **AC Coverage**: AC 3
- **Objective**: Ensure module stays focused and maintainable
- **Preconditions**: yaml_config.py fully implemented
- **Test Steps**:
  1. Count total lines in yaml_config.py (including comments, docstrings)
  2. Verify count is under 400
- **Expected Results**:
  - Line count < 400
- **Test Data**: N/A
- **Notes**: This is a maintainability constraint to prevent bloat

---

### EngineConfig Class (AC 4-8)

#### TEA-PY-008.5-UNIT-004: EngineConfig class exists
- **Priority**: P0
- **AC Coverage**: AC 4
- **Objective**: Verify EngineConfig class is defined correctly
- **Preconditions**: yaml_config.py created
- **Test Steps**:
  1. Import EngineConfig from yaml_config
  2. Verify it's a class
  3. Verify __init__ method exists
- **Expected Results**:
  - EngineConfig is importable
  - Is a class type
  - Has __init__ method
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-005: Constructor accepts engine reference
- **Priority**: P0
- **AC Coverage**: AC 5
- **Objective**: Verify EngineConfig can be initialized with engine reference
- **Preconditions**: Mock YAMLEngine instance available
- **Test Steps**:
  1. Create mock engine with required attributes (_metadata_store, _blob_storage, etc.)
  2. Instantiate EngineConfig(engine)
  3. Verify no exceptions
  4. Verify engine reference stored internally
- **Expected Results**:
  - EngineConfig initializes successfully
  - Stores engine reference as self._engine
- **Test Data**:
  ```python
  mock_engine = MagicMock()
  mock_engine._metadata_store = None
  mock_engine._blob_storage = None
  ```

#### TEA-PY-008.5-UNIT-006: resolve_opik_config method exists
- **Priority**: P0
- **AC Coverage**: AC 6
- **Objective**: Verify resolve_opik_config method is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig instance with mock engine
  2. Verify resolve_opik_config method exists
  3. Call with None (no YAML settings)
  4. Verify returns dict
- **Expected Results**:
  - Method exists and is callable
  - Returns dictionary with Opik config keys
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-007: Opik config precedence - defaults only
- **Priority**: P0
- **AC Coverage**: AC 6
- **Objective**: Test default Opik configuration values
- **Preconditions**: No env vars, no YAML settings, no constructor params
- **Test Steps**:
  1. Clear all Opik env vars
  2. Create EngineConfig with mock engine (no constructor Opik params)
  3. Call resolve_opik_config(None)
  4. Verify default values
- **Expected Results**:
  - enabled: False
  - api_key: None
  - workspace: None
  - project_name: "the-edge-agent"
  - url: None
  - llm_tracing: False
  - trace_export: False
- **Test Data**: N/A
- **Related**: TEA-BUILTIN-005.3

#### TEA-PY-008.5-UNIT-008: Opik config precedence - YAML overrides defaults
- **Priority**: P0
- **AC Coverage**: AC 6
- **Objective**: Verify YAML settings override defaults
- **Preconditions**: No env vars, no constructor params
- **Test Steps**:
  1. Clear all Opik env vars
  2. Create EngineConfig with mock engine
  3. Call resolve_opik_config with YAML settings:
     ```python
     {
       'enabled': True,
       'project_name': 'yaml-project',
       'workspace': 'yaml-workspace',
       'llm_tracing': True
     }
     ```
  4. Verify YAML values are used
- **Expected Results**:
  - enabled: True
  - project_name: "yaml-project"
  - workspace: "yaml-workspace"
  - llm_tracing: True
  - Other fields use defaults
- **Test Data**: See test steps
- **Related**: TEA-BUILTIN-005.3

#### TEA-PY-008.5-UNIT-009: Opik config precedence - env overrides YAML
- **Priority**: P0
- **AC Coverage**: AC 6
- **Objective**: Verify environment variables override YAML settings
- **Preconditions**: No constructor params
- **Test Steps**:
  1. Set env vars:
     - OPIK_PROJECT_NAME=env-project
     - OPIK_WORKSPACE=env-workspace
  2. Create EngineConfig with mock engine
  3. Call resolve_opik_config with YAML settings (different values)
  4. Verify env values win
- **Expected Results**:
  - project_name: "env-project" (from env, not YAML)
  - workspace: "env-workspace" (from env, not YAML)
- **Test Data**:
  - Env: OPIK_PROJECT_NAME=env-project
  - YAML: project_name: yaml-project
- **Related**: TEA-BUILTIN-005.3

#### TEA-PY-008.5-UNIT-010: Opik config precedence - constructor overrides all
- **Priority**: P0
- **AC Coverage**: AC 6
- **Objective**: Verify constructor params have highest precedence
- **Preconditions**: Env vars and YAML settings present
- **Test Steps**:
  1. Set env var: OPIK_PROJECT_NAME=env-project
  2. Create mock engine with constructor params stored:
     ```python
     engine._opik_project_name = "constructor-project"
     engine._opik_workspace = "constructor-workspace"
     ```
  3. Create EngineConfig(engine)
  4. Call resolve_opik_config with YAML settings
  5. Verify constructor values win
- **Expected Results**:
  - project_name: "constructor-project" (from constructor, not env or YAML)
  - workspace: "constructor-workspace" (from constructor)
- **Test Data**:
  - Constructor: opik_project_name="constructor-project"
  - Env: OPIK_PROJECT_NAME=env-project
  - YAML: project_name: yaml-project
- **Related**: TEA-BUILTIN-005.3

#### TEA-PY-008.5-UNIT-011: Opik config handles None vs missing
- **Priority**: P1
- **AC Coverage**: AC 6
- **Objective**: Test edge case where value is explicitly None vs not provided
- **Preconditions**: N/A
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Call resolve_opik_config with YAML containing explicit None:
     ```python
     {'project_name': None}
     ```
  3. Verify None is respected (doesn't fall through to default)
- **Expected Results**:
  - project_name: None (not "the-edge-agent")
- **Test Data**: See test steps
- **Notes**: Distinguish between absent key and explicit None

#### TEA-PY-008.5-UNIT-012: add_opik_exporter_from_config method exists
- **Priority**: P0
- **AC Coverage**: AC 7
- **Objective**: Verify method is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Verify add_opik_exporter_from_config method exists
  3. Call method (with mocked dependencies)
- **Expected Results**:
  - Method exists and is callable
  - No exceptions when called with valid config
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-013: add_opik_exporter when enabled=True
- **Priority**: P0
- **AC Coverage**: AC 7
- **Objective**: Verify OpikExporter is added when Opik is enabled
- **Preconditions**: Opik config resolved with enabled=True
- **Test Steps**:
  1. Create mock engine with trace context
  2. Create EngineConfig
  3. Set resolved config with enabled=True, valid api_key
  4. Call add_opik_exporter_from_config()
  5. Verify OpikExporter was created and added to trace context
- **Expected Results**:
  - OpikExporter instantiated with correct config
  - Exporter added to engine trace context
- **Test Data**:
  ```python
  config = {
    'enabled': True,
    'api_key': 'test-key',
    'project_name': 'test-project',
    'workspace': 'test-workspace'
  }
  ```
- **Notes**: May need to mock OpikExporter import

#### TEA-PY-008.5-UNIT-014: add_opik_exporter when enabled=False
- **Priority**: P1
- **AC Coverage**: AC 7
- **Objective**: Verify OpikExporter is NOT added when disabled
- **Preconditions**: Opik config resolved with enabled=False
- **Test Steps**:
  1. Create mock engine with trace context
  2. Create EngineConfig
  3. Set resolved config with enabled=False
  4. Call add_opik_exporter_from_config()
  5. Verify OpikExporter was NOT added
- **Expected Results**:
  - No OpikExporter created
  - Trace context unchanged
- **Test Data**: config = {'enabled': False}

#### TEA-PY-008.5-UNIT-015: configure_memory_infrastructure method exists
- **Priority**: P0
- **AC Coverage**: AC 8
- **Objective**: Verify method is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Verify configure_memory_infrastructure method exists
  3. Call with empty config {}
- **Expected Results**:
  - Method exists and is callable
  - No exceptions with empty config
- **Test Data**: config = {}

#### TEA-PY-008.5-UNIT-016: configure_memory_infrastructure with Firestore
- **Priority**: P0
- **AC Coverage**: AC 8
- **Objective**: Test MetadataStore configuration
- **Preconditions**: Mock create_metadata_store factory
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Mock create_metadata_store to return mock store
  3. Call configure_memory_infrastructure with:
     ```python
     {
       'memory': {
         'metadata_store': {
           'type': 'firestore',
           'project_id': 'test-project'
         }
       }
     }
     ```
  4. Verify create_metadata_store called with correct params
  5. Verify engine._metadata_store set
- **Expected Results**:
  - Factory called: create_metadata_store('firestore', project_id='test-project')
  - engine._metadata_store = returned instance
- **Test Data**: See test steps
- **Related**: TEA-BUILTIN-006

#### TEA-PY-008.5-UNIT-017: configure_memory_infrastructure with GCS
- **Priority**: P0
- **AC Coverage**: AC 8
- **Objective**: Test BlobStorage configuration
- **Preconditions**: Mock create_blob_storage factory
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Mock create_blob_storage
  3. Call configure_memory_infrastructure with GCS config
  4. Verify factory called and backend set
- **Expected Results**:
  - create_blob_storage('gcs', bucket='test-bucket') called
  - engine._blob_storage set
- **Test Data**:
  ```python
  {
    'memory': {
      'blob_storage': {
        'type': 'gcs',
        'bucket': 'test-bucket'
      }
    }
  }
  ```
- **Related**: TEA-BUILTIN-006

#### TEA-PY-008.5-UNIT-018: configure_memory_infrastructure with DuckDB
- **Priority**: P1
- **AC Coverage**: AC 8
- **Objective**: Test QueryEngine and VectorIndex configuration
- **Preconditions**: Mock create_query_engine, create_vector_index
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Mock DuckDB factories
  3. Call configure_memory_infrastructure with DuckDB config
  4. Verify both query_engine and vector_index set
- **Expected Results**:
  - create_query_engine('duckdb', path=':memory:') called
  - create_vector_index('duckdb', ...) called
  - Both backends set on engine
- **Test Data**:
  ```python
  {
    'memory': {
      'query_engine': {'type': 'duckdb', 'path': ':memory:'},
      'vector_index': {'type': 'duckdb', 'dimensions': 384}
    }
  }
  ```
- **Related**: TEA-BUILTIN-006

#### TEA-PY-008.5-UNIT-019: configure_memory_infrastructure missing config
- **Priority**: P2
- **AC Coverage**: AC 8
- **Objective**: Verify graceful handling when memory config absent
- **Preconditions**: N/A
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Call configure_memory_infrastructure with {}
  3. Verify no exceptions
  4. Verify backends remain None
- **Expected Results**:
  - No errors
  - engine._metadata_store = None
  - engine._blob_storage = None
  - etc.
- **Test Data**: config = {}

---

### Backend Properties (AC 9-13)

#### TEA-PY-008.5-UNIT-020: metadata_store property delegates
- **Priority**: P0
- **AC Coverage**: AC 9
- **Objective**: Verify metadata_store property returns engine backend
- **Preconditions**: Mock engine with _metadata_store
- **Test Steps**:
  1. Create mock engine with mock_store
  2. Create EngineConfig(engine)
  3. Access config.metadata_store
  4. Verify returns engine._metadata_store
- **Expected Results**:
  - Property returns same object as engine._metadata_store
- **Test Data**:
  ```python
  mock_store = MagicMock()
  engine._metadata_store = mock_store
  ```

#### TEA-PY-008.5-UNIT-021: blob_storage property delegates
- **Priority**: P0
- **AC Coverage**: AC 9
- **Objective**: Verify blob_storage property works
- **Preconditions**: Mock engine with _blob_storage
- **Test Steps**:
  1. Create mock engine with mock_storage
  2. Create EngineConfig(engine)
  3. Access config.blob_storage
  4. Verify returns engine._blob_storage
- **Expected Results**:
  - Property returns same object as engine._blob_storage
- **Test Data**: Similar to UNIT-020

#### TEA-PY-008.5-UNIT-022: query_engine property delegates
- **Priority**: P0
- **AC Coverage**: AC 10
- **Objective**: Verify query_engine property works
- **Preconditions**: Mock engine with _query_engine
- **Test Steps**:
  1. Create mock engine with mock_query_engine
  2. Create EngineConfig(engine)
  3. Access config.query_engine
  4. Verify returns engine._query_engine
- **Expected Results**:
  - Property returns same object as engine._query_engine
- **Test Data**: Similar to UNIT-020

#### TEA-PY-008.5-UNIT-023: vector_index property delegates
- **Priority**: P0
- **AC Coverage**: AC 10
- **Objective**: Verify vector_index property works
- **Preconditions**: Mock engine with _vector_index
- **Test Steps**:
  1. Create mock engine with mock_vector_index
  2. Create EngineConfig(engine)
  3. Access config.vector_index
  4. Verify returns engine._vector_index
- **Expected Results**:
  - Property returns same object as engine._vector_index
- **Test Data**: Similar to UNIT-020

#### TEA-PY-008.5-UNIT-024: embedding_fn property delegates
- **Priority**: P0
- **AC Coverage**: AC 11
- **Objective**: Verify embedding_fn property works
- **Preconditions**: Mock engine with _embedding_fn
- **Test Steps**:
  1. Create mock engine with mock_embedding_fn
  2. Create EngineConfig(engine)
  3. Access config.embedding_fn
  4. Verify returns engine._embedding_fn
- **Expected Results**:
  - Property returns same object as engine._embedding_fn
- **Test Data**:
  ```python
  mock_fn = lambda x: [0.1, 0.2, 0.3]
  engine._embedding_fn = mock_fn
  ```

#### TEA-PY-008.5-UNIT-025: opik_llm_tracing property delegates
- **Priority**: P0
- **AC Coverage**: AC 12
- **Objective**: Verify opik_llm_tracing property works
- **Preconditions**: Resolved Opik config
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Set resolved config with llm_tracing=True
  3. Access config.opik_llm_tracing
  4. Verify returns True
- **Expected Results**:
  - Property returns opik_config['llm_tracing']
- **Test Data**:
  ```python
  config._opik_config = {'llm_tracing': True}
  ```

#### TEA-PY-008.5-UNIT-026: opik_config property returns resolved config
- **Priority**: P0
- **AC Coverage**: AC 12
- **Objective**: Verify opik_config property returns full config
- **Preconditions**: Resolved Opik config
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Set resolved config
  3. Access config.opik_config
  4. Verify returns complete config dict
- **Expected Results**:
  - Property returns self._opik_config
  - Contains all expected keys (enabled, api_key, etc.)
- **Test Data**: Full Opik config dict

#### TEA-PY-008.5-UNIT-027: observability_context property delegates
- **Priority**: P0
- **AC Coverage**: AC 13
- **Objective**: Verify observability_context property works
- **Preconditions**: Mock engine with _trace_context
- **Test Steps**:
  1. Create mock engine with mock_trace_context
  2. Create EngineConfig(engine)
  3. Access config.observability_context
  4. Verify returns engine._trace_context
- **Expected Results**:
  - Property returns same object as engine._trace_context
- **Test Data**:
  ```python
  mock_context = MagicMock()
  engine._trace_context = mock_context
  ```

#### TEA-PY-008.5-UNIT-028: Properties return None when backends not configured
- **Priority**: P1
- **AC Coverage**: AC 9-13
- **Objective**: Verify properties gracefully return None
- **Preconditions**: Engine with no backends configured
- **Test Steps**:
  1. Create mock engine with all backends = None
  2. Create EngineConfig(engine)
  3. Access each property
  4. Verify all return None
- **Expected Results**:
  - metadata_store: None
  - blob_storage: None
  - query_engine: None
  - vector_index: None
  - embedding_fn: None
  - observability_context: None
- **Test Data**: N/A

---

### Lifecycle Management (AC 14-17)

#### TEA-PY-008.5-UNIT-029: close() method exists
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify close() method is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Verify close method exists
  3. Call close()
- **Expected Results**:
  - Method exists and is callable
  - No exceptions
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-030: close() cleans up metadata_store
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify metadata_store.close() is called
- **Preconditions**: Engine with metadata_store configured
- **Test Steps**:
  1. Create mock metadata_store with close() method
  2. Set engine._metadata_store = mock_store
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify mock_store.close() was called
  6. Verify engine._metadata_store set to None
- **Expected Results**:
  - mock_store.close() called once
  - engine._metadata_store = None after close
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-031: close() cleans up blob_storage
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify blob_storage.close() is called
- **Preconditions**: Engine with blob_storage configured
- **Test Steps**:
  1. Create mock blob_storage with close() method
  2. Set engine._blob_storage = mock_storage
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify mock_storage.close() called
  6. Verify engine._blob_storage = None
- **Expected Results**:
  - mock_storage.close() called once
  - engine._blob_storage = None after close
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-032: close() cleans up query_engine
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify query_engine.close() is called
- **Preconditions**: Engine with query_engine configured
- **Test Steps**:
  1. Create mock query_engine with close() method
  2. Set engine._query_engine = mock_engine
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify close() called and set to None
- **Expected Results**:
  - mock_engine.close() called once
  - engine._query_engine = None after close
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-033: close() cleans up vector_index
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify vector_index.close() is called
- **Preconditions**: Engine with vector_index configured
- **Test Steps**:
  1. Create mock vector_index with close() method
  2. Set engine._vector_index = mock_index
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify close() called and set to None
- **Expected Results**:
  - mock_index.close() called once
  - engine._vector_index = None after close
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-034: close() cleans up all backends
- **Priority**: P0
- **AC Coverage**: AC 14
- **Objective**: Verify all backends are cleaned up in one call
- **Preconditions**: Engine with all backends configured
- **Test Steps**:
  1. Create mocks for all backends (metadata_store, blob_storage, query_engine, vector_index)
  2. Set all on engine
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify all .close() methods called
  6. Verify all set to None
- **Expected Results**:
  - All backend .close() methods called
  - All engine backend attributes = None
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-035: close() is idempotent (safe to call multiple times)
- **Priority**: P0
- **AC Coverage**: AC 15
- **Objective**: Verify calling close() multiple times doesn't error
- **Preconditions**: EngineConfig with backends
- **Test Steps**:
  1. Create EngineConfig with mock backends
  2. Call config.close()
  3. Call config.close() again
  4. Call config.close() a third time
  5. Verify no exceptions
- **Expected Results**:
  - No exceptions on repeated calls
  - Backend close() only called once (on first call)
- **Test Data**: N/A
- **Notes**: Important for __del__ safety

#### TEA-PY-008.5-UNIT-036: close() handles backend without close() method
- **Priority**: P1
- **AC Coverage**: AC 14
- **Objective**: Verify graceful handling when backend lacks close()
- **Preconditions**: Mock backend without close() method
- **Test Steps**:
  1. Create mock backend without close() method
  2. Set engine._metadata_store = mock_backend
  3. Create EngineConfig(engine)
  4. Call config.close()
  5. Verify no exceptions
  6. Verify backend set to None anyway
- **Expected Results**:
  - No exceptions (AttributeError caught or checked)
  - engine._metadata_store = None
- **Test Data**: N/A
- **Notes**: Defensive programming

#### TEA-PY-008.5-UNIT-037: close() handles exception in backend.close()
- **Priority**: P1
- **AC Coverage**: AC 14
- **Objective**: Verify close() continues even if one backend fails
- **Preconditions**: Mock backend that raises on close()
- **Test Steps**:
  1. Create mock_store with close() that raises RuntimeError
  2. Create mock_storage with close() that succeeds
  3. Set both on engine
  4. Create EngineConfig(engine)
  5. Call config.close()
  6. Verify both are set to None despite exception
  7. Verify exception is logged but not raised
- **Expected Results**:
  - Both backends set to None
  - Error logged
  - No exception propagated
- **Test Data**: N/A
- **Notes**: Critical for robustness

#### TEA-PY-008.5-UNIT-038: __del__() method exists
- **Priority**: P0
- **AC Coverage**: AC 16
- **Objective**: Verify destructor is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Verify EngineConfig has __del__ method
  2. Create EngineConfig instance
  3. Delete instance (del config)
  4. Verify no exceptions
- **Expected Results**:
  - __del__ method exists
  - No exceptions during garbage collection
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-039: __del__() calls close() safely
- **Priority**: P0
- **AC Coverage**: AC 16
- **Objective**: Verify __del__ delegates to close()
- **Preconditions**: EngineConfig with mock backends
- **Test Steps**:
  1. Create EngineConfig with mock backends
  2. Mock the close() method to track calls
  3. Delete the EngineConfig instance
  4. Verify close() was called
- **Expected Results**:
  - close() called during __del__
  - All backends cleaned up
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-040: __del__() handles exception in close()
- **Priority**: P0
- **AC Coverage**: AC 16
- **Objective**: Verify __del__ doesn't propagate exceptions
- **Preconditions**: EngineConfig with close() that raises
- **Test Steps**:
  1. Create EngineConfig
  2. Mock close() to raise RuntimeError
  3. Delete the instance
  4. Verify no unhandled exception (should be caught/logged)
- **Expected Results**:
  - Exception caught and logged
  - No exception propagates from __del__
- **Test Data**: N/A
- **Notes**: Critical - exceptions in __del__ can cause issues

#### TEA-PY-008.5-UNIT-041: Backends set to None after close
- **Priority**: P0
- **AC Coverage**: AC 17
- **Objective**: Verify all backend attributes become None after close
- **Preconditions**: EngineConfig with all backends configured
- **Test Steps**:
  1. Create EngineConfig with all backends
  2. Verify backends are not None before close
  3. Call config.close()
  4. Verify all backends are None after close
- **Expected Results**:
  - Before: engine._metadata_store is not None
  - After: engine._metadata_store is None
  - Same for blob_storage, query_engine, vector_index
- **Test Data**: N/A

---

### Memory State (AC 18-20)

#### TEA-PY-008.5-UNIT-042: get_memory_state() method exists
- **Priority**: P0
- **AC Coverage**: AC 18
- **Objective**: Verify get_memory_state() is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Verify get_memory_state method exists
  3. Call method
- **Expected Results**:
  - Method exists and is callable
  - Returns dict
- **Test Data**: N/A

#### TEA-PY-008.5-UNIT-043: get_memory_state() returns serializable dict
- **Priority**: P0
- **AC Coverage**: AC 18
- **Objective**: Verify returned state is JSON-serializable
- **Preconditions**: Engine with memory backend
- **Test Steps**:
  1. Create mock memory backend with get_state() returning dict
  2. Set engine._memory_backend = mock_backend
  3. Create EngineConfig(engine)
  4. Call state = config.get_memory_state()
  5. Verify state is dict
  6. Verify json.dumps(state) succeeds
- **Expected Results**:
  - Returns dict
  - Dict is JSON-serializable
- **Test Data**:
  ```python
  mock_state = {'key': 'value', 'counter': 42}
  ```

#### TEA-PY-008.5-UNIT-044: get_memory_state() delegates to memory backend
- **Priority**: P0
- **AC Coverage**: AC 20
- **Objective**: Verify delegation to _memory_backend.get_state()
- **Preconditions**: Engine with memory backend
- **Test Steps**:
  1. Create mock memory backend
  2. Mock backend.get_state() to return specific dict
  3. Set engine._memory_backend = mock_backend
  4. Create EngineConfig(engine)
  5. Call config.get_memory_state()
  6. Verify backend.get_state() was called
  7. Verify returned value matches backend response
- **Expected Results**:
  - backend.get_state() called once
  - Return value matches mock response
- **Test Data**:
  ```python
  expected_state = {'namespace': 'default', 'data': {...}}
  mock_backend.get_state.return_value = expected_state
  ```

#### TEA-PY-008.5-UNIT-045: restore_memory_state() method exists
- **Priority**: P0
- **AC Coverage**: AC 19
- **Objective**: Verify restore_memory_state() is implemented
- **Preconditions**: EngineConfig class created
- **Test Steps**:
  1. Create EngineConfig with mock engine
  2. Verify restore_memory_state method exists
  3. Call with empty dict {}
- **Expected Results**:
  - Method exists and is callable
  - No exceptions with empty dict
- **Test Data**: state = {}

#### TEA-PY-008.5-UNIT-046: restore_memory_state() restores from checkpoint
- **Priority**: P0
- **AC Coverage**: AC 19
- **Objective**: Verify state restoration works correctly
- **Preconditions**: Engine with memory backend
- **Test Steps**:
  1. Create mock memory backend
  2. Mock backend.set_state() method
  3. Set engine._memory_backend = mock_backend
  4. Create EngineConfig(engine)
  5. Call config.restore_memory_state(state_dict)
  6. Verify backend.set_state() called with state_dict
- **Expected Results**:
  - backend.set_state(state_dict) called once
  - State passed correctly
- **Test Data**:
  ```python
  state_dict = {
    'namespace': 'default',
    'data': {'key1': 'value1', 'key2': 'value2'}
  }
  ```

#### TEA-PY-008.5-UNIT-047: restore_memory_state() delegates to memory backend
- **Priority**: P0
- **AC Coverage**: AC 20
- **Objective**: Verify delegation to _memory_backend.set_state()
- **Preconditions**: Engine with memory backend
- **Test Steps**:
  1. Create mock memory backend with set_state() method
  2. Set engine._memory_backend = mock_backend
  3. Create EngineConfig(engine)
  4. Call config.restore_memory_state(state)
  5. Verify backend.set_state(state) called
- **Expected Results**:
  - backend.set_state() called once with correct argument
- **Test Data**: See UNIT-046

#### TEA-PY-008.5-UNIT-048: Memory state round-trip preserves data
- **Priority**: P1
- **AC Coverage**: AC 18, 19
- **Objective**: Verify get -> restore cycle preserves data
- **Preconditions**: Engine with real memory backend
- **Test Steps**:
  1. Create EngineConfig with InMemoryBackend
  2. Store some data in memory backend
  3. Call state = config.get_memory_state()
  4. Clear memory backend
  5. Call config.restore_memory_state(state)
  6. Verify data restored correctly
- **Expected Results**:
  - Data after restore matches data before get
- **Test Data**:
  ```python
  original_data = {'user': 'alice', 'score': 100}
  ```
- **Notes**: Integration test to verify full cycle

---

### YAMLEngine Integration (AC 21-25)

#### TEA-PY-008.5-INTEGRATION-001: YAMLEngine has _engine_config attribute
- **Priority**: P0
- **AC Coverage**: AC 21
- **Objective**: Verify YAMLEngine creates EngineConfig instance
- **Preconditions**: yaml_config.py implemented
- **Test Steps**:
  1. Import YAMLEngine
  2. Create engine = YAMLEngine()
  3. Verify hasattr(engine, '_engine_config')
  4. Verify isinstance(engine._engine_config, EngineConfig)
- **Expected Results**:
  - _engine_config attribute exists
  - Is instance of EngineConfig
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-002: YAMLEngine.close() delegates to config
- **Priority**: P0
- **AC Coverage**: AC 22
- **Objective**: Verify YAMLEngine.close() calls EngineConfig.close()
- **Preconditions**: YAMLEngine with integration
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Mock engine._engine_config.close()
  3. Call engine.close()
  4. Verify _engine_config.close() was called
- **Expected Results**:
  - _engine_config.close() called once
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-003: YAMLEngine.metadata_store delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify property delegation works
- **Preconditions**: YAMLEngine with configured metadata_store
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Configure metadata_store via YAML
  3. Access engine.metadata_store
  4. Verify returns same as engine._engine_config.metadata_store
- **Expected Results**:
  - Property delegates correctly
  - Returns metadata_store instance
- **Test Data**:
  ```yaml
  settings:
    memory:
      metadata_store:
        type: firestore
        project_id: test
  ```

#### TEA-PY-008.5-INTEGRATION-004: YAMLEngine.blob_storage delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify blob_storage property delegation
- **Preconditions**: YAMLEngine with configured blob_storage
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Configure blob_storage via YAML
  3. Access engine.blob_storage
  4. Verify delegates to _engine_config.blob_storage
- **Expected Results**:
  - Property delegates correctly
- **Test Data**: Similar to INTEGRATION-003

#### TEA-PY-008.5-INTEGRATION-005: YAMLEngine.query_engine delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify query_engine property delegation
- **Preconditions**: YAMLEngine with configured query_engine
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Configure query_engine via YAML
  3. Access engine.query_engine
  4. Verify delegates correctly
- **Expected Results**:
  - Property delegates to _engine_config.query_engine
- **Test Data**: DuckDB configuration

#### TEA-PY-008.5-INTEGRATION-006: YAMLEngine.vector_index delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify vector_index property delegation
- **Preconditions**: YAMLEngine with configured vector_index
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Configure vector_index via YAML
  3. Access engine.vector_index
  4. Verify delegates correctly
- **Expected Results**:
  - Property delegates to _engine_config.vector_index
- **Test Data**: DuckDB vector configuration

#### TEA-PY-008.5-INTEGRATION-007: YAMLEngine.embedding_fn delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify embedding_fn property delegation
- **Preconditions**: YAMLEngine with embedding function
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Set embedding_fn
  3. Access engine.embedding_fn
  4. Verify delegates to _engine_config.embedding_fn
- **Expected Results**:
  - Property delegates correctly
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-008: YAMLEngine.opik_llm_tracing delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify opik_llm_tracing property delegation
- **Preconditions**: YAMLEngine with Opik config
- **Test Steps**:
  1. Create YAMLEngine with opik_llm_tracing=True
  2. Access engine.opik_llm_tracing
  3. Verify delegates to _engine_config.opik_llm_tracing
- **Expected Results**:
  - Property delegates correctly
  - Returns True
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-009: YAMLEngine.opik_config delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify opik_config property delegation
- **Preconditions**: YAMLEngine with Opik configuration
- **Test Steps**:
  1. Create YAMLEngine with Opik settings
  2. Access engine.opik_config
  3. Verify delegates to _engine_config.opik_config
  4. Verify returns complete config dict
- **Expected Results**:
  - Property delegates correctly
  - Returns dict with all Opik settings
- **Test Data**: Opik YAML config

#### TEA-PY-008.5-INTEGRATION-010: YAMLEngine.observability_context delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify observability_context property delegation
- **Preconditions**: YAMLEngine with trace context
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Access engine.observability_context
  3. Verify delegates to _engine_config.observability_context
- **Expected Results**:
  - Property delegates correctly
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-011: YAMLEngine._resolve_opik_config delegates
- **Priority**: P0
- **AC Coverage**: AC 24
- **Objective**: Verify internal method delegation
- **Preconditions**: YAMLEngine implementation
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Call engine._resolve_opik_config()
  3. Verify calls _engine_config.resolve_opik_config()
- **Expected Results**:
  - Method delegates to config
- **Test Data**: N/A
- **Notes**: May be called during __init__

#### TEA-PY-008.5-INTEGRATION-012: YAMLEngine._configure_memory_infrastructure delegates
- **Priority**: P0
- **AC Coverage**: AC 25
- **Objective**: Verify memory config delegation
- **Preconditions**: YAMLEngine implementation
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Call engine._configure_memory_infrastructure(config)
  3. Verify calls _engine_config.configure_memory_infrastructure(config)
- **Expected Results**:
  - Method delegates to config
- **Test Data**:
  ```python
  config = {'memory': {'metadata_store': {...}}}
  ```

#### TEA-PY-008.5-INTEGRATION-013: YAMLEngine.get_memory_state() delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify memory state getter delegation
- **Preconditions**: YAMLEngine with memory backend
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Call engine.get_memory_state()
  3. Verify calls _engine_config.get_memory_state()
- **Expected Results**:
  - Method delegates to config
  - Returns memory state dict
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-014: YAMLEngine.restore_memory_state() delegates
- **Priority**: P0
- **AC Coverage**: AC 23
- **Objective**: Verify memory state restoration delegation
- **Preconditions**: YAMLEngine with memory backend
- **Test Steps**:
  1. Create YAMLEngine instance
  2. Call engine.restore_memory_state(state)
  3. Verify calls _engine_config.restore_memory_state(state)
- **Expected Results**:
  - Method delegates to config
  - State restored correctly
- **Test Data**: state = {'namespace': 'default', 'data': {...}}

#### TEA-PY-008.5-INTEGRATION-015: No circular import between modules
- **Priority**: P0
- **AC Coverage**: AC 21
- **Objective**: Verify no circular import issues
- **Preconditions**: Both modules exist
- **Test Steps**:
  1. Import yaml_config
  2. Import yaml_engine
  3. Verify no ImportError
  4. Create instances of both
- **Expected Results**:
  - No circular import errors
  - Both modules work together
- **Test Data**: N/A
- **Notes**: Uses TYPE_CHECKING pattern

---

### Backward Compatibility (AC 26-28)

#### TEA-PY-008.5-INTEGRATION-016: All existing tests pass - memory tests
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify test_yaml_engine_memory.py passes unchanged
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest tests/test_yaml_engine_memory.py -v
  2. Verify all tests pass
  3. Verify no test modifications needed
- **Expected Results**:
  - 0 failures
  - 0 test file changes
- **Test Data**: N/A
- **Related**: TEA-BUILTIN-006

#### TEA-PY-008.5-INTEGRATION-017: All existing tests pass - Opik config tests
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify test_opik_configuration.py passes unchanged
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest tests/test_opik_configuration.py -v
  2. Verify all tests pass
  3. Verify no test modifications needed
- **Expected Results**:
  - 0 failures
  - 0 test file changes
- **Test Data**: N/A
- **Related**: TEA-BUILTIN-005.3

#### TEA-PY-008.5-INTEGRATION-018: All existing tests pass - Opik exporter tests
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify test_opik_exporter.py passes unchanged
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest tests/test_opik_exporter.py -v
  2. Verify all tests pass
- **Expected Results**:
  - 0 failures
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-019: All existing tests pass - Opik LLM tracing tests
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify test_opik_llm_tracing.py passes unchanged
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest tests/test_opik_llm_tracing.py -v
  2. Verify all tests pass
- **Expected Results**:
  - 0 failures
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-020: All existing tests pass - LTM backend tests
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify LTM tests pass unchanged
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest tests/test_ltm_*.py -v
  2. Verify all tests pass
- **Expected Results**:
  - 0 failures
- **Test Data**: N/A

#### TEA-PY-008.5-INTEGRATION-021: All existing tests pass - full suite
- **Priority**: P0
- **AC Coverage**: AC 26
- **Objective**: Verify entire test suite passes
- **Preconditions**: Refactoring complete
- **Test Steps**:
  1. Run: pytest python/tests/ -v
  2. Verify all 320+ tests pass
  3. Verify no tests skipped or modified
- **Expected Results**:
  - 320+ tests pass
  - 0 failures
  - 0 test modifications
- **Test Data**: N/A
- **Notes**: Critical - full regression validation

#### TEA-PY-008.5-INTEGRATION-022: Property access patterns preserved
- **Priority**: P0
- **AC Coverage**: AC 27
- **Objective**: Verify existing code can access properties unchanged
- **Preconditions**: YAMLEngine with backends
- **Test Steps**:
  1. Create YAMLEngine with all backends configured
  2. Access engine.metadata_store (as before)
  3. Access engine.blob_storage
  4. Access engine.query_engine
  5. Access engine.vector_index
  6. Access engine.opik_config
  7. Verify all work as before refactoring
- **Expected Results**:
  - All properties accessible
  - Return expected values
  - No API changes
- **Test Data**: Full backend configuration

#### TEA-PY-008.5-INTEGRATION-023: Configuration behavior identical
- **Priority**: P0
- **AC Coverage**: AC 28
- **Objective**: Verify Opik config resolution unchanged
- **Preconditions**: Multiple config sources
- **Test Steps**:
  1. Set constructor param: opik_project_name="constructor"
  2. Set env var: OPIK_WORKSPACE=env-workspace
  3. Set YAML: project_name: yaml-project
  4. Create YAMLEngine and load YAML
  5. Check engine.opik_config
  6. Verify precedence: constructor wins for project_name, env wins for workspace
- **Expected Results**:
  - Precedence behavior unchanged
  - Config values match pre-refactoring behavior
- **Test Data**: See test steps
- **Related**: TEA-BUILTIN-005.3

---

### End-to-End Scenarios (Critical Workflows)

#### TEA-PY-008.5-E2E-001: Complete workflow with memory infrastructure
- **Priority**: P0
- **AC Coverage**: AC 1-28
- **Objective**: Test full workflow with all components
- **Preconditions**: All implementation complete
- **Test Steps**:
  1. Create YAML file with:
     - Opik configuration (all sources)
     - Memory infrastructure config (Firestore, GCS, DuckDB)
     - Workflow nodes using memory actions
  2. Create YAMLEngine with constructor Opik params
  3. Load YAML file
  4. Execute workflow
  5. Verify memory backends work
  6. Verify Opik tracing works
  7. Get memory state
  8. Close engine
  9. Verify cleanup
- **Expected Results**:
  - Workflow executes successfully
  - All backends initialized
  - Opik config resolved correctly
  - Memory state retrievable
  - Clean shutdown
- **Test Data**: Complete YAML agent with all features
- **Notes**: Comprehensive integration test

#### TEA-PY-008.5-E2E-002: Checkpoint save and restore with new module
- **Priority**: P0
- **AC Coverage**: AC 18-20, 26
- **Objective**: Test checkpoint persistence works with extraction
- **Preconditions**: Engine with memory backend
- **Test Steps**:
  1. Create workflow with interrupt
  2. Execute until interrupt
  3. Call engine.get_memory_state()
  4. Save state to checkpoint
  5. Create new engine instance
  6. Restore state: engine.restore_memory_state(state)
  7. Resume execution
  8. Verify state preserved
- **Expected Results**:
  - State saves correctly
  - State restores correctly
  - Execution resumes from correct point
- **Test Data**: Interruptible workflow YAML
- **Related**: Checkpoint system

#### TEA-PY-008.5-E2E-003: Opik tracing end-to-end
- **Priority**: P0
- **AC Coverage**: AC 6-7, 12, 24
- **Objective**: Test Opik integration works end-to-end
- **Preconditions**: Mock Opik SDK available
- **Test Steps**:
  1. Set up Opik config (enabled=True, api_key, project)
  2. Create YAMLEngine with opik_llm_tracing=True
  3. Load workflow with LLM call
  4. Execute workflow
  5. Verify OpikExporter added
  6. Verify traces exported
  7. Close engine
- **Expected Results**:
  - OpikExporter initialized with correct config
  - LLM calls traced
  - Traces exported to Opik
- **Test Data**: LLM workflow YAML
- **Related**: TEA-BUILTIN-005.3
- **Notes**: May need Opik mocks

#### TEA-PY-008.5-E2E-004: Resource cleanup after exception
- **Priority**: P1
- **AC Coverage**: AC 14-17
- **Objective**: Verify resources cleaned up even with errors
- **Preconditions**: Engine with all backends
- **Test Steps**:
  1. Create YAMLEngine with all backends
  2. Configure memory infrastructure
  3. Execute workflow that raises exception
  4. Ensure exception propagates
  5. Call engine.close() in finally block
  6. Verify all backends cleaned up
- **Expected Results**:
  - Exception propagates
  - close() still works
  - All backends .close() called
  - All set to None
- **Test Data**: Workflow with deliberate error
- **Notes**: Tests robustness

#### TEA-PY-008.5-E2E-005: Multiple engines with different configs
- **Priority**: P2
- **AC Coverage**: AC 1-28
- **Objective**: Verify multiple engines can coexist
- **Preconditions**: Implementation complete
- **Test Steps**:
  1. Create engine1 with Opik project="project1"
  2. Create engine2 with Opik project="project2"
  3. Verify engine1.opik_config != engine2.opik_config
  4. Configure different memory backends for each
  5. Execute workflows in both
  6. Verify isolation
  7. Close both
- **Expected Results**:
  - Configs are independent
  - Backends don't interfere
  - Both clean up correctly
- **Test Data**: Two different YAML configs
- **Notes**: Tests instance isolation

---

## Test Data Requirements

### Opik Configuration Scenarios

| Scenario | Constructor | Env Vars | YAML Settings | Expected Result |
|----------|-------------|----------|---------------|-----------------|
| All defaults | None | None | None | project_name="the-edge-agent", enabled=False |
| YAML only | None | None | project_name: "yaml-proj" | project_name="yaml-proj" |
| Env only | None | OPIK_PROJECT_NAME=env-proj | None | project_name="env-proj" |
| Constructor only | opik_project_name="ctor-proj" | None | None | project_name="ctor-proj" |
| YAML + Env | None | OPIK_PROJECT_NAME=env-proj | project_name: "yaml-proj" | project_name="env-proj" (env wins) |
| All sources | opik_project_name="ctor" | OPIK_PROJECT_NAME=env | project_name: "yaml" | project_name="ctor" (constructor wins) |

### Memory Backend Configurations

| Backend | Type | Config Keys | Factory Called |
|---------|------|-------------|----------------|
| MetadataStore | firestore | project_id, credentials_path | create_metadata_store('firestore', ...) |
| BlobStorage | gcs | bucket, credentials_path | create_blob_storage('gcs', ...) |
| QueryEngine | duckdb | path, read_only | create_query_engine('duckdb', ...) |
| VectorIndex | duckdb | dimensions, distance_metric | create_vector_index('duckdb', ...) |

### Sample YAML Agent for E2E Testing

```yaml
name: test-config-module
description: E2E test for yaml_config extraction

settings:
  opik:
    enabled: true
    project_name: test-project
    workspace: test-workspace
    llm_tracing: true

  memory:
    metadata_store:
      type: firestore
      project_id: test-firebase-project

    blob_storage:
      type: gcs
      bucket: test-bucket

    query_engine:
      type: duckdb
      path: ":memory:"

    vector_index:
      type: duckdb
      dimensions: 384

state_schema:
  input: str
  output: str

nodes:
  - name: store_data
    run: |
      actions.memory_store("test_key", state["input"])
      return {"output": "stored"}

  - name: retrieve_data
    run: |
      value = actions.memory_retrieve("test_key")
      return {"output": value}

edges:
  - from: __start__
    to: store_data
  - from: store_data
    to: retrieve_data
  - from: retrieve_data
    to: __end__
```

---

## Coverage Matrix

| AC | Unit Tests | Integration Tests | E2E Tests | Total |
|----|-----------|-------------------|-----------|-------|
| 1 | UNIT-001 | - | - | 1 |
| 2 | UNIT-002 | - | - | 1 |
| 3 | UNIT-003 | - | - | 1 |
| 4 | UNIT-004 | - | - | 1 |
| 5 | UNIT-005 | - | - | 1 |
| 6 | UNIT-006 to 011 | - | E2E-003 | 7 |
| 7 | UNIT-012 to 014 | - | E2E-003 | 4 |
| 8 | UNIT-015 to 019 | INT-012 | E2E-001 | 7 |
| 9 | UNIT-020, 021, 028 | INT-003, 004 | - | 5 |
| 10 | UNIT-022, 023, 028 | INT-005, 006 | - | 4 |
| 11 | UNIT-024, 028 | INT-007 | - | 3 |
| 12 | UNIT-025, 026, 028 | INT-008, 009 | - | 5 |
| 13 | UNIT-027, 028 | INT-010 | - | 3 |
| 14 | UNIT-029 to 037 | INT-002 | E2E-004 | 11 |
| 15 | UNIT-035 | - | - | 1 |
| 16 | UNIT-038 to 040 | - | - | 3 |
| 17 | UNIT-041 | - | - | 1 |
| 18 | UNIT-042 to 044, 048 | INT-013 | E2E-002 | 6 |
| 19 | UNIT-045 to 048 | INT-014 | E2E-002 | 6 |
| 20 | UNIT-044, 047 | - | - | 2 |
| 21 | - | INT-001, 015 | - | 2 |
| 22 | - | INT-002 | - | 1 |
| 23 | - | INT-003 to 014 | - | 12 |
| 24 | - | INT-011 | - | 1 |
| 25 | - | INT-012 | - | 1 |
| 26 | - | INT-016 to 021 | - | 6 |
| 27 | - | INT-022 | - | 1 |
| 28 | - | INT-023 | E2E-001 | 2 |

**Total Test Scenarios**: 48 unit + 23 integration + 5 e2e = **76 tests**

---

## Test Execution Plan

### Phase 1: Unit Tests (Priority P0)
**Objective**: Validate EngineConfig class in isolation

**Tests**: UNIT-001 to UNIT-048 (P0 and P1 priority)

**Order**:
1. Module structure (UNIT-001 to 003)
2. Class and constructor (UNIT-004, 005)
3. Opik configuration (UNIT-006 to 011) - **Critical for TEA-BUILTIN-005.3**
4. Opik exporter (UNIT-012 to 014)
5. Memory infrastructure (UNIT-015 to 019) - **Critical for TEA-BUILTIN-006**
6. Backend properties (UNIT-020 to 028)
7. Lifecycle management (UNIT-029 to 041)
8. Memory state (UNIT-042 to 048)

**Tools**: pytest, unittest.mock

**Success Criteria**:
- All P0 unit tests pass
- 100% code coverage of EngineConfig class
- No regressions in Opik precedence logic
- No regressions in memory backend initialization

### Phase 2: Integration Tests (Priority P0)
**Objective**: Validate YAMLEngine ↔ EngineConfig interaction

**Tests**: INTEGRATION-001 to INTEGRATION-023

**Order**:
1. Module integration (INT-001, 015) - verify no circular imports
2. Delegation verification (INT-002 to 014) - all methods and properties
3. Backward compatibility (INT-016 to 023) - **Critical for AC 26-28**

**Tools**: pytest, real YAMLEngine instances, test fixtures

**Success Criteria**:
- All existing tests pass unchanged (320+ tests)
- All delegations work correctly
- No API changes detected
- Configuration behavior identical to pre-refactoring

### Phase 3: E2E Tests (Priority P0)
**Objective**: Validate complete workflows

**Tests**: E2E-001 to E2E-005

**Order**:
1. Complete workflow with all features (E2E-001)
2. Checkpoint save/restore (E2E-002)
3. Opik tracing end-to-end (E2E-003)
4. Error handling and cleanup (E2E-004)
5. Multi-instance isolation (E2E-005)

**Tools**: pytest, YAML fixtures, mock Opik SDK

**Success Criteria**:
- All critical workflows execute successfully
- State persistence works
- Opik integration works
- Resource cleanup is robust
- Multiple engines can coexist

### Phase 4: Regression Testing
**Objective**: Ensure no existing functionality broken

**Tests**: All existing test files
- tests/test_yaml_engine_memory.py
- tests/test_opik_configuration.py
- tests/test_opik_exporter.py
- tests/test_opik_llm_tracing.py
- tests/test_ltm_*.py

**Tools**: pytest with full suite

**Command**: `cd python && pytest tests/ -v --tb=short`

**Success Criteria**:
- 320+ tests pass
- 0 failures
- 0 modifications to test files
- Test execution time not significantly increased

---

## Risk Assessment and Mitigation

### High Risk Areas

| Risk | Impact | Likelihood | Mitigation | Test Coverage |
|------|--------|------------|------------|---------------|
| **Breaking Opik precedence** | High - Wrong config values used | Medium | UNIT-007 to 011, INT-023, E2E-003 | 9 tests |
| **Resource leak on close()** | High - Memory/connection leaks | Low | UNIT-029 to 041, E2E-004 | 13 tests |
| **Circular import** | High - Module fails to load | Low | INT-015, import tests | 2 tests |
| **Breaking existing tests** | High - Regressions | Medium | INT-016 to 021, Phase 4 | 6+ tests |
| **Memory state serialization** | Medium - Checkpoint broken | Low | UNIT-042 to 048, E2E-002 | 7 tests |
| **Property delegation broken** | Medium - API change | Low | INT-003 to 010, INT-022 | 10 tests |
| **__del__() exception** | Medium - Garbage collection issues | Low | UNIT-038 to 040 | 3 tests |
| **Backend initialization failure** | Medium - Memory features broken | Low | UNIT-015 to 019, INT-012 | 6 tests |

### Test Data Validation

All test scenarios must include:
1. **Clear preconditions**: What state must exist before test
2. **Explicit test data**: Actual values used, not placeholders
3. **Expected results**: Specific values, not "should work"
4. **Cleanup**: Restore env vars, close resources

### Traceability

Each test ID maps to:
- **AC Coverage**: Which acceptance criteria it validates
- **Story Reference**: TEA-PY-008.5
- **Related Features**: TEA-BUILTIN-005.3, TEA-BUILTIN-006
- **Priority**: P0-P3 based on risk and impact

---

## Test Environment Requirements

### Software Dependencies

| Dependency | Version | Purpose |
|------------|---------|---------|
| Python | 3.10+ | Runtime |
| pytest | Latest | Test framework |
| unittest.mock | Built-in | Mocking |
| firebase-admin | Latest (optional) | Firestore tests (can be mocked) |
| google-cloud-storage | Latest (optional) | GCS tests (can be mocked) |
| duckdb | Latest | QueryEngine/VectorIndex tests |
| opik | Latest (optional) | Opik tests (can be mocked) |

### Environment Variables

Tests must manage these env vars:
- `OPIK_API_KEY`
- `OPIK_WORKSPACE`
- `OPIK_PROJECT_NAME`
- `OPIK_URL_OVERRIDE`

**Important**: Use setUp/tearDown to isolate tests:
```python
def setUp(self):
    self._orig_env = {k: os.environ.get(k) for k in OPIK_VARS}
    for k in OPIK_VARS:
        if k in os.environ:
            del os.environ[k]

def tearDown(self):
    for k, v in self._orig_env.items():
        if v is None:
            os.environ.pop(k, None)
        else:
            os.environ[k] = v
```

### Mock Requirements

Mocks needed for:
1. **YAMLEngine**: For EngineConfig unit tests
2. **Memory backends**: MetadataStore, BlobStorage, QueryEngine, VectorIndex
3. **OpikExporter**: For Opik integration tests
4. **Opik SDK**: For LLM tracing tests (if not installed)
5. **Firebase/GCS clients**: For backend tests (avoid real API calls)

---

## Definition of Done

Test design is complete when:

- [ ] All 76 test scenarios documented
- [ ] Each test has clear objective, steps, expected results
- [ ] Test data specified for all scenarios
- [ ] Coverage matrix shows 100% AC coverage
- [ ] Risk assessment completed
- [ ] Test execution plan defined
- [ ] Phase 1-4 pass criteria documented
- [ ] Backward compatibility strategy defined
- [ ] Mock strategy documented
- [ ] Test environment requirements specified
- [ ] Traceability to ACs established
- [ ] Special focus areas (Opik, memory, lifecycle) covered
- [ ] Document reviewed by test architect (Quinn)

---

## Appendix A: Test Naming Conventions

### Test ID Format
`TEA-PY-008.5-{LEVEL}-{SEQ}`

- **LEVEL**: UNIT | INTEGRATION | E2E
- **SEQ**: 001, 002, 003, ...

### Test Function Naming
```python
def test_{feature}_{scenario}_{expected}():
    """TEA-PY-008.5-{LEVEL}-{SEQ}: Description."""
```

Example:
```python
def test_opik_config_precedence_constructor_wins():
    """TEA-PY-008.5-UNIT-010: Constructor params override env and YAML."""
```

---

## Appendix B: Related Documentation

| Document | Location | Purpose |
|----------|----------|---------|
| Story | docs/stories/TEA-PY-008.5-yaml-config-module.md | Requirements |
| Opik Config Spec | TEA-BUILTIN-005.3 | Precedence rules |
| Memory Infrastructure | TEA-BUILTIN-006 | Backend factories |
| Template Processor | TEA-PY-008.1 | Related extraction |
| Checkpoint Guide | docs/shared/architecture/checkpoint-guide.md | State serialization |
| Python Dev Guide | docs/python/development-guide.md | Testing guidelines |

---

## Appendix C: Test Implementation Example

Here's a sample implementation for one critical test:

```python
def test_opik_config_precedence_constructor_overrides_all():
    """
    TEA-PY-008.5-UNIT-010: Verify constructor params have highest precedence.

    Tests TEA-BUILTIN-005.3 requirement that constructor > env > YAML > defaults.
    """
    from unittest.mock import MagicMock
    from the_edge_agent.yaml_config import EngineConfig
    import os

    # Setup: Set all config sources with different values
    os.environ['OPIK_PROJECT_NAME'] = 'env-project'
    os.environ['OPIK_WORKSPACE'] = 'env-workspace'

    yaml_settings = {
        'project_name': 'yaml-project',
        'workspace': 'yaml-workspace',
        'llm_tracing': True
    }

    # Create mock engine with constructor params
    mock_engine = MagicMock()
    mock_engine._opik_project_name = 'constructor-project'
    mock_engine._opik_workspace = 'constructor-workspace'
    mock_engine._opik_llm_tracing = False

    # Execute
    config = EngineConfig(mock_engine)
    resolved = config.resolve_opik_config(yaml_settings)

    # Assert: Constructor wins
    assert resolved['project_name'] == 'constructor-project', \
        "Constructor should override env and YAML"
    assert resolved['workspace'] == 'constructor-workspace', \
        "Constructor should override env and YAML"
    assert resolved['llm_tracing'] == False, \
        "Constructor should override YAML"

    # Cleanup
    del os.environ['OPIK_PROJECT_NAME']
    del os.environ['OPIK_WORKSPACE']
```

---

**End of Test Design Document**

**Next Steps**:
1. Review by development team
2. Implementation of tests in parallel with code
3. Continuous execution during development
4. Final validation before merge
