# Changelog

All notable changes to tea-wasm-llm will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added (TEA-WASM-001: YAML Engine Expansion)

#### TEA-WASM-001.1: YAML Config Parsing
- Full YAML parsing for TEA format workflows
- `parse_yaml_config()` function for parsing YAML strings
- `WasmYamlConfig` struct with nodes, edges, variables, settings
- Support for `state_schema`, `variables`, `nodes`, and `edges` sections
- Node configuration with `action`, `with`, `output`, `goto` fields

#### TEA-WASM-001.2: Tera Template Integration
- Tera template engine integration (Jinja2-compatible syntax)
- `render_template()` function for template rendering
- Template variables: `{{ state.key }}`, `{{ variables.key }}`
- Built-in filters: `upper`, `lower`, `tojson`, `length`, `first`, `last`, `default`
- Math expressions in templates: `{{ state.count * 2 }}`
- Conditional expressions: `{% if condition %}...{% endif %}`
- Loop expressions: `{% for item in list %}...{% endfor %}`

#### TEA-WASM-001.3: Conditional Edge Routing
- Goto-based conditional routing
- `goto:` block with `if`/`to` conditions
- Expression evaluation for routing conditions
- Support for comparison operators: `==`, `!=`, `>`, `<`, `>=`, `<=`
- Support for logical operators: `and`, `or`, `not`
- Default routing (no condition) as fallback
- Special targets: `__end__` for workflow termination

#### TEA-WASM-001.4: Async Node Executor
- Asynchronous workflow execution with `execute_workflow_async()`
- `ExecutionOptions` for configuring workflow execution
- Template processing for `with:` block parameters
- Result storage at `output:` path
- Sequential node execution following goto routing
- Loop detection and prevention

#### TEA-WASM-001.5: Simulated Parallel Execution
- Parallel group detection for fan-out/fan-in patterns
- `ParallelGroup` struct for grouping parallel branches
- Four merge strategies:
  - `Isolated` (default): Branches see only original state
  - `LastWriteWins`: Later branches overwrite earlier
  - `MergeDeep`: Deep merge of all branch results
  - `FailOnConflict`: Error on conflicting state mutations
- Conflict detection for parallel state mutations
- `parallel_results` available in fan-in nodes
- Configurable via `settings.parallel.merge_strategy`

#### TEA-WASM-001.6: Action Parameter Standardization
- `extract_params()` for template-processed parameter extraction
- `validate_params()` for required parameter validation
- `apply_defaults()` for optional parameter defaults
- `set_at_path()` for nested dot-notation output paths
- `get_at_path()` for reading nested state values
- `ActionDef` struct for action parameter definitions
- Defined actions: `llm.call`, `llm.embed`, `llm.stream`, `storage.*`, `ltm.*`, `lua.eval`, `prolog.query`, `duckdb.*`, `return`, `passthrough`
- Clear error messages for missing/invalid parameters

#### TEA-WASM-001.7: Integration Testing & Documentation
- 13 example YAML workflow tests covering:
  - Simple templates and greetings
  - Template filters (upper, lower)
  - Variables in templates
  - Sequential node execution
  - Conditional routing with goto
  - Loops with iteration
  - Passthrough nodes
  - Nested output structures
  - Complex template expressions
- Updated README with YAML engine documentation
- API documentation for new public functions

### Changed
- `WasmNodeConfig` now uses `params` (alias for `with`) for parameter storage
- Template processing is recursive for nested objects and arrays

### Dependencies
- Added `lazy_static = "1.4"` for action definitions

## [0.9.x] - Previous Releases

See git history for changes prior to TEA-WASM-001 epic.
