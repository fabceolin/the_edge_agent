# TEA-RUST-034: External Imports Support

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RUST-034 |
| **Type** | Story |
| **Priority** | High |
| **Estimated Effort** | 13 points |
| **Status** | Draft |
| **Parent Epic** | TEA-RUST-001 |
| **Depends On** | TEA-RUST-004 (Node Execution), TEA-RUST-013 (CLI), TEA-CLI-004 (CLI Parity) |
| **Python Parity** | TEA-CLI-002, YE.6 |
| **Files to Modify** | `rust/src/engine/yaml.rs`, `rust/src/engine/executor.rs`, `rust/src/actions/mod.rs` |

## Description

**As a** workflow author using the Rust implementation,
**I want** to import external Lua modules and built-in action sets via the `imports` YAML section,
**So that** I can reuse actions across workflows and have parity with the Python implementation.

## Background

### Current State

The Rust implementation **parses** the `imports` section but **does not process it**:

```yaml
# This YAML is parsed but imports are IGNORED in Rust
imports:
  - path: ./actions/custom.lua
    namespace: custom
  - builtin: web
    namespace: http
```

The Python implementation fully supports:
- Loading local Python files (`path:`)
- Loading installed packages (`package:`)
- Built-in action sets (`builtin:`)
- Namespace prefixing (`namespace:`)
- Circular import detection

### Problem

Users cannot:
1. Share actions across multiple YAML workflows in Rust
2. Use built-in action sets (web, llm, memory) declaratively
3. Create reusable action libraries
4. Have workflow parity between Python and Rust runtimes

### Solution

Implement the imports system in Rust:
1. **Local Lua files** - Load and execute Lua modules that register actions
2. **Built-in sets** - Map `builtin: web` to `actions::web::register()`
3. **Namespace prefixing** - Prefix action names with namespace

**Note**: The Python-style `package:` import (for installed pip packages) has no direct Rust equivalent. This will be addressed via a separate mechanism (Rust crates or dynamic libraries) in a future story.

## YAML Syntax

```yaml
name: workflow-with-imports

imports:
  # Local Lua file (relative to YAML file)
  - path: ./actions/custom.lua
    namespace: custom

  # Built-in action set
  - builtin: web
    namespace: http

  # Built-in without namespace (registers at root)
  - builtin: memory

state_schema:
  input: str
  output: str

nodes:
  - name: fetch
    uses: http.fetch          # From builtin: web with namespace: http
    with:
      url: "{{ state.url }}"

  - name: process
    uses: custom.transform    # From path: ./actions/custom.lua
    with:
      data: "{{ state.response }}"

  - name: remember
    uses: memory.store        # From builtin: memory (no namespace)
    with:
      key: result
      value: "{{ state.output }}"
```

## Acceptance Criteria

### Local Lua File Imports

- [ ] **AC-1**: `path: ./relative/path.lua` loads Lua file relative to YAML location
- [ ] **AC-2**: `path: /absolute/path.lua` loads Lua file from absolute path
- [ ] **AC-3**: Lua module must export `register_actions(registry)` function
- [ ] **AC-4**: Actions registered in Lua are available with namespace prefix
- [ ] **AC-5**: Error if file not found with clear message including resolved path
- [ ] **AC-6**: Error if Lua file doesn't have `register_actions` function
- [ ] **AC-7**: Circular imports are detected and skipped (with debug log)

### Built-in Action Sets

- [ ] **AC-8**: `builtin: web` registers web actions (fetch, post, etc.)
- [ ] **AC-9**: `builtin: memory` registers memory actions (store, recall, etc.)
- [ ] **AC-10**: `builtin: llm` registers LLM actions (if implemented)
- [ ] **AC-11**: Unknown builtin name produces clear error message
- [ ] **AC-12**: Built-in actions respect namespace prefix

### Namespace Handling

- [ ] **AC-13**: `namespace: foo` prefixes all actions as `foo.action_name`
- [ ] **AC-14**: Empty/missing namespace registers actions at root level
- [ ] **AC-15**: Namespace conflicts produce warning (last import wins)

### Integration

- [ ] **AC-16**: Imports are processed before node execution starts
- [ ] **AC-17**: Imported actions are available in `uses:` field
- [ ] **AC-18**: Imported actions work with `with:` parameters
- [ ] **AC-19**: CLI `tea run` processes imports automatically

### Error Handling

- [ ] **AC-20**: All import errors are collected and reported together
- [ ] **AC-21**: Import errors include source location (path/builtin name)
- [ ] **AC-22**: Workflow fails fast if any import fails

## Technical Design

### Lua Module Contract

External Lua files must follow this contract:

```lua
-- actions/custom.lua

-- Required: register_actions function
function register_actions(registry)
    -- Register an action
    registry:register("transform", function(state, params)
        local data = params.data or state.input
        return {
            output = string.upper(data),
            transformed = true
        }
    end)

    -- Register another action
    registry:register("validate", function(state, params)
        local value = params.value
        return {
            is_valid = value ~= nil and value ~= "",
            validated_at = os.time()
        }
    end)
end

-- Optional: metadata for logging
_G.__tea_actions__ = {
    version = "1.0.0",
    description = "Custom transformation actions",
    actions = {"transform", "validate"}
}
```

### Rust Implementation

#### Import Processor

```rust
// rust/src/engine/imports.rs (new file)

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use mlua::{Lua, Function, Table};

use crate::engine::executor::ActionRegistry;
use crate::engine::yaml::ImportConfig;
use crate::{TeaError, TeaResult};

pub struct ImportProcessor {
    loaded_paths: HashSet<PathBuf>,
    loaded_builtins: HashSet<String>,
}

impl ImportProcessor {
    pub fn new() -> Self {
        Self {
            loaded_paths: HashSet::new(),
            loaded_builtins: HashSet::new(),
        }
    }

    /// Process all imports from YAML config
    pub fn process_imports(
        &mut self,
        imports: &[ImportConfig],
        yaml_dir: &Path,
        registry: &ActionRegistry,
        lua: &Lua,
    ) -> TeaResult<()> {
        let mut errors = Vec::new();

        for import in imports {
            let result = if let Some(path) = &import.path {
                self.load_lua_file(path, &import.namespace, yaml_dir, registry, lua)
            } else if let Some(builtin) = &import.builtin {
                self.load_builtin(builtin, &import.namespace, registry)
            } else {
                Err(TeaError::Yaml(
                    "Import must specify 'path' or 'builtin'".into()
                ))
            };

            if let Err(e) = result {
                errors.push(format!("{}: {}",
                    import.path.as_ref().or(import.builtin.as_ref()).unwrap_or(&"unknown".into()),
                    e
                ));
            }
        }

        if !errors.is_empty() {
            return Err(TeaError::Import(
                format!("Failed to load imports:\n{}", errors.join("\n"))
            ));
        }

        Ok(())
    }

    /// Load actions from a Lua file
    fn load_lua_file(
        &mut self,
        path: &str,
        namespace: &str,
        yaml_dir: &Path,
        registry: &ActionRegistry,
        lua: &Lua,
    ) -> TeaResult<()> {
        // Resolve path relative to YAML file
        let full_path = if Path::new(path).is_absolute() {
            PathBuf::from(path)
        } else {
            yaml_dir.join(path).canonicalize()
                .map_err(|e| TeaError::Import(format!("Cannot resolve path {}: {}", path, e)))?
        };

        // Check for circular imports
        if self.loaded_paths.contains(&full_path) {
            tracing::debug!("Skipping already loaded: {:?}", full_path);
            return Ok(());
        }

        // Read and execute Lua file
        let lua_code = std::fs::read_to_string(&full_path)
            .map_err(|e| TeaError::Import(format!("Cannot read {:?}: {}", full_path, e)))?;

        lua.load(&lua_code).exec()
            .map_err(|e| TeaError::Import(format!("Lua error in {:?}: {}", full_path, e)))?;

        // Get register_actions function
        let register_fn: Function = lua.globals().get("register_actions")
            .map_err(|_| TeaError::Import(
                format!("{:?} must define register_actions(registry) function", full_path)
            ))?;

        // Create registry proxy table for Lua
        let lua_registry = self.create_lua_registry(lua, namespace, registry)?;

        // Call register_actions(registry)
        register_fn.call::<_, ()>(lua_registry)
            .map_err(|e| TeaError::Import(format!("register_actions failed: {}", e)))?;

        // Log metadata if present
        self.log_metadata(lua, path, namespace);

        self.loaded_paths.insert(full_path);
        Ok(())
    }

    /// Load a built-in action set
    fn load_builtin(
        &mut self,
        name: &str,
        namespace: &str,
        registry: &ActionRegistry,
    ) -> TeaResult<()> {
        // Check for duplicate
        let key = format!("{}:{}", name, namespace);
        if self.loaded_builtins.contains(&key) {
            tracing::debug!("Skipping already loaded builtin: {}", name);
            return Ok(());
        }

        match name {
            "web" => crate::actions::web::register(registry, namespace),
            "memory" => crate::actions::memory::register(registry, namespace),
            "llm" => crate::actions::llm::register(registry, namespace),
            "json" => crate::actions::json::register(registry, namespace),
            _ => return Err(TeaError::Import(
                format!("Unknown builtin: '{}'. Available: web, memory, llm, json", name)
            )),
        }

        self.loaded_builtins.insert(key);
        tracing::info!("Loaded builtin '{}' with namespace '{}'", name, namespace);
        Ok(())
    }

    /// Create a Lua table that proxies registry.register() calls
    fn create_lua_registry(
        &self,
        lua: &Lua,
        namespace: &str,
        registry: &ActionRegistry,
    ) -> TeaResult<Table> {
        let ns = namespace.to_string();
        let reg = registry.clone(); // ActionRegistry should be Arc-wrapped

        let table = lua.create_table()?;

        table.set("register", lua.create_function(move |_, (name, func): (String, Function)| {
            let full_name = if ns.is_empty() {
                name
            } else {
                format!("{}.{}", ns, name)
            };

            // Store Lua function reference in registry
            reg.register_lua(&full_name, func);
            Ok(())
        })?)?;

        Ok(table)
    }

    fn log_metadata(&self, lua: &Lua, source: &str, namespace: &str) {
        if let Ok(metadata) = lua.globals().get::<_, Table>("__tea_actions__") {
            let version: String = metadata.get("version").unwrap_or_default();
            let description: String = metadata.get("description").unwrap_or_default();
            tracing::info!(
                "Loaded {} (namespace: {}, version: {}): {}",
                source,
                if namespace.is_empty() { "root" } else { namespace },
                version,
                description
            );
        }
    }
}
```

#### Built-in Action Modules

```rust
// rust/src/actions/web.rs
pub fn register(registry: &ActionRegistry, namespace: &str) {
    let prefix = if namespace.is_empty() {
        String::new()
    } else {
        format!("{}.", namespace)
    };

    registry.register(
        &format!("{}fetch", prefix),
        |state, params| {
            // HTTP GET implementation
            todo!()
        }
    );

    registry.register(
        &format!("{}post", prefix),
        |state, params| {
            // HTTP POST implementation
            todo!()
        }
    );
}
```

#### Integration in YamlEngine

```rust
// rust/src/engine/yaml.rs

impl YamlEngine {
    pub fn load_from_file(&mut self, path: &Path) -> TeaResult<StateGraph> {
        let content = std::fs::read_to_string(path)?;
        let yaml_dir = path.parent().unwrap_or(Path::new("."));

        self.load_from_string_with_base(&content, yaml_dir)
    }

    pub fn load_from_string_with_base(&mut self, yaml: &str, base_dir: &Path) -> TeaResult<StateGraph> {
        let config: YamlConfig = serde_yaml::from_str(yaml)?;

        // Process imports FIRST
        if !config.imports.is_empty() {
            let mut processor = ImportProcessor::new();
            processor.process_imports(
                &config.imports,
                base_dir,
                &self.registry,
                &self.lua,
            )?;
        }

        // Then build graph...
        self.build_graph(&config)
    }
}
```

## Test Cases

### Unit Tests

```rust
#[test]
fn test_import_lua_file() {
    let yaml = r#"
name: import-test
imports:
  - path: ./fixtures/custom_actions.lua
    namespace: custom
nodes:
  - name: test
    uses: custom.greet
    with:
      name: "World"
edges:
  - from: __start__
    to: test
  - from: test
    to: __end__
"#;
    let engine = YamlEngine::new();
    let graph = engine.load_from_string_with_base(yaml, Path::new("tests")).unwrap();
    let result = execute(graph, json!({})).unwrap();
    assert_eq!(result["greeting"], "Hello, World!");
}

#[test]
fn test_import_builtin() {
    let yaml = r#"
name: builtin-test
imports:
  - builtin: json
    namespace: j
nodes:
  - name: parse
    uses: j.parse
    with:
      data: '{"key": "value"}'
"#;
    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    // Should not error - action is registered
}

#[test]
fn test_import_file_not_found() {
    let yaml = r#"
name: error-test
imports:
  - path: ./nonexistent.lua
"#;
    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Cannot read"));
}

#[test]
fn test_import_unknown_builtin() {
    let yaml = r#"
name: error-test
imports:
  - builtin: unknown_builtin
"#;
    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Unknown builtin"));
}

#[test]
fn test_import_circular_detection() {
    // Create two Lua files that import each other
    // Verify second import is skipped without error
}

#[test]
fn test_import_namespace_prefix() {
    // Verify actions are prefixed correctly
}
```

### Integration Tests

```rust
#[test]
fn test_import_parity_with_python() {
    // Same YAML file should work in both runtimes
    // (excluding package: imports which are Python-only)
}
```

## Out of Scope

- **`package:` imports** - Rust doesn't have pip packages; requires separate mechanism (crates or FFI)
- **Hot reloading** - Imports are processed once at load time
- **Import versioning** - No version constraints on imports
- **Remote imports** - No HTTP/git URL imports

## Dependencies

- **mlua** - Already used for Lua scripting
- Built-in action modules must exist (web, memory, llm, json)

## Related Stories

- **TEA-CLI-002**: Python CLI actions loading (reference implementation)
- **YE.6**: Python external action imports (reference implementation)
- **TEA-CLI-004**: CLI parity (this story is a dependency)

## Tasks / Subtasks

- [ ] **Task 1**: Create `rust/src/engine/imports.rs` module (AC-1 to AC-7)
  - [ ] Implement `ImportProcessor` struct
  - [ ] Implement `load_lua_file()` with path resolution
  - [ ] Implement circular import detection
  - [ ] Create Lua registry proxy table

- [ ] **Task 2**: Implement built-in action sets (AC-8 to AC-12)
  - [ ] Refactor `actions/mod.rs` to support namespaced registration
  - [ ] Add `register(registry, namespace)` to each action module
  - [ ] Map builtin names to registration functions

- [ ] **Task 3**: Implement namespace handling (AC-13 to AC-15)
  - [ ] Prefix actions with namespace
  - [ ] Handle empty namespace (root level)
  - [ ] Warn on namespace conflicts

- [ ] **Task 4**: Integrate with YamlEngine (AC-16 to AC-19)
  - [ ] Process imports before graph building
  - [ ] Pass yaml_dir to import processor
  - [ ] Update CLI to use new load path

- [ ] **Task 5**: Error handling (AC-20 to AC-22)
  - [ ] Collect all import errors
  - [ ] Include source location in errors
  - [ ] Fail fast on any error

- [ ] **Task 6**: Testing
  - [ ] Create test fixtures (Lua files)
  - [ ] Unit tests for each import type
  - [ ] Integration tests with full workflows
  - [ ] Parity tests with Python (path imports only)

## Dev Notes

### Test Fixtures

Create `rust/tests/fixtures/custom_actions.lua`:

```lua
function register_actions(registry)
    registry:register("greet", function(state, params)
        local name = params.name or "World"
        return { greeting = "Hello, " .. name .. "!" }
    end)
end

__tea_actions__ = {
    version = "1.0.0",
    description = "Test actions for import testing"
}
```

### ActionRegistry Changes

The `ActionRegistry` needs to support Lua function storage:

```rust
impl ActionRegistry {
    pub fn register_lua(&self, name: &str, func: mlua::Function) {
        // Store Lua function for later execution
    }
}
```

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Unit | `rust/tests/test_imports.rs` | Import processor logic |
| Integration | `rust/tests/test_yaml_imports.rs` | Full workflow with imports |
| Fixtures | `rust/tests/fixtures/*.lua` | Test Lua modules |

## Changelog

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2025-12-21 | 1.0 | Initial story creation | Sarah (PO Agent) |
