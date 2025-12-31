# Story TEA-RUST-043.4: Extract Configuration Structs Module

## Status
Ready for Dev

> **SM Validation**: ✅ PASS (story-draft-checklist) - 2025-12-27

**Parallel to**: TEA-PY-008.4 and TEA-PY-008.5 (Python equivalents combined)

## Story

**As a** developer maintaining the Rust YamlEngine codebase,
**I want** YAML configuration structs extracted into a dedicated `yaml_config.rs` module,
**so that** the ~250 lines of struct definitions are isolated and reusable across modules.

## Context

### Existing System Integration

- **Integrates with**: `yaml.rs`, `graph.rs`, all yaml_* modules
- **Technology**: Rust 1.75+, serde, serde_yaml
- **Follows pattern**: Pure data structs with serde derives
- **Touch points**:
  - `YamlEngine::load_from_string()` - deserializes YamlConfig
  - `build_graph()` - uses all config structs
  - All factory modules use NodeConfig, EdgeConfig

### Structs to Extract (Lines 20-268, ~250 lines)

| Struct/Enum | Lines | Purpose |
|-------------|-------|---------|
| `YamlConfig` | 20-58 | Main workflow configuration |
| `ImportConfig` | 62-75 | Import section config |
| `NodeConfig` | 78-160 | Node configuration |
| `Goto` enum | 162-172 | Goto navigation type |
| `GotoRule` | 174-191 | Conditional goto rule |
| `EdgeConfig` | 193-214 | Edge configuration |
| `ErrorPolicyConfig` | 216-267 | Error policy settings |
| Default impls | 238-267 | Default value functions |

### Key Features to Preserve

1. **Serde derives**: All structs must preserve `Serialize, Deserialize`
2. **Serde attributes**: `#[serde(default)]`, `#[serde(rename)]`
3. **Default implementations**: For optional fields
4. **Documentation**: Inline rustdoc comments

## Acceptance Criteria

### Module Creation
1. New file `rust/src/engine/yaml_config.rs` created
2. Module has comprehensive rustdoc documentation
3. Module under 350 lines total

### Struct Definitions
4. `YamlConfig` struct moved with all fields
5. `ImportConfig` struct moved with all fields
6. `NodeConfig` struct moved with all fields and documentation
7. `Goto` enum moved with variants
8. `GotoRule` struct moved with all fields
9. `EdgeConfig` struct moved with all fields
10. `ErrorPolicyConfig` struct moved with defaults

### Serde Attributes Preserved
11. All `#[serde(default)]` attributes preserved
12. All `#[serde(rename = "...")]` attributes preserved
13. All `#[derive(Debug, Clone, Serialize, Deserialize)]` preserved
14. Optional fields use `Option<T>` pattern

### Default Implementations
15. `default_max_retries()` function moved
16. `default_backoff_base()` function moved
17. `default_backoff_max()` function moved
18. `default_jitter()` function moved
19. `default_on_failure()` function moved
20. `impl Default for ErrorPolicyConfig` moved

### Public Exports
21. All structs re-exported from `yaml.rs`
22. All enums re-exported from `yaml.rs`
23. Import paths work: `use crate::engine::yaml_config::*`

### Backward Compatibility
24. All existing tests pass without modification
25. YAML parsing behavior identical
26. No public API changes

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_config.rs module** (AC: 1-3)
  - [ ] Create `rust/src/engine/yaml_config.rs`
  - [ ] Add module documentation
  - [ ] Add required imports (serde, serde_json, HashMap)
  - [ ] Add module to `mod.rs`

- [ ] **Task 2: Move main config structs** (AC: 4-6)
  - [ ] Move `YamlConfig` struct with all fields
  - [ ] Move `ImportConfig` struct
  - [ ] Move `NodeConfig` struct with all documentation

- [ ] **Task 3: Move navigation types** (AC: 7-9)
  - [ ] Move `Goto` enum
  - [ ] Move `GotoRule` struct
  - [ ] Move `EdgeConfig` struct

- [ ] **Task 4: Move error policy** (AC: 10)
  - [ ] Move `ErrorPolicyConfig` struct
  - [ ] Move all default functions

- [ ] **Task 5: Preserve serde attributes** (AC: 11-14)
  - [ ] Verify all `#[serde(default)]` preserved
  - [ ] Verify all `#[serde(rename)]` preserved
  - [ ] Verify all derives preserved
  - [ ] Test optional field parsing

- [ ] **Task 6: Move default implementations** (AC: 15-20)
  - [ ] Move `default_max_retries()`
  - [ ] Move `default_backoff_base()`
  - [ ] Move `default_backoff_max()`
  - [ ] Move `default_jitter()`
  - [ ] Move `default_on_failure()`
  - [ ] Move `impl Default for ErrorPolicyConfig`

- [ ] **Task 7: Set up public exports** (AC: 21-23)
  - [ ] Add `pub use yaml_config::*;` to yaml.rs
  - [ ] Verify external import paths work
  - [ ] Update mod.rs if needed

- [ ] **Task 8: Verify backward compatibility** (AC: 24-26)
  - [ ] Run parsing tests: `cargo test parse`
  - [ ] Test YAML deserialization
  - [ ] Run full test suite: `cargo test`

## Dev Notes

### yaml_config.rs Structure

```rust
// yaml_config.rs
//! YAML configuration struct definitions.
//!
//! This module contains all the serde-deserializable structs
//! for parsing YAML workflow definitions.

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;

use crate::engine::graph::RetryConfig;
use crate::engine::observability::ObsConfig;

/// YAML configuration for a workflow.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct YamlConfig {
    /// Workflow name
    pub name: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,

    /// State schema (optional)
    #[serde(default)]
    pub state_schema: Option<HashMap<String, String>>,

    /// Initial state values (merged with CLI input, CLI takes precedence)
    #[serde(default)]
    pub initial_state: Option<JsonValue>,

    /// Global variables
    #[serde(default)]
    pub variables: HashMap<String, JsonValue>,

    /// External module imports
    #[serde(default)]
    pub imports: Vec<ImportConfig>,

    /// Node definitions
    pub nodes: Vec<NodeConfig>,

    /// Edge definitions
    #[serde(default)]
    pub edges: Vec<EdgeConfig>,

    /// Global error policy
    #[serde(default)]
    pub error_policy: Option<ErrorPolicyConfig>,

    /// Observability configuration (TEA-OBS-001.2)
    #[serde(default)]
    pub observability: ObsConfig,
}

/// Import configuration for external modules.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportConfig {
    /// Path to Lua module file
    #[serde(default)]
    pub path: Option<String>,

    /// Built-in action set name
    #[serde(default)]
    pub builtin: Option<String>,

    /// Namespace prefix for imported actions
    #[serde(default)]
    pub namespace: String,
}

/// Node configuration from YAML.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeConfig {
    /// Node name
    pub name: String,

    /// Node type: "standard" (default) or "while_loop" (TEA-RUST-033)
    #[serde(default, rename = "type")]
    pub node_type: Option<String>,

    /// Action to use (e.g., "llm.call")
    #[serde(default)]
    pub uses: Option<String>,

    /// Alias for 'uses'
    #[serde(default)]
    pub action: Option<String>,

    /// Action parameters
    #[serde(default, rename = "with")]
    pub with_params: Option<HashMap<String, JsonValue>>,

    /// Inline code (run directly) - Lua or Prolog
    #[serde(default)]
    pub run: Option<String>,

    /// Language for inline code: "lua" (default) or "prolog"
    #[serde(default)]
    pub language: Option<String>,

    /// Retry configuration
    #[serde(default)]
    pub retry: Option<RetryConfig>,

    /// Fallback node on failure
    #[serde(default)]
    pub fallback: Option<String>,

    /// Node metadata
    #[serde(default)]
    pub metadata: HashMap<String, JsonValue>,

    /// Output key for storing action result in state
    #[serde(default)]
    pub output: Option<String>,

    /// TEA-RUST-033: Maximum iterations for while_loop nodes
    #[serde(default)]
    pub max_iterations: Option<usize>,

    /// TEA-RUST-033: Condition expression for while_loop nodes
    #[serde(default)]
    pub condition: Option<String>,

    /// TEA-RUST-033: Body nodes for while_loop
    #[serde(default)]
    pub body: Option<Vec<NodeConfig>>,

    /// TEA-YAML-002: Navigation target after node execution
    #[serde(default)]
    pub goto: Option<Goto>,
}

/// Goto navigation specification.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Goto {
    /// Simple unconditional jump
    Simple(String),
    /// Conditional routing with rules
    Conditional(Vec<GotoRule>),
}

/// A single goto routing rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GotoRule {
    /// Condition expression (Jinja2 template)
    #[serde(rename = "if")]
    pub if_condition: Option<String>,

    /// Target node name
    pub to: String,
}

/// Edge configuration from YAML.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeConfig {
    /// Source node name
    pub from: String,

    /// Target node name
    pub to: String,

    /// Condition expression (for conditional edges)
    #[serde(default)]
    pub condition: Option<String>,

    /// Parallel execution targets
    #[serde(default)]
    pub parallel: Option<Vec<String>>,
}

/// Global error handling policy.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorPolicyConfig {
    /// Maximum retry attempts
    #[serde(default = "default_max_retries")]
    pub max_retries: u32,

    /// Base delay for exponential backoff (ms)
    #[serde(default = "default_backoff_base")]
    pub backoff_base: u64,

    /// Maximum delay for exponential backoff (ms)
    #[serde(default = "default_backoff_max")]
    pub backoff_max: u64,

    /// Whether to add jitter to backoff
    #[serde(default = "default_jitter")]
    pub jitter: bool,

    /// Action on final failure: "abort" or "continue"
    #[serde(default = "default_on_failure")]
    pub on_failure: String,
}

fn default_max_retries() -> u32 { 3 }
fn default_backoff_base() -> u64 { 1000 }
fn default_backoff_max() -> u64 { 30000 }
fn default_jitter() -> bool { true }
fn default_on_failure() -> String { "abort".to_string() }

impl Default for ErrorPolicyConfig {
    fn default() -> Self {
        Self {
            max_retries: default_max_retries(),
            backoff_base: default_backoff_base(),
            backoff_max: default_backoff_max(),
            jitter: default_jitter(),
            on_failure: default_on_failure(),
        }
    }
}
```

### Integration in yaml.rs

```rust
// yaml.rs
mod yaml_config;
pub use yaml_config::*;

// Structs are now accessible from yaml_config
```

### Testing Requirements

- **Test file locations**: `rust/src/engine/yaml.rs` (inline tests)
- **Key test functions**: `test_parse_simple_yaml`, `test_parse_yaml_with_*`
- **Run command**: `cd rust && cargo test parse`

### Serde Attribute Reference

| Attribute | Purpose |
|-----------|---------|
| `#[serde(default)]` | Use Default::default() for missing fields |
| `#[serde(rename = "x")]` | Use different name in YAML |
| `#[serde(default = "fn")]` | Use custom default function |
| `#[serde(untagged)]` | Enum without tag in YAML |

## Definition of Done

- [ ] yaml_config.rs created with all config structs
- [ ] All structs moved with serde derives preserved
- [ ] All default implementations moved
- [ ] Public re-exports set up in yaml.rs
- [ ] All tests pass without modification
- [ ] No clippy warnings
- [ ] Module under 350 lines

## Risk Assessment

- **Primary Risk**: Breaking serde deserialization
- **Mitigation**: Preserve exact attribute syntax, test all YAML fixtures

- **Secondary Risk**: Missing public exports
- **Mitigation**: Use `pub use yaml_config::*;` pattern

- **Rollback**: Git revert single commit

## Dependency

- **Blocks**: TEA-RUST-043.2, TEA-RUST-043.3 (they depend on config structs)
- **Recommended order**: Implement this story first

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation | Sarah (PO) |
