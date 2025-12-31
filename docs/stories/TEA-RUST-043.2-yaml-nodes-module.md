# Story TEA-RUST-043.2: Extract Node Factory Module

## Status
Ready for Dev

> **SM Validation**: ✅ PASS (story-draft-checklist) - 2025-12-27

**Parallel to**: TEA-PY-008.2 (Python equivalent)

## Story

**As a** developer maintaining the Rust YamlEngine codebase,
**I want** node creation logic extracted into a dedicated `yaml_nodes.rs` module,
**so that** the ~130 lines of node factory code is isolated, testable, and maintainable.

## Context

### Existing System Integration

- **Integrates with**: `yaml.rs`, `graph.rs`, `lua_runtime.rs`, `prolog_runtime.rs`
- **Technology**: Rust 1.75+, mlua (Lua), janus-swi (Prolog)
- **Follows pattern**: Factory struct with engine reference
- **Touch points**:
  - `build_graph()` - calls `build_node()` for each node config
  - `graph.rs` Node struct - node creation target
  - Runtime modules for inline code execution

### Methods to Extract (Lines 668-800, ~130 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `build_node()` | 668-732 | Main node factory dispatch |
| `build_while_loop_node()` | 734-800 | While-loop node construction |

### Related Logic to Extract

- Language detection for inline code (Lua vs Prolog)
- Node metadata handling
- Retry config attachment
- Fallback node configuration

## Acceptance Criteria

### Module Creation
1. New file `rust/src/engine/yaml_nodes.rs` created
2. Module has comprehensive rustdoc documentation
3. Module under 250 lines total

### NodeFactory Struct
4. `NodeFactory` struct created
5. Constructor accepts references to template processor and action registry
6. `build_node(&self, config: &NodeConfig) -> TeaResult<Node>` implemented
7. `build_while_loop_node(&self, config: &NodeConfig) -> TeaResult<Node>` implemented

### Node Types Supported
8. Standard nodes with `run:` inline code work
9. Action nodes with `uses:` work
10. While-loop nodes with `type: while_loop` work
11. Node metadata preserved
12. Retry configuration attached correctly
13. Fallback node references preserved

### Language Detection
14. Lua code detection works (default for inline)
15. Prolog code detection works (`language: prolog` or patterns)
16. Explicit `language:` field overrides detection
17. Error on unsupported language

### While-Loop Validation (TEA-RUST-033)
18. `max_iterations` required for while_loop type
19. `condition` required for while_loop type
20. `body` node list required for while_loop type
21. Descriptive error messages on validation failure

### YamlEngine Integration
22. `YamlEngine` creates and owns `NodeFactory`
23. `YamlEngine::build_node()` delegates to factory
24. Factory receives template processor for param processing

### Backward Compatibility
25. All existing tests pass without modification
26. No public API changes to YamlEngine
27. Node behavior identical before/after extraction

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_nodes.rs module** (AC: 1-3)
  - [ ] Create `rust/src/engine/yaml_nodes.rs`
  - [ ] Add module documentation with examples
  - [ ] Add required imports (graph::Node, serde_json)
  - [ ] Add module to `mod.rs`

- [ ] **Task 2: Implement NodeFactory struct** (AC: 4-7)
  - [ ] Create `NodeFactory` struct
  - [ ] Add constructor with template processor reference
  - [ ] Move `build_node()` logic
  - [ ] Move `build_while_loop_node()` logic

- [ ] **Task 3: Support all node types** (AC: 8-13)
  - [ ] Test standard run nodes
  - [ ] Test action nodes with `uses:`
  - [ ] Test while-loop nodes
  - [ ] Verify metadata preserved
  - [ ] Verify retry config attached
  - [ ] Verify fallback references work

- [ ] **Task 4: Implement language detection** (AC: 14-17)
  - [ ] Detect Lua patterns (default)
  - [ ] Detect Prolog patterns (`:- `, `?-`)
  - [ ] Honor explicit `language:` field
  - [ ] Return error for unsupported languages

- [ ] **Task 5: Preserve while-loop validation** (AC: 18-21)
  - [ ] Validate max_iterations present
  - [ ] Validate condition present
  - [ ] Validate body present
  - [ ] Match existing error messages

- [ ] **Task 6: Update YamlEngine integration** (AC: 22-24)
  - [ ] Import NodeFactory in yaml.rs
  - [ ] Create `node_factory` field in YamlEngine
  - [ ] Update `build_node()` to delegate

- [ ] **Task 7: Verify backward compatibility** (AC: 25-27)
  - [ ] Run node tests: `cargo test build_node`
  - [ ] Run while-loop tests: `cargo test while_loop`
  - [ ] Run full test suite: `cargo test`

## Dev Notes

### NodeFactory Pattern

```rust
// yaml_nodes.rs
//! Node factory for building StateGraph nodes from YAML configuration.

use serde_json::Value as JsonValue;
use std::collections::HashMap;

use crate::engine::graph::{ActionConfig, Node, RetryConfig};
use crate::engine::yaml_config::NodeConfig;
use crate::engine::yaml_templates::TemplateProcessor;
use crate::error::{TeaError, TeaResult};

/// Factory for creating Node instances from YAML configuration.
pub struct NodeFactory<'a> {
    /// Reference to template processor for parameter rendering
    template_processor: &'a TemplateProcessor,
}

impl<'a> NodeFactory<'a> {
    /// Create a new node factory.
    pub fn new(template_processor: &'a TemplateProcessor) -> Self {
        Self { template_processor }
    }

    /// Build a node from YAML configuration.
    pub fn build_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        // Check for while_loop type
        if let Some(ref node_type) = config.node_type {
            if node_type == "while_loop" {
                return self.build_while_loop_node(config);
            }
        }

        // Build standard node
        let mut node = Node::new(&config.name);

        // Add action if specified
        if let Some(ref action_name) = config.uses.as_ref().or(config.action.as_ref()) {
            let action_config = ActionConfig {
                action: action_name.clone(),
                params: config.with_params.clone().unwrap_or_default(),
                output_key: config.output.clone(),
            };
            node = node.with_action(action_config);
        }

        // Add inline code if specified
        if let Some(ref code) = config.run {
            let language = self.detect_language(config, code);
            node = node.with_inline_code(code.clone(), language);
        }

        // Add retry config if specified
        if let Some(ref retry) = config.retry {
            node = node.with_retry(retry.clone());
        }

        // Add fallback if specified
        if let Some(ref fallback) = config.fallback {
            node = node.with_fallback(fallback.clone());
        }

        // Add metadata
        if !config.metadata.is_empty() {
            node = node.with_metadata(config.metadata.clone());
        }

        Ok(node)
    }

    /// Build a while-loop node (TEA-RUST-033).
    pub fn build_while_loop_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        // Validate required fields
        let max_iterations = config.max_iterations.ok_or_else(|| {
            TeaError::Validation(format!(
                "while_loop node '{}' requires max_iterations",
                config.name
            ))
        })?;

        let condition = config.condition.as_ref().ok_or_else(|| {
            TeaError::Validation(format!(
                "while_loop node '{}' requires condition",
                config.name
            ))
        })?;

        let body = config.body.as_ref().ok_or_else(|| {
            TeaError::Validation(format!(
                "while_loop node '{}' requires body",
                config.name
            ))
        })?;

        // Build body nodes
        let body_nodes: Vec<Node> = body
            .iter()
            .map(|node_config| self.build_node(node_config))
            .collect::<TeaResult<Vec<_>>>()?;

        // Create while-loop node
        let node = Node::new_while_loop(
            &config.name,
            condition.clone(),
            max_iterations,
            body_nodes,
        );

        Ok(node)
    }

    /// Detect language for inline code.
    fn detect_language(&self, config: &NodeConfig, code: &str) -> String {
        // Explicit language takes precedence
        if let Some(ref lang) = config.language {
            return lang.clone();
        }

        // Detect Prolog patterns
        if code.contains(":-") || code.starts_with("?-") {
            return "prolog".to_string();
        }

        // Default to Lua
        "lua".to_string()
    }
}
```

### Integration in YamlEngine

```rust
// yaml.rs
mod yaml_nodes;
use yaml_nodes::NodeFactory;

impl YamlEngine {
    fn build_graph(&self, config: YamlConfig) -> TeaResult<StateGraph> {
        let node_factory = NodeFactory::new(&self.template_processor);

        let mut graph = StateGraph::new();

        for node_config in &config.nodes {
            let node = node_factory.build_node(node_config)?;
            graph.add_node(node);
        }

        // ... edge processing
    }
}
```

### Testing Requirements

- **Test file locations**: `rust/src/engine/yaml.rs` (inline tests), `rust/tests/`
- **Key test functions**: `test_parse_yaml_with_action`, `test_while_loop_*`
- **Run command**: `cd rust && cargo test build_node while_loop`

### Language Detection Patterns

| Pattern | Language |
|---------|----------|
| Contains `:-` | Prolog |
| Starts with `?-` | Prolog |
| `language: prolog` | Prolog |
| Default | Lua |

## Definition of Done

- [ ] yaml_nodes.rs created with NodeFactory struct
- [ ] All node building methods moved and working
- [ ] YamlEngine delegates to NodeFactory
- [ ] While-loop validation preserved
- [ ] All tests pass without modification
- [ ] No clippy warnings
- [ ] Module under 250 lines

## Risk Assessment

- **Primary Risk**: Breaking while-loop validation messages
- **Mitigation**: Match exact error message format from existing code

- **Secondary Risk**: Language detection edge cases
- **Mitigation**: Preserve exact detection logic, test with fixture files

- **Rollback**: Git revert single commit

## Dependency

- **Depends on**: TEA-RUST-043.1 (TemplateProcessor for param processing)
- **Depends on**: TEA-RUST-043.4 (NodeConfig struct definition)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation from TEA-PY-008.2 | Sarah (PO) |
