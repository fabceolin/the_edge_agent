# Epic TEA-RUST-043: YAML Engine Modularization

## Status
Ready

> **SM Validation**: ✅ PASS (story-draft-checklist) - 2025-12-27
> - Parallel to Python TEA-PY-008 modularization epic
> - All 5 sub-stories validated and ready for development

## Epic Goal

Extract functionality from `yaml.rs` into focused modules, reducing the file from ~2,177 lines to <1,000 lines while maintaining full backward compatibility and Rust idiomatic patterns.

## Epic Description

### Background

The Rust YAML engine (`rust/src/engine/yaml.rs`) has grown to 2,177 lines containing multiple concerns:
- YAML configuration parsing and struct definitions
- Template processing (Tera templates)
- Node factory (building nodes from config)
- Edge factory (goto, implicit edges, conditional routing)
- Graph construction orchestration

This mirrors the Python codebase's growth, which was addressed in TEA-PY-008. The Rust implementation should follow the same modularization pattern, adapted to Rust idioms.

### Current State

| Metric | Target | Current State |
|--------|--------|---------------|
| `yaml.rs` lines | <1,000 | 2,177 |
| Total methods | ~25 | ~35 |
| Logical concerns | 1 (core engine) | 5+ |

### Existing System Context

- **Technology stack**: Rust 1.75+, Tera templates, mlua (Lua runtime), janus-swi (Prolog)
- **Integration points**: `executor.rs`, `graph.rs`, `lua_runtime.rs`, `prolog_runtime.rs`, `observability.rs`
- **Existing patterns**: Actions module structure in `actions/`

### Enhancement Details

- **What's being extracted**: Template processing, node builders, edge builders, configuration structs
- **How it integrates**: New modules under `engine/` following existing patterns
- **Success criteria**: yaml.rs under 1,000 lines, all tests pass, no API changes

## Stories

### Story 1: Extract Template Processing Module
**File**: `yaml_templates.rs` (~250 lines)
**Methods to extract** (lines 897-1103):
- `render_template()` - Tera template rendering with caching
- `process_params()` - Parameter processing with templates
- `process_value()` - Value processing (recursive)
- `eval_condition()` - Condition expression evaluation
- `get_state_bool()` - Boolean state access helper
- Template cache (`Arc<RwLock<HashMap>>`)

**Acceptance Criteria**:
1. New file `yaml_templates.rs` created with comprehensive documentation
2. `TemplateProcessor` struct created with engine reference pattern
3. Template caching preserved with `Arc<RwLock<HashMap>>`
4. All template-related tests pass unchanged

### Story 2: Extract Node Factory Module
**File**: `yaml_nodes.rs` (~200 lines)
**Methods to extract** (lines 668-800):
- `build_node()` - Main node factory dispatch
- `build_while_loop_node()` - While-loop node construction
- Lua/Prolog language detection (from config)

**Acceptance Criteria**:
1. New file `yaml_nodes.rs` created
2. `NodeFactory` struct receives YamlEngine reference
3. While-loop validation preserved
4. All node-related tests pass unchanged

### Story 3: Extract Edge Factory Module
**File**: `yaml_edges.rs` (~400 lines)
**Methods to extract** (lines 511-896):
- `process_goto_and_implicit_edges()` - TEA-YAML-002 implicit chaining
- `process_node_goto()` - Per-node goto processing
- `add_edge()` - Legacy edge configuration
- `infer_entry_finish()` - Entry/finish point inference

**Acceptance Criteria**:
1. New file `yaml_edges.rs` created
2. `EdgeFactory` struct handles all edge/goto processing
3. Goto precedence rules preserved
4. All goto/edge tests pass unchanged

### Story 4: Extract Configuration Structs Module
**File**: `yaml_config.rs` (~300 lines)
**Structs to extract** (lines 20-268):
- `YamlConfig` - Main workflow configuration
- `ImportConfig` - Import section config
- `NodeConfig` - Node configuration
- `Goto`, `GotoRule` - Goto navigation types
- `EdgeConfig` - Edge configuration
- `ErrorPolicyConfig` - Error policy

**Acceptance Criteria**:
1. New file `yaml_config.rs` created
2. All config structs moved with derives preserved
3. Serde attributes preserved exactly
4. All parsing tests pass unchanged

### Story 5: Extract Engine Builder Module
**File**: `yaml_builder.rs` (~150 lines)
**Methods to extract**:
- `build_graph()` - Graph construction orchestration
- Config accessors (checkpoint_dir, secrets, observability_config)
- Engine initialization helpers

**Acceptance Criteria**:
1. New file `yaml_builder.rs` created
2. `GraphBuilder` struct orchestrates graph construction
3. Uses NodeFactory, EdgeFactory, TemplateProcessor
4. All integration tests pass unchanged

## Compatibility Requirements

- [ ] Existing `YamlEngine::new()` constructor works identically
- [ ] All public methods and properties unchanged
- [ ] `pub use` exports preserve API
- [ ] YAML configurations work unchanged
- [ ] Performance impact minimal (no new allocations in hot paths)

## Risk Mitigation

- **Primary Risk**: Breaking Tera template caching with Arc/RwLock
- **Mitigation**: Preserve exact caching pattern, test concurrent access

- **Secondary Risk**: Circular dependencies between new modules
- **Mitigation**: Use trait objects or late binding for cross-module calls

- **Rollback Plan**: Git revert to pre-split state (single commit per story)

## Definition of Done

- [ ] All 5 stories completed with acceptance criteria met
- [ ] `yaml.rs` reduced to under 1,000 lines
- [ ] All Rust tests pass without modification
- [ ] No circular import issues
- [ ] Each new module well-documented with rustdoc
- [ ] All public exports preserved via `mod.rs`
- [ ] Documentation updated in source-tree.md

## Technical Notes

### Module Structure After Modularization

```
rust/src/engine/
├── yaml.rs              (~800 lines) - Core YamlEngine, public API
├── yaml_templates.rs    (~250 lines) - Tera template processing
├── yaml_nodes.rs        (~200 lines) - Node factory
├── yaml_edges.rs        (~400 lines) - Edge/goto processing
├── yaml_config.rs       (~300 lines) - Configuration structs
├── yaml_builder.rs      (~150 lines) - Graph construction
├── mod.rs                            - Module exports
├── graph.rs                          - StateGraph (existing)
├── executor.rs                       - Execution engine (existing)
└── ...
```

### Rust Idioms to Follow

```rust
// yaml_templates.rs
use std::sync::{Arc, RwLock};
use tera::{Context, Tera};

pub struct TemplateProcessor {
    tera: Arc<RwLock<Tera>>,
    cache: Arc<RwLock<HashMap<String, String>>>,
    variables: HashMap<String, JsonValue>,
    secrets: HashMap<String, JsonValue>,
}

impl TemplateProcessor {
    pub fn new(variables: HashMap<String, JsonValue>) -> Self {
        Self {
            tera: Arc::new(RwLock::new(Tera::default())),
            cache: Arc::new(RwLock::new(HashMap::new())),
            variables,
            secrets: HashMap::new(),
        }
    }

    pub fn render(&self, template: &str, state: &JsonValue) -> TeaResult<String> {
        // Check cache first
        // ...implementation
    }
}
```

```rust
// yaml.rs (after refactoring)
mod yaml_templates;
mod yaml_nodes;
mod yaml_edges;
mod yaml_config;
mod yaml_builder;

pub use yaml_config::*;
use yaml_templates::TemplateProcessor;
use yaml_nodes::NodeFactory;
use yaml_edges::EdgeFactory;
use yaml_builder::GraphBuilder;

pub struct YamlEngine {
    template_processor: TemplateProcessor,
    node_factory: NodeFactory,
    edge_factory: EdgeFactory,
    // ...
}
```

### Testing Strategy

- **No test modifications allowed** - Pure refactoring
- Run full suite: `cd rust && cargo test`
- Run YAML tests: `cargo test yaml`
- Verify public API: Check all `pub` exports work

## Story Manager Handoff

Please develop detailed user stories for this brownfield epic. Key considerations:

- This parallels Python TEA-PY-008 modularization pattern
- Integration points: `graph.rs`, `executor.rs`, runtime modules
- Existing patterns: Use `Arc<RwLock<>>` for shared mutable state
- Critical compatibility: No public API changes, all tests must pass
- Each story must include verification that existing functionality remains intact

The epic should maintain system integrity while reducing `yaml.rs` from 2,177 to under 1,000 lines.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial epic creation from TEA-PY-008 | Sarah (PO) |

## Dependencies

- **Parallel to**: TEA-PY-008 (Python equivalent)
- **Blocks**: None - Can be implemented incrementally

## Priority

Medium - Technical debt reduction, improves maintainability, mirrors Python structure
