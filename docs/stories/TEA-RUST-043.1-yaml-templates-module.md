# Story TEA-RUST-043.1: Extract Template Processing Module

## Status
Ready for Dev

> **SM Validation**: ✅ PASS (story-draft-checklist) - 2025-12-27

**Parallel to**: TEA-PY-008.1 (Python equivalent)

## Story

**As a** developer maintaining the Rust YamlEngine codebase,
**I want** template processing logic extracted into a dedicated `yaml_templates.rs` module,
**so that** the core engine focuses on orchestration while template logic is isolated and testable.

## Context

### Existing System Integration

- **Integrates with**: `yaml.rs` YamlEngine struct
- **Technology**: Rust 1.75+, Tera templates (Jinja2-compatible)
- **Follows pattern**: Rust module pattern with `Arc<RwLock<>>` for shared state
- **Touch points**:
  - `render_template()` - called from node run functions
  - `process_params()` - called from action execution
  - `eval_condition()` - called from edge conditions

### Methods to Extract (Lines 897-1103, ~210 lines)

| Method | Lines | Purpose |
|--------|-------|---------|
| `render_template()` | 897-967 | Tera template rendering with caching |
| `process_params()` | 969-983 | Parameter processing entry point |
| `process_value()` | 985-1053 | Recursive value processing |
| `eval_condition()` | 1055-1091 | Condition expression evaluation |
| `get_state_bool()` | 1093-1103 | Boolean state access helper |

### Additional State to Move

- Template cache: `Arc<RwLock<HashMap<String, String>>>`
- Tera instance: `Arc<RwLock<Tera>>`
- Variables and secrets references

## Acceptance Criteria

### Module Creation
1. New file `rust/src/engine/yaml_templates.rs` created
2. Module has comprehensive rustdoc documentation
3. Module under 300 lines total

### TemplateProcessor Struct
4. `TemplateProcessor` struct created with `Arc<RwLock<>>` pattern
5. Constructor accepts `variables` and `secrets` HashMaps
6. `render(&self, template: &str, state: &JsonValue) -> TeaResult<String>` implemented
7. `process_params(&self, params: &HashMap<String, JsonValue>, state: &JsonValue) -> TeaResult<HashMap<String, JsonValue>>` implemented
8. `eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool>` implemented

### Template Features Preserved
9. `{{ state.key }}` variable access works identically
10. `{{ variables.key }}` and `{{ secrets.key }}` access works
11. `{{ checkpoint.dir }}` and `{{ checkpoint.last }}` work (via context injection)
12. Tera filters (`| json_encode`, `| upper`, etc.) work
13. Template caching for performance preserved
14. Error messages with template context preserved

### Thread Safety
15. `Arc<RwLock<HashMap>>` for template cache
16. Safe concurrent access from multiple threads
17. No deadlock potential in cache operations

### YamlEngine Integration
18. `YamlEngine` creates and owns `TemplateProcessor`
19. `YamlEngine::render_template()` delegates to processor
20. `YamlEngine::process_params()` delegates to processor
21. `YamlEngine::eval_condition()` delegates to processor

### Backward Compatibility
22. All existing tests pass without modification
23. No public API changes to YamlEngine
24. Template behavior identical before/after extraction

### Code Quality
25. All public items have rustdoc comments
26. No `unwrap()` in non-test code (use `?` operator)
27. Clippy lints pass

## Tasks / Subtasks

- [ ] **Task 1: Create yaml_templates.rs module** (AC: 1-3)
  - [ ] Create `rust/src/engine/yaml_templates.rs`
  - [ ] Add module documentation with examples
  - [ ] Add required imports (tera, serde_json, std::sync)
  - [ ] Add module to `mod.rs`

- [ ] **Task 2: Implement TemplateProcessor struct** (AC: 4-8)
  - [ ] Create `TemplateProcessor` struct
  - [ ] Add `new(variables, secrets)` constructor
  - [ ] Move `render_template()` logic to `render()`
  - [ ] Move `process_params()` logic
  - [ ] Move `process_value()` as private method
  - [ ] Move `eval_condition()` logic
  - [ ] Move `get_state_bool()` as private helper

- [ ] **Task 3: Preserve template features** (AC: 9-14)
  - [ ] Test state/variables/secrets access
  - [ ] Test checkpoint context injection
  - [ ] Test Tera filters work
  - [ ] Verify template caching works
  - [ ] Verify error messages include context

- [ ] **Task 4: Implement thread safety** (AC: 15-17)
  - [ ] Use `Arc<RwLock<HashMap>>` for cache
  - [ ] Use `RwLock::read()` for cache lookups
  - [ ] Use `RwLock::write()` for cache insertions
  - [ ] Ensure no panic on lock contention

- [ ] **Task 5: Update YamlEngine integration** (AC: 18-21)
  - [ ] Import TemplateProcessor in yaml.rs
  - [ ] Create `template_processor` field in YamlEngine
  - [ ] Update `render_template()` to delegate
  - [ ] Update `process_params()` to delegate
  - [ ] Update `eval_condition()` to delegate

- [ ] **Task 6: Verify backward compatibility** (AC: 22-24)
  - [ ] Run full test suite: `cargo test`
  - [ ] Run template-specific tests: `cargo test render_template`
  - [ ] Run condition tests: `cargo test eval_condition`

- [ ] **Task 7: Code quality check** (AC: 25-27)
  - [ ] Add rustdoc to all public items
  - [ ] Replace any `unwrap()` with `?`
  - [ ] Run `cargo clippy`

## Dev Notes

### TemplateProcessor Pattern

```rust
// yaml_templates.rs
//! Template processing for YAML workflows using Tera.
//!
//! Provides Jinja2-compatible template syntax for variable substitution
//! in YAML configurations.

use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tera::{Context, Tera};

use crate::error::{TeaError, TeaResult};

/// Processes Tera templates with caching for performance.
pub struct TemplateProcessor {
    /// Tera instance for template compilation
    tera: Arc<RwLock<Tera>>,
    /// Cache for compiled templates
    cache: Arc<RwLock<HashMap<String, String>>>,
    /// Global variables from YAML
    variables: HashMap<String, JsonValue>,
    /// Secret values (not logged)
    secrets: HashMap<String, JsonValue>,
    /// Checkpoint directory for {{ checkpoint.dir }}
    checkpoint_dir: Option<String>,
    /// Last checkpoint path for {{ checkpoint.last }}
    last_checkpoint: Option<String>,
}

impl TemplateProcessor {
    /// Create a new template processor.
    pub fn new(variables: HashMap<String, JsonValue>) -> Self {
        Self {
            tera: Arc::new(RwLock::new(Tera::default())),
            cache: Arc::new(RwLock::new(HashMap::new())),
            variables,
            secrets: HashMap::new(),
            checkpoint_dir: None,
            last_checkpoint: None,
        }
    }

    /// Set secrets for template access.
    pub fn set_secrets(&mut self, secrets: HashMap<String, JsonValue>) {
        self.secrets = secrets;
    }

    /// Set checkpoint context for templates.
    pub fn set_checkpoint_context(&mut self, dir: Option<String>, last: Option<String>) {
        self.checkpoint_dir = dir;
        self.last_checkpoint = last;
    }

    /// Render a template string with state context.
    pub fn render(&self, template: &str, state: &JsonValue) -> TeaResult<String> {
        // Check cache first
        let cache_key = format!("{:?}:{}", state, template);
        if let Ok(cache) = self.cache.read() {
            if let Some(result) = cache.get(&cache_key) {
                return Ok(result.clone());
            }
        }

        // Build context
        let mut context = Context::new();
        context.insert("state", state);
        context.insert("variables", &self.variables);
        context.insert("secrets", &self.secrets);

        // Add checkpoint context
        let checkpoint = serde_json::json!({
            "dir": self.checkpoint_dir,
            "last": self.last_checkpoint,
        });
        context.insert("checkpoint", &checkpoint);

        // Render template
        let result = Tera::one_off(template, &context, false)
            .map_err(|e| TeaError::Template(e.to_string()))?;

        // Cache result
        if let Ok(mut cache) = self.cache.write() {
            cache.insert(cache_key, result.clone());
        }

        Ok(result)
    }

    /// Process parameters, rendering any template strings.
    pub fn process_params(
        &self,
        params: &HashMap<String, JsonValue>,
        state: &JsonValue,
    ) -> TeaResult<HashMap<String, JsonValue>> {
        let mut result = HashMap::new();
        for (key, value) in params {
            result.insert(key.clone(), self.process_value(value, state)?);
        }
        Ok(result)
    }

    /// Evaluate a condition expression.
    pub fn eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool> {
        // Implementation...
    }

    fn process_value(&self, value: &JsonValue, state: &JsonValue) -> TeaResult<JsonValue> {
        // Implementation...
    }
}
```

### Integration in YamlEngine

```rust
// yaml.rs
mod yaml_templates;
use yaml_templates::TemplateProcessor;

pub struct YamlEngine {
    template_processor: TemplateProcessor,
    // ... other fields
}

impl YamlEngine {
    pub fn new() -> Self {
        Self {
            template_processor: TemplateProcessor::new(HashMap::new()),
            // ...
        }
    }

    pub fn render_template(&self, template: &str, state: &JsonValue) -> TeaResult<String> {
        self.template_processor.render(template, state)
    }
}
```

### Testing Requirements

- **Test file locations**: `rust/src/engine/yaml.rs` (inline tests)
- **Key test functions**: `test_render_template`, `test_process_params`, `test_eval_condition_*`
- **Run command**: `cd rust && cargo test render_template eval_condition process_params`

### Cache Thread Safety

The cache uses `RwLock` for safe concurrent access:
- Multiple readers can access cache simultaneously
- Writers get exclusive access
- Use `read()` for lookups, `write()` for insertions
- Handle `PoisonError` gracefully (return computation result)

## Definition of Done

- [ ] yaml_templates.rs created with TemplateProcessor struct
- [ ] All template methods moved and working
- [ ] YamlEngine delegates to TemplateProcessor
- [ ] Thread-safe caching preserved
- [ ] All tests pass without modification
- [ ] No clippy warnings
- [ ] Module under 300 lines

## Risk Assessment

- **Primary Risk**: Cache deadlock with nested render calls
- **Mitigation**: Use `try_read()`/`try_write()` with fallback to recomputation

- **Secondary Risk**: Breaking Tera context format
- **Mitigation**: Preserve exact context structure, test all variable access patterns

- **Rollback**: Git revert single commit

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation from TEA-PY-008.1 | Sarah (PO) |
