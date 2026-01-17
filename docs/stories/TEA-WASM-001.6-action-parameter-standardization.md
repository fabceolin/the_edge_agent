# Story TEA-WASM-001.6: Action Parameter Standardization

## Status
Ready for Review

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm action parameters to match the Python/Rust engine conventions,
**so that** my YAML workflows are portable across all TEA runtimes.

## Acceptance Criteria

1. `with:` block parameters processed with template rendering
2. `output:` field stores action result at specified state path
3. Nested output paths work (`output: result.data.items`)
4. Missing required parameters produce clear error messages
5. Optional parameters use documented defaults
6. Action results can be accessed via `{{ result.field }}` in subsequent templates

## Tasks / Subtasks

- [x] Standardize parameter extraction (AC: 1)
  - [x] Create `extract_params(node: &WasmNodeConfig, state: &JsonValue) -> HashMap<String, JsonValue>`
  - [x] Process each param value through template engine
  - [x] Handle nested param structures

- [x] Implement output path storage (AC: 2, 3)
  - [x] Create `set_at_path(state: &mut JsonValue, path: &str, value: JsonValue)`
  - [x] Parse dot-notation paths (`result.data.items`)
  - [x] Create intermediate objects as needed
  - [ ] ~~Handle array index notation (optional)~~ (deferred)

- [x] Implement required parameter validation (AC: 4)
  - [x] Define required params per action
  - [x] Validate before execution
  - [x] Error message includes action name and param name

- [x] Document and implement defaults (AC: 5)
  - [x] Document default values per action
  - [x] Apply defaults for missing optional params
  - [ ] ~~Log when default is used (debug level)~~ (deferred)

- [x] Implement result access (AC: 6)
  - [x] Store action result in temporary `result` context
  - [x] Make available for output template processing
  - [x] Clean up after node completion

- [x] Update all actions for consistency
  - [x] `llm.call` - prompt, temperature, max_tokens, model
  - [x] `llm.embed` - text, model
  - [x] `storage.*` - uri, content, etc.
  - [x] `ltm.*` - key, value, metadata, etc.
  - [x] `lua.eval` - code
  - [x] `prolog.query` - query, kb

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs
│   ├── executor.rs     # From Story 4
│   ├── templates.rs    # From Story 2
│   ├── actions/        # NEW: Split action implementations
│   │   ├── mod.rs
│   │   ├── llm.rs
│   │   ├── storage.rs
│   │   ├── ltm.rs
│   │   └── scripting.rs
│   └── params.rs       # NEW: Parameter handling utilities
└── Cargo.toml
```

### Parameter Extraction
```rust
pub fn extract_params(
    node: &WasmNodeConfig,
    state: &JsonValue,
    variables: &HashMap<String, JsonValue>,
) -> Result<HashMap<String, JsonValue>, ParamError> {
    let raw_params = node.with.clone().unwrap_or_default();
    let mut processed = HashMap::new();

    for (key, value) in raw_params {
        let processed_value = match value {
            JsonValue::String(s) => {
                // Template process string values
                render_template(&s, state, variables)?
            }
            other => other, // Pass through non-string values
        };
        processed.insert(key, processed_value);
    }

    Ok(processed)
}
```

### Output Path Storage
```rust
pub fn set_at_path(state: &mut JsonValue, path: &str, value: JsonValue) {
    let parts: Vec<&str> = path.split('.').collect();

    let mut current = state;
    for (i, part) in parts.iter().enumerate() {
        if i == parts.len() - 1 {
            // Last part - set value
            current[part] = value;
            return;
        }

        // Intermediate part - ensure object exists
        if !current[part].is_object() {
            current[part] = JsonValue::Object(serde_json::Map::new());
        }
        current = &mut current[part];
    }
}

// Usage
let mut state = json!({});
set_at_path(&mut state, "result.data.items", json!([1, 2, 3]));
// state = {"result": {"data": {"items": [1, 2, 3]}}}
```

### Action Parameter Definitions
```rust
pub struct ActionDef {
    pub name: &'static str,
    pub required: &'static [&'static str],
    pub optional: &'static [(&'static str, JsonValue)],
}

pub const LLM_CALL: ActionDef = ActionDef {
    name: "llm.call",
    required: &["prompt"],
    optional: &[
        ("temperature", JsonValue::Number(0.7.into())),
        ("max_tokens", JsonValue::Number(1000.into())),
        ("model", JsonValue::String("gpt-4".into())),
    ],
};

pub const LLM_EMBED: ActionDef = ActionDef {
    name: "llm.embed",
    required: &["text"],
    optional: &[
        ("model", JsonValue::String("text-embedding-3-small".into())),
    ],
};

pub const STORAGE_READ: ActionDef = ActionDef {
    name: "storage.read",
    required: &["uri"],
    optional: &[
        ("encoding", JsonValue::String("utf-8".into())),
    ],
};
```

### Validation Pattern
```rust
pub fn validate_params(
    action: &ActionDef,
    params: &HashMap<String, JsonValue>,
) -> Result<(), ParamError> {
    for required in action.required {
        if !params.contains_key(*required) {
            return Err(ParamError::MissingRequired {
                action: action.name.to_string(),
                param: required.to_string(),
            });
        }
    }
    Ok(())
}

pub fn apply_defaults(
    action: &ActionDef,
    params: &mut HashMap<String, JsonValue>,
) {
    for (name, default) in action.optional {
        if !params.contains_key(*name) {
            params.insert(name.to_string(), default.clone());
        }
    }
}
```

### Result Context Pattern
```rust
async fn execute_node_async(
    node: &WasmNodeConfig,
    mut state: JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    // Extract and validate params
    let mut params = extract_params(node, &state, &config.variables)?;
    let action_def = get_action_def(&node.action)?;
    validate_params(&action_def, &params)?;
    apply_defaults(&action_def, &mut params);

    // Execute action
    let result = execute_action(&node.action, &params, &state).await?;

    // Store result at output path if specified
    if let Some(output_path) = &node.output {
        // Process output path as template (for dynamic paths)
        let processed_path = render_template_string(output_path, &state, &config.variables)?;
        set_at_path(&mut state, &processed_path, result);
    }

    Ok(state)
}
```

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_params.rs`

### Test Cases
```rust
#[test]
fn test_template_param_processing() {
    let state = json!({"name": "Alice"});
    let node = WasmNodeConfig {
        with: Some(hashmap!{
            "greeting".to_string() => json!("Hello {{ state.name }}!")
        }),
        ..Default::default()
    };

    let params = extract_params(&node, &state, &HashMap::new()).unwrap();
    assert_eq!(params["greeting"], "Hello Alice!");
}

#[test]
fn test_output_path_simple() {
    let mut state = json!({});
    set_at_path(&mut state, "result", json!("value"));
    assert_eq!(state["result"], "value");
}

#[test]
fn test_output_path_nested() {
    let mut state = json!({});
    set_at_path(&mut state, "result.data.items", json!([1, 2, 3]));
    assert_eq!(state["result"]["data"]["items"], json!([1, 2, 3]));
}

#[test]
fn test_missing_required_param() {
    let params = HashMap::new();
    let result = validate_params(&LLM_CALL, &params);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("prompt"));
}

#[test]
fn test_default_applied() {
    let mut params = hashmap!{
        "prompt".to_string() => json!("test")
    };
    apply_defaults(&LLM_CALL, &mut params);
    assert_eq!(params["temperature"], 0.7);
    assert_eq!(params["max_tokens"], 1000);
}

#[wasm_bindgen_test]
async fn test_output_stores_result() {
    let yaml = r#"
        name: test
        nodes:
          - name: call
            action: return
            with:
              value: "hello"
            output: greeting.message
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();
    assert_eq!(state["greeting"]["message"], "hello");
}
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-17 | 0.2 | Implementation complete - parameter standardization module | James (Dev) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
None - implementation completed without blocking issues

### Completion Notes List
- Created `params.rs` module with comprehensive parameter handling
- Implemented `extract_params()` for template-processed parameter extraction
- Implemented `set_at_path()` for nested dot-notation output paths
- Implemented `get_at_path()` for reading nested values
- Implemented `validate_params()` for required parameter validation
- Implemented `apply_defaults()` for optional parameter defaults
- Created `ActionDef` struct for action parameter definitions
- Defined all standard actions: llm.call, llm.embed, llm.stream, storage.*, ltm.*, lua.eval, prolog.query, duckdb.*
- Created 17 unit tests for params module
- Created 16 integration tests covering all parameter scenarios
- Added `lazy_static` dependency for action definitions

### File List
| File | Action | Description |
|------|--------|-------------|
| `rust/tea-wasm-llm/src/params.rs` | Created | Parameter handling module |
| `rust/tea-wasm-llm/src/lib.rs` | Modified | Added params module and exports |
| `rust/tea-wasm-llm/Cargo.toml` | Modified | Added lazy_static dependency |
| `rust/tea-wasm-llm/tests/test_params.rs` | Created | Parameter integration tests |

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 14 |
| Unit Tests | 10 |
| Integration Tests | 3 |
| E2E Tests | 1 |
| P0 (Critical) | 5 |
| P1 (Important) | 6 |
| P2 (Edge cases) | 3 |

### Risk Assessment
| Risk ID | Score | Description |
|---------|-------|-------------|
| TECH-007 | 6 (High) | Parameter type coercion mismatch across runtimes |
| COMPAT-002 | 4 (Medium) | Action signature divergence from Python/Rust |

### Key Test Scenarios
- `1.6-UNIT-001`: `with:` params processed with template rendering (P0)
- `1.6-UNIT-003`: Nested output paths work (`output: result.data.items`) (P0)
- `1.6-UNIT-005`: Missing required params produce clear errors (P0)
- `1.6-UNIT-007`: Default values applied for optional params (P0)
- `1.6-INT-001`: Action params match Python/Rust conventions (P0, TECH-007)
- `1.6-INT-002`: Result accessible in subsequent templates (P1)

### Recommendations
1. Create cross-runtime param validation test suite
2. Document all action signatures with types in YAML reference
3. Add param type validation (not just presence check)

### Gate Status
**PASS** - Story is ready for implementation. Risk TECH-007 mitigated by cross-runtime testing recommendation.

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
