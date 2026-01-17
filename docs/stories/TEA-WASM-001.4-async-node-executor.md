# Story TEA-WASM-001.4: Async Node Executor

## Status
Draft

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm to execute nodes asynchronously,
**so that** long-running operations (LLM calls, storage) don't block the browser main thread.

## Acceptance Criteria

1. All node execution is async (`async fn execute_node()`)
2. State mutations persist correctly across async node transitions
3. Errors include node name, action, and error context
4. No blocking operations in WASM main thread
5. Execution can be cancelled/aborted (optional, stretch goal)
6. Progress callbacks for long-running executions (optional)

## Tasks / Subtasks

- [ ] Convert executor to async (AC: 1)
  - [ ] Make `execute_node()` async
  - [ ] Make `execute_yaml_workflow()` async
  - [ ] Update all action functions to be async-compatible
  - [ ] Use `wasm-bindgen-futures` for WASM async

- [ ] Implement async execution loop (AC: 1, 2)
  - [ ] Create main execution loop with await
  - [ ] Pass state between nodes correctly
  - [ ] Handle node result and state mutation

- [ ] Implement error context (AC: 3)
  - [ ] Wrap errors with node name
  - [ ] Include action name in error
  - [ ] Add stack-like context for nested errors

- [ ] Verify non-blocking behavior (AC: 4)
  - [ ] Test with slow LLM mock
  - [ ] Verify UI remains responsive during execution
  - [ ] Profile main thread blocking

- [ ] (Stretch) Implement cancellation (AC: 5)
  - [ ] Add `AbortController` pattern
  - [ ] Check cancellation between nodes
  - [ ] Clean up on cancellation

- [ ] (Stretch) Implement progress callbacks (AC: 6)
  - [ ] Add `on_node_start` callback
  - [ ] Add `on_node_complete` callback
  - [ ] Pass to executor via options

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs          # Main executor - convert to async
│   ├── config.rs       # From Story 1
│   ├── templates.rs    # From Story 2
│   ├── routing.rs      # From Story 3
│   └── executor.rs     # NEW: Async execution engine
└── Cargo.toml
```

### Current Synchronous Pattern
```rust
// Current (synchronous)
pub fn execute_yaml(yaml: &str, initial_state: &str) -> Result<String, JsValue> {
    let config = parse_config(yaml)?;
    let mut state = parse_state(initial_state)?;

    for node in &config.nodes {
        state = execute_node(node, state)?;
    }

    Ok(serde_json::to_string(&state)?)
}
```

### Target Async Pattern
```rust
use wasm_bindgen_futures::future_to_promise;

#[wasm_bindgen]
pub fn execute_yaml_workflow(yaml: &str, initial_state: &str) -> Promise {
    let yaml = yaml.to_string();
    let initial_state = initial_state.to_string();

    future_to_promise(async move {
        execute_yaml_async(&yaml, &initial_state)
            .await
            .map(|s| JsValue::from_str(&s))
            .map_err(|e| JsValue::from_str(&e.to_string()))
    })
}

async fn execute_yaml_async(yaml: &str, initial_state: &str) -> Result<String, WasmError> {
    let config = parse_yaml_config(yaml)?;
    let mut state: JsonValue = serde_json::from_str(initial_state)?;

    let mut current_node = find_start_node(&config);

    while let Some(node_name) = current_node {
        if node_name == "__end__" {
            break;
        }

        let node = config.nodes.iter()
            .find(|n| n.name == node_name)
            .ok_or_else(|| WasmError::NodeNotFound(node_name.clone()))?;

        // Execute node asynchronously
        state = execute_node_async(node, state, &config).await
            .map_err(|e| e.with_context(&node_name))?;

        // Resolve next node using routing (from Story 3)
        current_node = resolve_next_node(&node_name, &state, &config);
    }

    Ok(serde_json::to_string(&state)?)
}

async fn execute_node_async(
    node: &WasmNodeConfig,
    state: JsonValue,
    config: &WasmYamlConfig,
) -> Result<JsonValue, WasmError> {
    let action = node.action.as_deref().unwrap_or("passthrough");

    match action {
        "llm.call" => llm_call_async(node, state).await,
        "llm.embed" => llm_embed_async(node, state).await,
        "storage.read" => storage_read_async(node, state).await,
        "storage.write" => storage_write_async(node, state).await,
        "lua.eval" => lua_eval(node, state), // Lua is sync
        "prolog.query" => prolog_query(node, state), // Prolog is sync
        "return" => execute_return(node, state),
        _ => Ok(state), // passthrough
    }
}
```

### Error Context Pattern
```rust
#[derive(Debug, thiserror::Error)]
pub enum WasmError {
    #[error("Error in node '{node}': {source}")]
    NodeError {
        node: String,
        #[source]
        source: Box<WasmError>,
    },

    #[error("Action '{action}' failed: {message}")]
    ActionError {
        action: String,
        message: String,
    },

    // ... other variants
}

impl WasmError {
    pub fn with_context(self, node: &str) -> Self {
        WasmError::NodeError {
            node: node.to_string(),
            source: Box::new(self),
        }
    }
}
```

### WASM Async Notes
- Use `wasm-bindgen-futures` for Promise integration
- `future_to_promise()` converts Rust Future to JS Promise
- Browser's event loop handles scheduling
- No true concurrency - single-threaded async

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_executor.rs`

### Test Cases
```rust
#[wasm_bindgen_test]
async fn test_async_execution_completes() {
    let yaml = r#"
        name: test
        nodes:
          - name: step1
            action: return
            with:
              value: "hello"
            output: result
    "#;

    let result = execute_yaml_async(yaml, "{}").await;
    assert!(result.is_ok());

    let state: JsonValue = serde_json::from_str(&result.unwrap()).unwrap();
    assert_eq!(state["result"], "hello");
}

#[wasm_bindgen_test]
async fn test_error_includes_node_context() {
    let yaml = r#"
        name: test
        nodes:
          - name: failing_node
            action: llm.call
            with:
              prompt: "test"
    "#;

    let result = execute_yaml_async(yaml, "{}").await;
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("failing_node"));
}

#[wasm_bindgen_test]
async fn test_state_persists_across_nodes() {
    let yaml = r#"
        name: test
        nodes:
          - name: set_value
            action: return
            with:
              x: 1
            output: data
          - name: check_value
            action: return
            with:
              y: "{{ state.data.x + 1 }}"
            output: result
    "#;

    let result = execute_yaml_async(yaml, "{}").await.unwrap();
    let state: JsonValue = serde_json::from_str(&result).unwrap();
    assert_eq!(state["result"]["y"], 2);
}
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
_To be filled during implementation_

### Debug Log References
_To be filled during implementation_

### Completion Notes List
_To be filled during implementation_

### File List
_To be filled during implementation_

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 12 |
| Unit Tests | 8 |
| Integration Tests | 3 |
| E2E Tests | 1 |
| P0 (Critical) | 5 |
| P1 (Important) | 5 |
| P2 (Edge cases) | 2 |

### Risk Assessment
| Risk ID | Score | Description |
|---------|-------|-------------|
| TECH-002 | 6 (High) | Async state mutation race conditions |
| PERF-001 | 4 (Medium) | Sequential async overhead |

### Key Test Scenarios
- `1.4-UNIT-001`: Async execution completes successfully (P0)
- `1.4-UNIT-003`: State persists correctly across async node transitions (P0)
- `1.4-UNIT-005`: Errors include node name and action context (P0)
- `1.4-INT-001`: Multi-node async workflow executes in order (P0)
- `1.4-INT-003`: LLM mock async call completes without blocking (P0)

### Recommendations
1. Ensure state mutation is atomic within each node
2. Add timeout handling for long-running async operations
3. Consider adding cancellation support as stretch goal

### Gate Status
**PASS** - Story is ready for implementation. Risk TECH-002 mitigated by sequential execution model.

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
