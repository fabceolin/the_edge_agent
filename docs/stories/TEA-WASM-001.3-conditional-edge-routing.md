# Story TEA-WASM-001.3: Conditional Edge Routing

## Status
Ready for Review

## Epic
[TEA-WASM-001: WASM YAML Engine Expansion](./TEA-WASM-001-yaml-engine-expansion.md)

## Story

**As a** browser-based agent developer,
**I want** tea-wasm-llm to support conditional edge routing with `goto` and `when` conditions,
**so that** I can create branching workflows that make decisions based on state.

## Acceptance Criteria

1. Parse `goto: nodeB` simple navigation syntax
2. Parse `goto: nodeB, when: state.score > 0.8` conditional syntax
3. Parse array-based conditional edges with multiple conditions
4. Evaluate `when` conditions using Tera expressions (from Story 2)
5. Support `__start__` and `__end__` special node names
6. Fall back to sequential execution when no explicit edge matches
7. First unmatched condition falls through to next or default
8. Circular edge detection with warning (non-blocking)

## Tasks / Subtasks

- [x] Extend edge config parsing (AC: 1, 2, 3)
  - [x] Parse simple `goto: nodeName` string
  - [x] Parse `goto` object with `to` and `when` fields
  - [x] Parse array of conditional gotos
  - [x] Handle inline `goto:` in node config

- [x] Define GotoConfig enum (AC: 1, 2, 3)
  - [x] `Simple(String)` - unconditional goto
  - [x] `Conditional(Vec<GotoBranch>)` - array of conditions
  - [x] GotoBranch with `to` and optional `condition`

- [x] Implement condition evaluation (AC: 4, 7)
  - [x] Create `evaluate_condition(condition: &str, state: &JsonValue) -> bool`
  - [x] Use Tera to evaluate condition as expression
  - [x] Handle truthy/falsy values

- [x] Implement edge resolution (AC: 5, 6, 7)
  - [x] Create `resolve_next_node(current: &str, state: &JsonValue, config: &WasmYamlConfig) -> Option<String>`
  - [x] Check node's `goto` first
  - [x] Check explicit edges from current node
  - [x] Fall back to next sequential node
  - [x] Return `None` for `__end__` or no next node

- [x] Handle special nodes (AC: 5)
  - [x] `__start__` - virtual entry point
  - [x] `__end__` - virtual exit point
  - [x] Find first node when `__start__` edge exists

- [x] Implement cycle detection (AC: 8)
  - [x] Track visited nodes during execution
  - [x] Warn on revisit (allow for loops, but warn)
  - [x] Hard limit on iterations (1000 default)

- [x] Update executor to use edge routing
  - [x] Created `build_execution_path()` function
  - [x] Call `resolve_next_node()` after each node
  - [x] Stop when `__end__` or `None` returned

## Dev Notes

### Relevant Source Tree
```
rust/tea-wasm-llm/
├── src/
│   ├── lib.rs          # Update executor logic
│   ├── config.rs       # GotoConfig enum (from Story 1)
│   ├── templates.rs    # Condition evaluation (from Story 2)
│   └── routing.rs      # NEW: Edge resolution logic
└── Cargo.toml
```

### YAML Syntax Examples

**Simple goto:**
```yaml
nodes:
  - name: step1
    action: llm.call
    goto: step2
```

**Conditional goto:**
```yaml
nodes:
  - name: classify
    action: llm.call
    output: classification
    goto:
      - to: handle_positive
        when: state.classification.sentiment == "positive"
      - to: handle_negative
        when: state.classification.sentiment == "negative"
      - to: handle_neutral  # default (no when)
```

**Edge-based routing:**
```yaml
edges:
  - from: __start__
    to: init
  - from: classify
    to: positive_handler
    when: state.result.score > 0.8
  - from: classify
    to: negative_handler
    when: state.result.score < 0.2
  - from: classify
    to: neutral_handler  # default
```

### GotoConfig Implementation
```rust
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum GotoConfig {
    Simple(String),
    Conditional(ConditionalGoto),
    Array(Vec<ConditionalGoto>),
}

#[derive(Debug, Clone, Deserialize)]
pub struct ConditionalGoto {
    pub to: String,
    #[serde(default)]
    pub when: Option<String>,
}
```

### Condition Evaluation
```rust
pub fn evaluate_condition(condition: &str, state: &JsonValue) -> Result<bool, ConditionError> {
    // Wrap condition in Tera if expression
    let template = format!("{{% if {} %}}true{{% else %}}false{{% endif %}}", condition);

    let result = render_template(&template, state, &HashMap::new())?;

    Ok(result == "true")
}
```

### Edge Resolution Algorithm
```rust
pub fn resolve_next_node(
    current: &str,
    state: &JsonValue,
    config: &WasmYamlConfig,
) -> Option<String> {
    // 1. Check node's inline goto
    if let Some(node) = config.nodes.iter().find(|n| n.name == current) {
        if let Some(goto) = &node.goto {
            if let Some(next) = evaluate_goto(goto, state) {
                return Some(next);
            }
        }
    }

    // 2. Check explicit edges from current node
    for edge in &config.edges {
        if edge.from == current {
            if let Some(when) = &edge.when {
                if evaluate_condition(when, state).unwrap_or(false) {
                    return Some(edge.to.clone());
                }
            } else {
                return Some(edge.to.clone()); // unconditional edge
            }
        }
    }

    // 3. Fall back to sequential (next node in array)
    let current_idx = config.nodes.iter().position(|n| n.name == current)?;
    config.nodes.get(current_idx + 1).map(|n| n.name.clone())
}
```

### Python Reference
See `python/src/the_edge_agent/yaml_edges.py` for the full implementation:
- `EdgeFactory._process_goto()` for goto parsing
- `EdgeFactory._add_condition_edge()` for conditional edges

## Testing

### Test Location
`rust/tea-wasm-llm/tests/test_routing.rs`

### Test Cases
```rust
#[test]
fn test_simple_goto() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: a
            goto: c
          - name: b
          - name: c
    "#);
    let next = resolve_next_node("a", &json!({}), &config);
    assert_eq!(next, Some("c".to_string()));
}

#[test]
fn test_conditional_goto_matches() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: check
            goto:
              - to: high
                when: state.score > 0.8
              - to: low
    "#);
    let next = resolve_next_node("check", &json!({"score": 0.9}), &config);
    assert_eq!(next, Some("high".to_string()));
}

#[test]
fn test_conditional_goto_fallback() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: check
            goto:
              - to: high
                when: state.score > 0.8
              - to: low
    "#);
    let next = resolve_next_node("check", &json!({"score": 0.5}), &config);
    assert_eq!(next, Some("low".to_string()));
}

#[test]
fn test_sequential_fallback() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: a
          - name: b
          - name: c
    "#);
    let next = resolve_next_node("a", &json!({}), &config);
    assert_eq!(next, Some("b".to_string()));
}

#[test]
fn test_end_node() {
    let config = parse_yaml(r#"
        name: test
        nodes:
          - name: a
            goto: __end__
    "#);
    let next = resolve_next_node("a", &json!({}), &config);
    assert_eq!(next, Some("__end__".to_string()));
}
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-17 | 0.2 | Implementation complete - routing with cycle detection | James (Dev) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
None - implementation completed without blocking issues

### Completion Notes List
- Created `routing.rs` module with comprehensive edge routing logic
- Implemented `GotoConfig` enum already in `config.rs` from Story 1
- Implemented `evaluate_condition()` using Tera template engine
- Implemented `resolve_next_node()` with priority: inline goto > edges > sequential
- Implemented `find_entry_node()` for `__start__` edge handling
- Implemented `ExecutionContext` for cycle detection and iteration limits
- Implemented `detect_cycles()` for compile-time cycle detection
- Implemented `build_execution_path()` for execution planning
- Created 18 unit tests covering all routing scenarios
- Created 10 integration tests for real-world workflows
- Default iteration limit: 1000 (configurable via ExecutionContext)

### File List
| File | Action | Description |
|------|--------|-------------|
| `rust/tea-wasm-llm/src/routing.rs` | Created | Edge routing logic |
| `rust/tea-wasm-llm/src/lib.rs` | Modified | Added routing module and exports |
| `rust/tea-wasm-llm/tests/test_routing.rs` | Created | Routing integration tests |

## QA Results

### QA Review Date
2026-01-17

### Reviewer
Quinn (Test Architect)

### Test Design Summary
| Metric | Count |
|--------|-------|
| Total Scenarios | 15 |
| Unit Tests | 11 |
| Integration Tests | 3 |
| E2E Tests | 1 |
| P0 (Critical) | 7 |
| P1 (Important) | 7 |
| P2 (Edge cases) | 1 |

### Risk Assessment
| Risk ID | Score | Description |
|---------|-------|-------------|
| TECH-003 | 6 (High) | Condition evaluation infinite loops |
| TECH-006 | 4 (Medium) | Edge case routing ambiguity |

### Key Test Scenarios
- `1.3-UNIT-001`: Parse simple `goto: nodeB` (P0)
- `1.3-UNIT-004`: Condition `state.score > 0.8` evaluates true (P0)
- `1.3-UNIT-007`: `__end__` terminates execution (P0)
- `1.3-UNIT-011`: Iteration limit (1000) prevents infinite loop (P0, TECH-003)
- `1.3-INT-002`: Conditional goto branches correctly (P0)

### Recommendations
1. Ensure iteration limit is mandatory, not optional
2. Add cycle detection at graph compilation time, not just runtime
3. Document edge evaluation order clearly

### Gate Status
**PASS** - Story is ready for implementation. Risk TECH-003 mitigated by iteration limit requirement (AC 8).

### Reference
Test design: `docs/qa/assessments/TEA-WASM-001-test-design-20260117.md`
