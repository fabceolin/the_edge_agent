# Story TEA-WASM-001.3: Conditional Edge Routing

## Status
Draft

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

- [ ] Extend edge config parsing (AC: 1, 2, 3)
  - [ ] Parse simple `goto: nodeName` string
  - [ ] Parse `goto` object with `to` and `when` fields
  - [ ] Parse array of conditional gotos
  - [ ] Handle inline `goto:` in node config

- [ ] Define GotoConfig enum (AC: 1, 2, 3)
  - [ ] `Simple(String)` - unconditional goto
  - [ ] `Conditional { to: String, when: String }` - single condition
  - [ ] `Array(Vec<ConditionalGoto>)` - multiple conditions

- [ ] Implement condition evaluation (AC: 4, 7)
  - [ ] Create `evaluate_condition(condition: &str, state: &JsonValue) -> bool`
  - [ ] Use Tera to evaluate condition as expression
  - [ ] Handle truthy/falsy values (null, 0, "", false = falsy)

- [ ] Implement edge resolution (AC: 5, 6, 7)
  - [ ] Create `resolve_next_node(current: &str, state: &JsonValue, config: &WasmYamlConfig) -> Option<String>`
  - [ ] Check node's `goto` first
  - [ ] Check explicit edges from current node
  - [ ] Fall back to next sequential node
  - [ ] Return `None` for `__end__` or no next node

- [ ] Handle special nodes (AC: 5)
  - [ ] `__start__` - virtual entry point
  - [ ] `__end__` - virtual exit point
  - [ ] Find first node when `__start__` edge exists

- [ ] Implement cycle detection (AC: 8)
  - [ ] Track visited nodes during execution
  - [ ] Warn on revisit (allow for loops, but warn)
  - [ ] Hard limit on iterations (e.g., 1000)

- [ ] Update executor to use edge routing
  - [ ] Replace simple sequential iteration
  - [ ] Call `resolve_next_node()` after each node
  - [ ] Stop when `__end__` or `None` returned

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
